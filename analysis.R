library(tidyverse)
library(readxl)
library(predictrace)
library(brms)

raw_files <- list.files('data/raw_xlsx/', full.names = TRUE)

d <- bind_rows(lapply(raw_files, read_xlsx))

# Rename Vars
names(d)
d <- d %>% 
  rename(ID = `Exe ID`,
         first_name = `First name`,
         last_name = `Last name`, 
         title = Title, level = Level, department = Department, 
         boss_ID = `Boss ID`, boss_full_name = `Boss name`, boss_title = `Boss title`, 
         company_ID = `Org ID`, company = `Company`, 
         industry = Industry, 
         country = Country, 
         mother_company = `Mother Company`, mother_company_ID = `Mother Org ID`,
         details = `Executive's Details`)

# Remove Duplicates
d <- distinct(d, ID, .keep_all = TRUE) %>% 
  # boss and employee should not be the same person
  filter(ID != boss_ID)

length(unique(d$ID)) # 27184 unique executives
length(unique(d$first_name)) # 6770 unique first names
length(unique(d$last_name)) # 17000 unique last names
length(unique(d$company_ID)) # 1920 unique companies
length(unique(d$country)) # 78 unique countries

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Variables I want
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Boss First/Last name
d$boss_first_name <- unlist(lapply(d$boss_ID, 
                                   function(x){
                                     if(x %in% d$ID){
                                       d$first_name[d$ID == x][1]
                                     }else{
                                       str_extract(d$boss_full_name[d$boss_ID == x][1], "[:alpha:]+ ")
                                     }}))
d$boss_last_name <- unlist(lapply(d$boss_ID, 
                                  function(x){
                                    if(x %in% d$ID){
                                      d$last_name[d$ID == x][1]
                                    }else{
                                      str_remove(d$boss_full_name[d$boss_ID == x][1], "[:alpha:]+ ")
                                    }}))

# Predicted Gender
predicted_gender <- predict_gender(d$first_name[d$country == "United States (USA)"], probability = FALSE) %>% 
  select(name, likely_gender) %>% 
  rename(first_name = name, gender = likely_gender) %>% 
  mutate(gender = as.integer(gender == "female"))
boss_predicted_gender <- predicted_gender %>% 
  rename(boss_first_name = first_name, 
         boss_gender = gender)
# Predicted Ethnicity (based on first and last name - https://ethnicolr.readthedocs.io/ethnicolr.html#general-api)
names_ethnicity <- read_csv('data/names_ethnicity.csv')
names_ethnicity <- names_ethnicity %>% 
  select(firstName, lastName, race,
         `Asian,GreaterEastAsian,EastAsian_mean`, `Asian,GreaterEastAsian,Japanese_mean`,
         `Asian,IndianSubContinent_mean`, `GreaterAfrican,Africans_mean`, `GreaterAfrican,Muslim_mean`,
         `GreaterEuropean,British_mean`, `GreaterEuropean,EastEuropean_mean`, `GreaterEuropean,Jewish_mean`, 
         `GreaterEuropean,WestEuropean,French_mean`, `GreaterEuropean,WestEuropean,Germanic_mean`, 
         `GreaterEuropean,WestEuropean,Hispanic_mean`, `GreaterEuropean,WestEuropean,Italian_mean`, 
         `GreaterEuropean,WestEuropean,Nordic_mean`) %>% 
  rename(first_name = firstName, last_name = lastName, 
         EastAsian = `Asian,GreaterEastAsian,EastAsian_mean`, Japanese = `Asian,GreaterEastAsian,Japanese_mean`,
         Indian = `Asian,IndianSubContinent_mean`, African = `GreaterAfrican,Africans_mean`, Muslim = `GreaterAfrican,Muslim_mean`,
         British = `GreaterEuropean,British_mean`, EastEuropean = `GreaterEuropean,EastEuropean_mean`, Jewish = `GreaterEuropean,Jewish_mean`, 
         French = `GreaterEuropean,WestEuropean,French_mean`, German = `GreaterEuropean,WestEuropean,Germanic_mean`, 
         Hispanic = `GreaterEuropean,WestEuropean,Hispanic_mean`, Italian = `GreaterEuropean,WestEuropean,Italian_mean`, 
         Nordic = `GreaterEuropean,WestEuropean,Nordic_mean`) %>% 
  mutate(WestEuropean = British + French + German + Nordic,
         race = str_remove_all(race, ".+,"),
         ethnicity = case_when(race %in% c("British", "French", "Germanic", "Nordic") ~ "Western European or African American",
                               race == "EastEuropean" ~ "Eastern European",
                               race == "IndianSubContinent" ~ "Indian",
                               race == "EastAsian" ~ "Chinese or Korean",
                               race == "Africans" ~ "African",
                               .default = race)) %>% 
  select(-c(British,French,German,Nordic, race))

boss_names_ethnicity <- names_ethnicity %>% 
  rename(boss_ethnicity = ethnicity,
         boss_first_name = first_name, boss_last_name = last_name, 
         boss_WestEuropean = WestEuropean, boss_EastAsian = EastAsian, boss_Japanese = Japanese,
         boss_Indian = Indian, boss_African = African, boss_Muslim = Muslim,
         boss_EastEuropean = EastEuropean, boss_Jewish = Jewish, 
         boss_Hispanic = Hispanic, boss_Italian = Italian)

ethnicity_avg_probs <- names_ethnicity %>% 
  group_by(ethnicity) %>% 
  summarise(across(EastAsian:WestEuropean, mean)) %>% 
  drop_na() %>% 
  ungroup()
#~~~~
# Name Popularity (binary)
name_popularity <- tidytuesdayR::tt_load(2022, week = 12)$babynames
name_popularity <- name_popularity %>%
  rename(first_name = name) %>%
  filter(first_name %in% d$first_name[d$country == "United States (USA)"]) %>%
  group_by(first_name) %>%
  summarise(prop = mean(prop)) %>%
  ungroup() %>%
  mutate(common_name = as.integer(prop > .001)) %>%
  select(-prop)

d_america <- d %>% 
  # only US for now
  filter(country == "United States (USA)") %>% 
  # select important vars
  select(ID, first_name, boss_ID, boss_first_name, 
         company_ID, company, level) %>% 
  # remove top level (bosses not employees)
  filter(!is.na(boss_ID))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Balanced Dataset for Modeling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

companies_draw <- sample(unique(d_america$company_ID), 250)

# Temporary resampled training data
d_america_train <- d_america %>% 
  filter(company_ID %in% companies_draw) %>% 
  # code as true
  mutate(real = 1)

# Permuted Boss-Employee Relationships
d_america_false <- d_america %>% 
  # sample only drawn companies
  filter(company_ID %in% companies_draw) %>% 
  # all possible employee-boss combinations
  tidyr::expand(tidyr::nesting(ID, first_name), tidyr::nesting(boss_ID, boss_first_name)) %>% 
  # rejoin true combinations
  left_join(d %>% select(ID, boss_ID, company_ID), relationship = "many-to-many") %>% 
  # remove true combinations
  filter(is.na(company_ID)) %>% select(-company_ID) %>% 
  # rejoin interesting vars
  left_join(d %>% group_by(boss_ID) %>% reframe(company_ID, company, level), by = "boss_ID", relationship = "many-to-many") %>% 
  # sample only as many rows as are in real data
  slice_sample(n = nrow(d_america_train)) %>% 
  # code as false
  mutate(real = 0)

d_america_train <- d_america_train %>% 
  # bind false data
  bind_rows(d_america_false) %>% 
  # calculate dyadic variables
  mutate(same_first_name = as.integer(first_name == boss_first_name))

# Fold in Gender
d_america_train <- d_america_train %>% 
  left_join(predicted_gender, multiple = "any") %>% 
  left_join(boss_predicted_gender, multiple = "any") %>% 
  mutate(both_male = as.integer(boss_gender == 0 & gender == 0),
         boss_male = as.integer(boss_gender == 0 & gender == 1),
         boss_female = as.integer(boss_gender == 1 & gender == 0),
         both_female = as.integer(boss_gender == 1 & gender == 1))

# Fold in Ethnicity
d_america_train <- d_america_train %>% 
  left_join(names_ethnicity, multiple = "any") %>% 
  left_join(boss_names_ethnicity, multiple = "any") %>% 
  # Same Ethnicity as Boss
  mutate(both_EastAsian = EastAsian*boss_EastAsian,
         both_EastEuropean = EastEuropean*boss_EastEuropean,
         both_Japanese = Japanese*boss_Japanese,
         both_Indian = Indian*boss_Indian,
         both_African = African*boss_African,
         both_Muslim = Muslim*boss_Muslim,
         both_WestEuropean = WestEuropean*boss_WestEuropean,
         both_Jewish = Jewish*boss_Jewish,
         both_Hispanic = Hispanic*boss_Hispanic,
         both_Italian = Italian*boss_Italian,
         WE_Asian = boss_WestEuropean*(EastAsian + Japanese),
         WE_Indian = boss_WestEuropean*Indian,
         WE_Muslim = boss_WestEuropean*Muslim,
         WE_Hispanic = boss_WestEuropean*Hispanic,
         WE_Jewish = boss_WestEuropean*Jewish) %>% 
  # Center continuous variables
  mutate(across(both_EastAsian:WE_Jewish, function(x){x - mean(x, na.rm = TRUE)}))

# Fold in Name Popularity
d_america_train <- d_america_train %>% 
  left_join(name_popularity, multiple = "any") %>%
  left_join(name_popularity %>% rename(boss_first_name = first_name,
                                       boss_common_name = common_name), multiple = "any") %>%
  # Unrecognized names are for sure uncommon
  replace_na(list(common_name = 0, boss_common_name = 0)) %>%
  mutate(both_uncommon_names = as.integer(boss_common_name == 0 & common_name == 0))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modeling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Re-run "Balanced Dataset for Modeling" for resampled false cases
# both_EastAsian + both_Japanese + both_Indian + both_African + both_Muslim + both_WestEuropean + both_Jewish + both_Hispanic + both_Italian +
# WE_Asian + WE_Indian + WE_Muslim + WE_Hispanic + WE_Jewish
mod_america_bayes10 <- brm(real ~ 0 + same_first_name + both_uncommon_names + both_male + boss_male + boss_female + both_female + both_EastAsian + both_EastEuropean + both_Japanese + both_Indian + both_African + both_Muslim + both_WestEuropean + both_Jewish + both_Hispanic + both_Italian + WE_Asian + WE_Indian + WE_Muslim + WE_Hispanic + WE_Jewish + (0 + same_first_name | boss_ID) + (0 + same_first_name + both_uncommon_names + both_male + boss_male + boss_female + both_female + both_EastAsian + both_EastEuropean + both_Japanese + both_Indian + both_African + both_Muslim + both_WestEuropean + both_Jewish + both_Hispanic + both_Italian + WE_Asian + WE_Indian + WE_Muslim + WE_Hispanic + WE_Jewish | company_ID),
                           data = d_america_train,
                           family = bernoulli(),
                           prior = c(
                             prior(normal(0, 1), class = "b"),
                             prior(student_t(4, 0, 1), class = "sd")
                           ),
                           iter = 8000,
                           warmup = 2000,
                           cores = 4)

save(mod_america_bayes1, mod_america_bayes2, mod_america_bayes3, mod_america_bayes4, mod_america_bayes5, mod_america_bayes6, mod_america_bayes7, mod_america_bayes8, mod_america_bayes9, mod_america_bayes10, file = "data/results.RData")

# Combine models trained on resampled false cases
# First 21 vars for fixed effects only. First 253 for sd and cor.
full_vars <- head(variables(mod_america_bayes1), 21)

mod_america_posterior_combined <- bind_rows( lapply( c(
  as_draws(mod_america_bayes1, variable = full_vars),
  as_draws(mod_america_bayes2, variable = full_vars),
  as_draws(mod_america_bayes3, variable = full_vars),
  as_draws(mod_america_bayes4, variable = full_vars),
  as_draws(mod_america_bayes5, variable = full_vars),
  as_draws(mod_america_bayes6, variable = full_vars),
  as_draws(mod_america_bayes7, variable = full_vars),
  as_draws(mod_america_bayes8, variable = full_vars),
  as_draws(mod_america_bayes9, variable = full_vars),
  as_draws(mod_america_bayes10, variable = full_vars)
), as_tibble ) )

mod_america_posterior_medians <- mod_america_posterior_combined %>% 
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>% as.list()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Posterior Distributions
library(ggdist)

# Posterior Distributions
mod_america_posterior_combined %>% 
  select(b_same_first_name:b_both_female) %>% 
  pivot_longer(everything(), names_to = "param", names_prefix = "b_", values_to = "draw") %>% 
  ggplot() +
  stat_halfeye(aes(draw, param), fill = "skyblue", color = "blue4") +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  theme_bw() +
  labs(x = "Posterior Parameter Estimate", y = "")

ggsave("~/Projects/rimonim.github.io/blog/hire_your_clone/posteriors1.png",
       width = 5, height = 6)

mod_america_posterior_combined %>% 
  select(b_both_EastAsian:b_WE_Jewish) %>% 
  pivot_longer(everything(), names_to = "param", names_prefix = "b_", values_to = "draw") %>% 
  ggplot() +
  stat_halfeye(aes(draw, param), fill = "skyblue", color = "blue4") +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  theme_bw() +
  labs(x = "Posterior Parameter Estimate", y = "")

ggsave("~/Projects/rimonim.github.io/blog/hire_your_clone/posteriors2.png",
       width = 5, height = 6)

d_america_allvars <- d_america %>%
  mutate(same_first_name = as.integer(first_name == boss_first_name)) %>%
  left_join(predicted_gender, multiple = "any") %>%
  left_join(boss_predicted_gender, multiple = "any") %>%
  mutate(dyad_gender = case_when(boss_gender == 0 & gender == 0 ~ 'Both Men',
                                 boss_gender == 0 & gender == 1 ~ 'Boss Man',
                                 boss_gender == 1 & gender == 0 ~ 'Boss Woman',
                                 boss_gender == 1 & gender == 1 ~ 'Both Women')) %>%
  left_join(names_ethnicity, multiple = "any") %>% 
  left_join(boss_names_ethnicity, multiple = "any") %>% 
  # Same Race as Boss
  mutate(both_EastAsian = EastAsian*boss_EastAsian,
         both_EastEuropean = EastEuropean*boss_EastEuropean,
         both_Japanese = Japanese*boss_Japanese,
         both_Indian = Indian*boss_Indian,
         both_African = African*boss_African,
         both_Muslim = Muslim*boss_Muslim,
         both_WestEuropean = WestEuropean*boss_WestEuropean,
         both_Jewish = Jewish*boss_Jewish,
         both_Hispanic = Hispanic*boss_Hispanic,
         both_Italian = Italian*boss_Italian,
         WE_Asian = boss_WestEuropean*(EastAsian + Japanese),
         WE_Indian = boss_WestEuropean*Indian,
         WE_Muslim = boss_WestEuropean*Muslim,
         WE_Hispanic = boss_WestEuropean*Hispanic,
         WE_Jewish = boss_WestEuropean*Jewish) %>% 
  left_join(name_popularity, multiple = "any") %>%
  left_join(name_popularity %>% rename(boss_first_name = first_name,
                                       boss_common_name = common_name), multiple = "any") %>%
  # Unrecognized names are for sure uncommon
  replace_na(list(common_name = 0, boss_common_name = 0)) %>%
  mutate(both_uncommon_names = as.integer(boss_common_name == 0 & common_name == 0))

# Mostly imaginary employees for all combinations of vars, each average for their race.
average_employees <- d_america_allvars %>% 
  drop_na() %>% 
  select(first_name, common_name, gender, ethnicity) %>% 
  distinct(first_name, .keep_all = TRUE) %>% 
  group_by(common_name, gender, ethnicity) %>% 
  reframe(across(everything(), ~head(.x, 5))) %>% 
  # join average race values
  left_join(ethnicity_avg_probs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Disclaimers:
# - Results of this analysis do not necessarily reflect biases in hiring. 
# - Since race and gender are inferred from names, and because race if defined very coarsely (only four categories),
#   their true effects are probably somewhat watered down. This may also be a confounding factor
#   in estimating the effect of sharing the same first name: People who share the same first name are
#   likely to share the same ethnicity - a model that can't distinguish ethnicity with fine enough 
#   granularity may therefore OVERestimate the effect of sharing the same first name.
# - Because data on gender and race are inferred from names, missing values of these
#   variables may not be randomly distributed. 