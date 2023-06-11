library(tidyverse)
library(readxl)
library(predictrace) # Predict race and gender from names - based on U.S. Census data
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
d <- distinct(d, ID, .keep_all = TRUE)

length(unique(d$ID)) # 27184 unique executives
length(unique(d$first_name)) # 6770 unique first names
length(unique(d$last_name)) # 17000 unique last names
length(unique(d$company_ID)) # 1920 unique companies
length(unique(d$country)) # 78 unique countries

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Variables I want
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Boss First/Last name
d$boss_first_name <- unlist(lapply(d$boss_ID, function(x){d$first_name[d$ID == x][1]}))
d$boss_last_name <- unlist(lapply(d$boss_ID, function(x){d$last_name[d$ID == x][1]}))

# Predicted Gender
  predicted_gender <- predict_gender(d$first_name[d$country == "United States (USA)"], probability = FALSE) %>% 
    select(name, likely_gender) %>% 
    rename(first_name = name, gender = likely_gender) %>% 
    mutate(gender = as.integer(gender == "female"))
  boss_predicted_gender <- predicted_gender %>% 
    rename(boss_first_name = first_name, 
           boss_gender = gender)
# Predicted Race (based on first and last name)
  predicted_race <- d %>% filter(country == "United States (USA)") %>% select(first_name, last_name) %>% 
    add_column(
      predict_race(d$last_name[d$country == "United States (USA)"], surname = TRUE) %>% 
        select(probability_american_indian:probability_2races) %>% 
        rename_with(~paste0(.x, "Xprob_last_name"))
      ) %>% 
    add_column(
      predict_race(d$first_name[d$country == "United States (USA)"], surname = FALSE) %>% 
        select(probability_american_indian:probability_2races) %>% 
        rename_with(~paste0(.x, "Xprob_first_name"))
    ) %>% 
    pivot_longer(probability_american_indianXprob_last_name:probability_2racesXprob_first_name,
             names_to = c("race", ".value"), names_prefix = "probability_", names_sep = "X") %>% 
    replace_na(list(prob_last_name = 0, prob_first_name = 0)) %>% 
    mutate(prob = prob_last_name + prob_first_name) %>% 
    group_by(first_name, last_name) %>% 
    mutate(groupsum = sum(prob)) %>% 
    filter(groupsum != 0) %>% 
    mutate(ismax = prob == max(prob)) %>% 
    filter(ismax) %>% 
    select(first_name, last_name, race)
  boss_predicted_race <- predicted_race %>% 
    rename(boss_first_name = first_name, 
           boss_last_name = last_name,
           boss_race = race)
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
  # all possible employee-boss combinations
  tidyr::expand(tidyr::nesting(ID, first_name), tidyr::nesting(boss_ID, boss_first_name)) %>% 
  # rejoin true combinations
  left_join(d %>% select(ID, boss_ID, company_ID), relationship = "many-to-many") %>% 
  # remove true combinations
  filter(is.na(company_ID)) %>% select(-company_ID) %>% 
  # rejoin interesting vars
  left_join(d %>% group_by(boss_ID) %>% reframe(company_ID, company, level), by = "boss_ID", relationship = "many-to-many") %>% 
  # sample only drawn companies
  filter(company_ID %in% companies_draw) %>% 
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

# Fold in Race
d_america_train <- d_america_train %>% 
  left_join(predicted_race, multiple = "any") %>% 
  left_join(boss_predicted_race, multiple = "any") %>% 
  replace_na(list(race = "other", boss_race = "other")) %>% 
  # Same Race as Boss
  mutate(both_asian = as.integer(boss_race == "asian" & race == "asian"),
         both_black = as.integer(boss_race == "black" & race == "black"),
         both_hispanic = as.integer(boss_race == "hispanic" & race == "hispanic"),
         both_white = as.integer(boss_race == "white" & race == "white"),
         boss_white = as.integer(boss_race == "white" & race != "white"))

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
mod_america_bayes5 <- brm(real ~ 0 + same_first_name + both_uncommon_names + both_male + boss_male + boss_female + both_female + both_asian + both_black + both_hispanic + both_white + boss_white + (0 + same_first_name + both_uncommon_names + both_male + boss_male + boss_female + both_female + both_asian + both_black + both_hispanic + both_white + boss_white | company_ID),
                         data = d_america_train,
                         family = bernoulli(),
                         prior = c(
                           prior(normal(0, 1), class = "b"),
                           prior(student_t(4, 0, 1), class = "sd")
                         ),
                         iter = 8000,
                         warmup = 2000,
                         cores = 4)

# Combine models trained on resampled false cases
full_vars <- head(variables(mod_america_bayes0), 11)

mod_america_posterior_combined <- bind_rows( lapply( c(
  as_draws(mod_america_bayes0, variable = full_vars),
  as_draws(mod_america_bayes1, variable = full_vars),
  as_draws(mod_america_bayes2, variable = full_vars),
  as_draws(mod_america_bayes3, variable = full_vars),
  as_draws(mod_america_bayes4, variable = full_vars),
  as_draws(mod_america_bayes5, variable = full_vars)
  ), as_tibble ) )

mod_america_posterior_medians <- mod_america_posterior_combined %>% 
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>% as.list()

summary(mod_america_bayes1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Posterior Distributions
mod_america_posterior_combined %>% 
  ggplot(aes(b_boss_white)) +
    geom_density(fill = "skyblue") +
    geom_vline(aes(xintercept = median(b_boss_white)), color = "red") +
    theme_minimal()

d_america_allvars <- d_america %>%
  mutate(same_first_name = as.integer(first_name == boss_first_name)) %>%
  left_join(predicted_gender, multiple = "any") %>%
  left_join(boss_predicted_gender, multiple = "any") %>%
  mutate(dyad_gender = case_when(boss_gender == 0 & gender == 0 ~ 'Both Men',
                                 boss_gender == 0 & gender == 1 ~ 'Boss Man',
                                 boss_gender == 1 & gender == 0 ~ 'Boss Woman',
                                 boss_gender == 1 & gender == 1 ~ 'Both Women')) %>%
  left_join(predicted_race, multiple = "any") %>%
  left_join(boss_predicted_race, multiple = "any") %>%
  replace_na(list(race = "other", boss_race = "other")) %>%
  # Same Race as Boss
  mutate(dyad_race = case_when(boss_race == "asian" & race == "asian" ~ 'Both Asian',
                               boss_race == "black" & race == "black" ~ 'Both Black',
                               boss_race == "hispanic" & race == "hispanic" ~ 'Both Hispanic',
                               boss_race == "white" & race == "white" ~ 'Both White',
                               boss_race == "white" & race != "white" ~ 'Boss White',
                               .default = "Two Minorities")) %>%
  left_join(name_popularity, multiple = "any") %>%
  left_join(name_popularity %>% rename(boss_first_name = first_name,
                                       boss_common_name = common_name), multiple = "any") %>%
  # Unrecognized names are for sure uncommon
  replace_na(list(common_name = 0, boss_common_name = 0)) %>%
  mutate(both_uncommon_names = as.integer(boss_common_name == 0 & common_name == 0))

calculated_odds <- tibble(same_first_name = 0:1,
                          both_uncommon_names = 0:1,
                          both_male = 0:1,
                          boss_male = 0:1,
                          boss_female = 0:1,
                          both_female = 0:1,
                          both_asian = 0:1,
                          both_black = 0:1,
                          both_hispanic = 0:1,
                          both_white = 0:1,
                          boss_white = 0:1)
calculated_odds <- tidyr::expand(calculated_odds, !!!calculated_odds) %>% 
  mutate(logodds = mod_america_posterior_medians$b_same_first_name*same_first_name + mod_america_posterior_medians$b_both_uncommon_names*both_uncommon_names + mod_america_posterior_medians$b_both_male*both_male + mod_america_posterior_medians$b_boss_male*boss_male + mod_america_posterior_medians$b_boss_female*boss_female + mod_america_posterior_medians$b_both_female*both_female + mod_america_posterior_medians$b_both_asian*both_asian + mod_america_posterior_medians$b_both_black*both_black + mod_america_posterior_medians$b_both_hispanic*both_hispanic + mod_america_posterior_medians$b_both_white*both_white + mod_america_posterior_medians$b_boss_white*boss_white,
         odds = exp(logodds)) %>% 
  select(-logodds)

example_employees <- d_america_allvars %>% 
  drop_na() %>% 
  select(first_name, common_name, gender, race) %>% 
  group_by(common_name, gender, race) %>% 
  reframe(across(everything(), ~head(.x, 10)))
  

## Idea: for gender plot, menu to control race, etc.

# Same First Name
d_america %>% 
  filter(same_first_name == 1,
         !is.na(same_race)) %>% 
  group_by(real, same_race) %>% 
  summarise(percent = 100*n()/11704) %>% 
  ggplot(aes(factor(real), percent, fill = factor(same_race, levels = c(1, 0)))) +
    geom_bar(stat = "identity") +
    theme_bw()

# Gender
d_america %>% 
  filter(gender == 1,
         !is.na(boss_gender)) %>% 
  group_by(real, boss_gender) %>% 
  summarise(percent = n()) %>% 
  ggplot(aes(factor(real), percent, fill = factor(boss_gender))) +
  geom_col(position = "fill") +
  theme_bw()

# Race

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# TODO
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Race should include all possible combinations, like gender.

# Name popularity?
# - popularity cutoff and treat like

# Random selection by companies (should make estimation easier)

# Disclaimers:
# - because data on gender and race are inferred from names, missing values of these
#   variables may not be randomly distributed. 
