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

# Same Name as Boss
d <- d %>% 
  mutate(same_first_name = first_name == boss_first_name,
         same_last_name = last_name == boss_last_name)

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
# Prevalence of Predicted Nationality within Company?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Balanced Dataset for Modeling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d_america <- d %>% 
  # only US for now
  filter(country == "United States (USA)") %>% 
  # select important vars
  select(ID, first_name, last_name, boss_ID, boss_first_name, boss_last_name, company_ID, company, level) %>% 
  # remove top level (bosses not employees)
  filter(!is.na(boss_ID))

# Permuted Boss-Employee Relationships
d_america_false <- d_america %>% 
  # all possible employee-boss combinations
  tidyr::expand(tidyr::nesting(ID, first_name, last_name), tidyr::nesting(boss_ID, boss_first_name, boss_last_name)) %>% 
  # rejoin true combinations
  left_join(d %>% select(ID, boss_ID, company_ID), relationship = "many-to-many") %>% 
  # remove true combinations
  filter(is.na(company_ID)) %>% select(-company_ID) %>% 
  # rejoin interesting vars
  left_join(d %>% group_by(boss_ID) %>% reframe(company_ID, company, level), by = "boss_ID", relationship = "many-to-many") %>% 
  # draw the same number of false as true
  slice_sample(n = nrow(d_america)) %>% 
  # code as false
  mutate(real = 0)

d_america <- d_america %>% 
  # code as true
  mutate(real = 1) %>% 
  # bind false data
  bind_rows(d_america_false) %>% 
  # calculate dyadic variables
  mutate(same_first_name = as.integer(first_name == boss_first_name),
         same_last_name = as.integer(last_name == boss_last_name))

# Fold in Gender
d_america <- d_america %>% 
  left_join(predicted_gender, multiple = "any") %>% 
  left_join(boss_predicted_gender, multiple = "any") %>% 
  mutate(dyad_gender = case_when(boss_gender == 0 & gender == 0 ~ "both male",
                                 boss_gender == 0 & gender == 1 ~ "boss male",
                                 boss_gender == 1 & gender == 0 ~ "boss female",
                                 boss_gender == 1 & gender == 1 ~ "both female"))

# Fold in Race
d_america <- d_america %>% 
  left_join(predicted_race, multiple = "any") %>% 
  left_join(boss_predicted_race, multiple = "any") %>% 
  replace_na(list(race = "other", boss_race = "other")) %>% 
  # Same Race as Boss
  mutate(race = factor(race),
         boss_race = factor(boss_race),
         same_race = as.integer(race == boss_race))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modeling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Re-run "Balanced Dataset for Modeling" for resampled false cases

mod_america_bayes2 <- brm(real ~ 0 + same_first_name + dyad_gender + race:same_race + (0 + same_first_name + dyad_gender + race:same_race | company_ID),
                         data = d_america,
                         family = bernoulli(),
                         prior = c(
                           prior(normal(0, 1), class = "b")
                         ),
                         iter = 10000,
                         warmup = 2000,
                         cores = 4)

# Combine models trained on resampled false cases
mod_america_bayes_combined <- combine_models(mod_america_bayes1, mod_america_bayes2, check_data = FALSE)
summary(mod_america_bayes1)
as_draws(mod_america_bayes1, 
         variable = c("b_same_first_name", "b_dyad_genderbossfemale", "b_dyad_genderbossmale", 
                      "b_dyad_genderbothfemale", "b_dyad_genderbothmale", "b_raceasian:same_race", 
                      "b_raceblack:same_race", "b_racehispanic:same_race", "b_raceother:same_race", 
                      "b_racewhite:same_race"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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


# Disclaimers:
# - because data on gender and race are inferred from names, missing values of these
#   variables may not be randomly distributed. 

# same_first_name            0.58      0.33     0.02     1.32 1.00    13925    14508
# dyad_genderbossfemale     -0.27      0.04    -0.36    -0.18 1.00    24994    25609
# dyad_genderbossmale       -0.19      0.04    -0.27    -0.11 1.00    23940    25197
# dyad_genderbothfemale      0.12      0.05     0.03     0.22 1.00    27416    24527
# dyad_genderbothmale       -0.04      0.04    -0.11     0.03 1.00    21303    24105
# raceasian:same_race        0.96      0.34     0.32     1.68 1.00    35770    20771
# raceblack:same_race        0.55      0.94    -1.31     2.36 1.00    48431    19357
# racehispanic:same_race     1.37      0.47     0.49     2.35 1.00    32261    21930
# raceother:same_race        0.25      0.75    -1.30     1.73 1.00    36608    22823
# racewhite:same_race        0.12      0.04     0.05     0.18 1.00    18661    22630
