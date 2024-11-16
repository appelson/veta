library(tidyverse)
library(janitor)
library(tidycensus)

committments <- read_csv("data/current_commitments.csv") %>% clean_names()
demographics <- read_csv("data/demographics.csv") %>% clean_names() %>%
  mutate(race_cat = ifelse(ethnicity == "White", "white", "non_white"))




demographics_charges <- demographics %>%
  filter(str_detect(controlling_offense, "PC")) %>%
  mutate(Offenses = str_remove(controlling_offense, "PC"),
         Offenses = str_remove(Offenses, " 2nd")) %>%
  left_join(df, by = "Offenses")
  
  
prior_committments <- read_csv("data/prior_commitments.csv") %>% clean_names()



committments_pred <- read_csv("currentcommits_predicted.csv")
demographics_pred <- read_csv("demographics_predicted.csv")
priorcommits_predicted <- read_csv("priorcommits_predicted.csv")



# HOW MANY PEOPLE
demographics %>%
  nrow()


# WHO IS INCARCERATED
demographics %>%
  tabyl(ethnicity) %>%
  arrange(-n)

# WHO IS INCARCERATED RACE
demographics %>%
  tabyl(race_cat)

# WHERE ARE THEY INCARCERATED
demographics %>%
  tabyl(current_location)

# WHAT ARE THEIR CHARGES
demographics_charges %>%
  tabyl(description) %>%
  arrange(-n)

# WHAT ARE THEIR CHARGE TYPES
demographics_charges %>%
  tabyl(Type) %>%
  arrange(-n)

# WHAT ARE THEIR CHARGE TYPES BY ETHNICITY
demographics_charges %>%
  tabyl(Type, race_cat)

# WHAT ARE THE CHARGE TYPES NOT JOINED, ETC
demographics_charges %>%
  filter(is.na(Type)) %>%
  tabyl(Offenses)

# SENTENCE LENGTH
demographics %>%
  tabyl(aggregate_sentence_in_months)







demographics_pred %>%
  tabyl(ethnicity)











committments_cleaned <- committments %>%
  mutate(
    Years = as.numeric(ifelse(grepl("Years", sentence_from_abstract_of_judgement), 
                              gsub(".*?(\\d+) Years.*", "\\1", sentence_from_abstract_of_judgement), 0)),
    Months = as.numeric(ifelse(grepl("Months", sentence_from_abstract_of_judgement), 
                               gsub(".*?(\\d+) Months.*", "\\1", sentence_from_abstract_of_judgement), 0)),
    sentence_months = Years * 12 + Months
  ) %>%
  mutate(
    sentence_months = case_when(
      sentence_from_abstract_of_judgement == "Condemned" ~ NA_integer_, 
      sentence_from_abstract_of_judgement == "Life w/o Parole" ~ NA_integer_,  
      sentence_from_abstract_of_judgement == "Life with Parole" ~ NA_integer_, 
      sentence_from_abstract_of_judgement == "NA" | sentence_from_abstract_of_judgement == "0" ~ NA_integer_, # Replace "0" or "NA" with true NA
      TRUE ~ sentence_months
    ),
    sentence_time_type = case_when(
      !is.na(sentence_months) ~ "Time Period",
      TRUE ~ sentence_from_abstract_of_judgement
    )
  ) %>%
  select(-c(Years,Months)) %>%
    mutate(
     Years = as.numeric(ifelse(grepl("Years", offense_time_with_enhancement), 
                            gsub(".*?(\\d+) Years.*", "\\1", offense_time_with_enhancement), 0)),
      Months = as.numeric(ifelse(grepl("Months", offense_time_with_enhancement), 
                             gsub(".*?(\\d+) Months.*", "\\1", offense_time_with_enhancement), 0)),
     offense_time_months = Years * 12 + Months,
     offense_time_months = ifelse(offense_time_months == 0,NA,offense_time_months)
    ) %>%
  select(-c(Years,Months)) %>%
  mutate(offense_time_type = case_when(
    str_detect(tolower(offense_time_with_enhancement), "to-life") ~ "To Life",
    offense_time_with_enhancement == "Condemned" ~ "Condemned",
    offense_time_with_enhancement == "Life without Possibility of Parole" ~ "Life Without Parole",
    offense_time_with_enhancement == "Stayed" ~ "Stayed",
    TRUE ~ "Not To Life"
  ))


df <- read_csv("selection_criteria.csv")


committments_cleaned %>%
  filter(offense_category == "Drug Crimes") %>%
  tabyl(offense_description)


















convert_to_months <- function(duration) {


# Apply function and print
months <- sapply(durations, convert_to_months)





api_key = "8b0dc67a5d26f4d27b193904ac4ef087b0409b5e"
census_api_key(api_key)
vars_2020 <- load_variables(2020, "pl")
race_vars <- vars_2020 %>%
  filter(concept == "RACE")
v = race_vars$name
census_data <- get_decennial(geography = "county", variables = v, year = "2020", sumfile = "pl", state = "California") %>%
  clean_names() %>%
  rename(sentencing_county = name,
         name = variable) %>%
  left_join(race_vars, by = "name") %>%
  select(sentencing_county, value, label) %>%
  pivot_wider(names_from = "label", values_from = "value", values_fn = sum) %>%
  clean_names() %>%
  mutate(sentencing_county = str_remove(sentencing_county, " County, California"))





