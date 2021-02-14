# Dependencies
library(tidyverse)
library(RBtest)
library(haven)

# Import
redcap <- read_csv("data/raw/redcap_cleaned.csv")

# MISSING DATA? -----------------------------------------------------------

# Are data missing completely at random (MCAR)?
redcap %>%
  select(starts_with(c("gims", "mhi"))) %>%
  RBtest()

# Select just the scales
redcap_scales <- redcap %>%
  select(record_id, starts_with(c("gims", "mhi")))

# Replace missing values
for (i in 2:length(redcap_scales)) {
  
  redcap_scales[i] <- ifelse(is.na(redcap_scales[i][[1]]), 
                             mean(redcap_scales[i][[1]], na.rm = TRUE), 
                             redcap_scales[i][[1]])
}

# Missing values remaining?
redcap_scales %>%
  gather(key = "scale", value = "score", -record_id) %>%
  filter(is.na(score)) %>%
  nrow()

# Combine data again
redcap_1 <- redcap %>%
  select(-starts_with(c("gims", "mhi"))) %>%
  left_join(redcap_scales, by = "record_id")

# CHECK DATASET -----------------------------------------------------------

# How many observations?
nrow(redcap)

# Correct variables in the data set?
redcap

# Any missing data generally?
gather(redcap, key = "variables", value = "values", -record_id) %>%
  filter(is.na(values))

# Any missing scale data?
redcap %>%
  select(record_id, starts_with("gims")) %>%
  gather(key = "item", value = "value", -record_id) %>%
  filter(is.na(value))

# Original gender variables
unique(redcap$gender)

# Transgender collapse into one variable
redcap %>%
  mutate(
    gender = recode(gender, "trans_woman" = "trans", "trans_man" = "trans")
  ) %>% 
  count(gender)

# WRITE TO FILE -----------------------------------------------------------

write_csv(redcap_1, file = "data/raw/gims_cleaned.csv")
write_sav(redcap_1, path = "data/raw/gims_cleaned.sav")
