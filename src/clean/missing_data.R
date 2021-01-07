# Dependencies
library(tidyverse)

# Import
redcap_raw <- read_csv("data/raw/redcap_raw.csv")

# BASIC COUNT DATA FOR PARTICIPANT SECTION --------------------------------

# Total N = 1002, but three entries removed due to duplicate emails

# Total responses
nrow(redcap_raw)

# Not completed - number
redcap_raw %>%
  filter(complete == 0) %>%
  nrow()

# Not completed - remove
redcap_raw_1 <- redcap_raw %>%
  filter(complete != 0) %>%
  select(-complete)
nrow(redcap_raw_1)

# Not USA - number
redcap_raw_1 %>%
  filter(live_usa == 0) %>%
  nrow()

# Not USA - remove
redcap_raw_2 <- redcap_raw_1 %>%
  filter(live_usa != 0) %>%
  select(-live_usa)
nrow(redcap_raw_2)

# Consent - number
redcap_raw_2 %>%
  filter(consent == 0) %>%
  nrow()

# Consent - remove
redcap_raw_3 <- redcap_raw_2 %>%
  filter(consent != 0) %>%
  select(-consent)
nrow(redcap_raw_3)

# Age - number
redcap_raw_3 %>%
  filter(age < 18 | is.na(age)) %>%
  nrow()

# Age - remove
redcap_raw_4 <- redcap_raw_3 %>%
  filter(age >= 18)
nrow(redcap_raw_4)

# Check 1 - number
redcap_raw_4 %>%
  filter(check_1 != 1 | is.na(check_1)) %>%
  nrow()

# Check 1 - remove
redcap_raw_5 <- redcap_raw_4 %>%
  filter(check_1 == 1 ) %>%
  select(-check_1)
nrow(redcap_raw_5)

# Check 2 - number
redcap_raw_5 %>%
  filter(check_2 != 1 | is.na(check_2)) %>%
  nrow()

# Check 2 - remove
redcap_raw_6 <- redcap_raw_5 %>%
  filter(check_2 == 1) %>%
  select(-check_2)
nrow(redcap_raw_6)

# Missing sex assigned at birth variable
redcap_raw_6 %>%
  filter(is.na(saab)) %>% 
  nrow()

# Missing SAAB removed
redcap_raw_7 <- redcap_raw_6 %>%
  filter(!is.na(saab))
nrow(redcap_raw_7)

# Total removed
nrow(redcap_raw) - nrow(redcap_raw_7)

# EXPORT DATA -------------------------------------------------------------

# Export
write_csv(redcap_raw_7, path = "data/raw/redcap_complete.csv")
