# Dependencies
library(rio)
library(tidyverse)

# Import
sex_obj <- import("data/sex_obj_cleaned_Outliers_string_recoded.sav") %>%
  as_tibble()
redcap_cleaned <- read_csv("data/raw/sex_obj_cleaned.csv")

# Check importing
nrow(sex_obj)

# RECODE ------------------------------------------------------------------

# Select cleaned demographic variables
sex_obj_1 <- redcap_cleaned %>%
  filter(record_id %in% sex_obj$record_id) %>%
  mutate(gender = recode(gender, "trans_man" = "transgender", "trans_woman" = "transgender"))

# DEMOGRAPHICS: NARRATIVE -------------------------------------------------

# Gender
sex_obj_1 %>%
  count(gender) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Age
sex_obj_1 %>%
  summarize(
    M = mean(age),
    SD = sd(age)
  )

# Hormone Status
sex_obj_1 %>%
  group_by(gender) %>%
  count(hormone) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Bottom Surgery Status
sex_obj_1 %>%
  group_by(gender) %>%
  count(bottom_sex) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Top Surgery Status
sex_obj_1 %>%
  group_by(gender) %>%
  count(top_sex) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# DEMOGRAPHICS: TABLE -----------------------------------------------------

# SAAB
sex_obj_1 %>%
  group_by(gender) %>%
  count(saab) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Sexual Orientation
sex_obj_1 %>%
  group_by(gender) %>%
  count(sex_orient) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Racial identity
sex_obj_1 %>%
  group_by(gender) %>%
  count(ethnic) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Education
sex_obj_1 %>%
  group_by(gender) %>%
  count(education) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Income
sex_obj_1 %>%
  group_by(gender) %>%
  count(income) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))

# Region
sex_obj_1 %>%
  group_by(gender) %>%
  count(region) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(gender, desc(n))
