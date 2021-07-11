# Dependencies
library(rio)
library(tidyverse)

# Import
sex_obj <- import("data/sex_obj_cleaned_Outliers_string_recoded.sav") %>%
  as_tibble()

# Check importing
nrow(sex_obj)

# RECODE ------------------------------------------------------------------

# Convert numbers to strings
sex_obj_1 <- sex_obj %>%
  mutate(
    sex_orient = recode(sex_orient, `1` = "asexual", `2` = "bi/pansexual", `3` = "gay", `4` = "heterosexual",
                        `5` = "lesbian", `6` = "other", `7` = "bi/pansexual",
                        `8` = "queer", `9` = "other"),
    region = recode(region, `1` = "East Coast", `2` = "Midwest", `3` = "Rocky Mountains", `4` = "Southwest",
                    `5` = "West Coast", `6` = "Southeast", `7` = "Alaska / Hawaii"),
    income = recode(income, `1` = "less than $20,000", `2` = "$20,000 - $44,999", 
                    `3` = "$45,000 - $139,999", `4` = "more than $140,000", `5` = "more than $140,000", 
                    `6` = "more than $140,000"),
    education = recode(education, `1` = "high school or less", `2` = "high school or less", 
                       `3` = "high school diploma / GED", `4` = "some college", `5` = "associate degree", 
                       `6` = "bachelors degree", `7` = "tradeperson certificate", 
                       `8` = "graduate school", `9` = "graduate school", 
                       `10` = "graduate school"),
    hormone = recode(hormone, `1` = "currently taking", `2` = "took in the past but not currently", 
                     `3` = "plan to take in the future", `4` = "unsure about the future", 
                     `5` = "do not plan to take in the future"),
    top_sex = recode(top_sex, `1` = "have had", `2` = "would like to in the future", 
                     `3` = "unsure about the future", `4` = "do not plan to in the future"),
    bottom_sex = recode(bottom_sex, `1` = "have had", `2` = "would like to in the future", 
                        `3` = "unsure about the future", `4` = "do not plan to in the future")
  )
head(sex_obj_1)

# CALCULATE DEMOGRAPHICS --------------------------------------------------

# Age
sex_obj_1 %>%
  summarize(
    M = mean(age),
    SD = sd(age)
  )

# SAAB
sex_obj_1 %>%
  count(saab) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Sexual Orientation
sex_obj_1 %>%
  count(sex_orient) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Racial identity
sex_obj_1 %>%
  count(ethnic) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Hormone Status
sex_obj_1 %>%
  count(hormone) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Bottom Surgery Status
sex_obj_1 %>%
  count(bottom_sex) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Top Surgery Status
sex_obj_1 %>%
  count(top_sex) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Education
sex_obj_1 %>%
  count(education) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Income
sex_obj_1 %>%
  count(income) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))

# Region
sex_obj_1 %>%
  count(region) %>%
  mutate(perc = (n / nrow(sex_obj_1)) * 100) %>%
  arrange(desc(n))
