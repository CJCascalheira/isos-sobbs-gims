# Dependencies
library(tidyverse)
library(rio)
library(psych)

# Import
redcap <- import("data/sex_obj_cleaned_Outliers_string_recoded.sav") %>%
  as_tibble()

# Describe items
redcap %>%
  select(starts_with("sobbs")) %>%
  describe()

# Correlation matrix
interitem_cor <- redcap %>%
  select(starts_with("sobbs")) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble()
interitem_cor

# DO NOT USE APA TABLES PACKAGE---RESULTED IN INCORRECT INTERITEM CORRELATIONS BEING REPORTED
write_csv(interitem_cor, "data/results/interitem_cor.csv")

# SCALES AND SUBSCALES ----------------------------------------------------

# SOBBS Total Score - mean
sobbs_total <- redcap %>%
  select(record_id, starts_with("sobbs")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_sobbs = mean(score)
  )

# Observer's Perspective - mean
subscale_op <- redcap %>%
  select(record_id, starts_with("sobbs")) %>%
  select(record_id, sobbs_2, sobbs_4, sobbs_6, sobbs_7, sobbs_8, sobbs_12, sobbs_14) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    subscale_op = mean(score)
  )

# Body as Self - mean
subscale_body <- redcap %>%
  select(record_id, starts_with("sobbs")) %>%
  select(record_id, sobbs_1, sobbs_3, sobbs_5, sobbs_9, sobbs_10, sobbs_11, sobbs_13) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    subscale_body = mean(score)
  )

# Join
sobbs_scales <- left_join(sobbs_total, subscale_op) %>%
  left_join(subscale_body)

# Describe
describe(sobbs_scales)

# Correlation matrix
sobbs_scales %>%
  select(-record_id) %>%
  cor()
