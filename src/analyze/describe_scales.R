# Dependencies
library(tidyverse)
library(RBtest)
library(haven)
library(Amelia)

# Import
redcap <- read_csv("data/raw/redcap_cleaned.csv")

# Total
nrow(redcap)

# MISSING DATA? -----------------------------------------------------------

# Are data missing completely at random (MCAR)?
redcap %>%
  select(record_id, starts_with(c("sobbs", "isos", "mhi"))) %>%
  RBtest()

# Select just the scales
redcap_scales <- redcap %>%
  select(record_id, starts_with(c("sobbs", "isos", "mhi")))

# Select just the demographics
redcap_demo <- redcap %>%
  select(-starts_with(c("sobbs", "isos", "mhi")))

# For Amelia, see: https://www.opr.princeton.edu/workshops/Downloads/2018Jan_AmeliaPratt.pdf

# Need to replace with expectation maximization
a_out <- amelia(as.data.frame(redcap_scales), m = 5, idvars = "record_id", 
       ords = c("sobbs_1", "sobbs_2", "sobbs_3", "sobbs_4", "sobbs_5", "sobbs_6", 
                "sobbs_7", "sobbs_8", "sobbs_9", "sobbs_10", "sobbs_11", "sobbs_12", 
                "sobbs_13", "sobbs_14", "isos_1", "isos_2", "isos_3", "isos_4", 
                "isos_5", "isos_6", "isos_7", "isos_8", "isos_9", "isos_10", 
                "isos_11", "isos_12", "isos_13", "isos_14", "isos_15", "mhi_1", 
                "mhi_5", "mhi_4", "mhi_3", "mhi_2"),
       emburn = c(50, 100)
)

# Write to file
write.amelia(obj = a_out, file.stem = "sex_obj_em_", format = "csv")

# CHECK DATASET -----------------------------------------------------------

# Import imputed dataset
redcap_amelia <- read_csv("data/raw/amelia_em/sex_obj_em_5.csv") %>%
  select(-X1)

# Combine the datasets
redcap_1 <- left_join(redcap_demo, redcap_amelia)

# Any missing data generally?
gather(redcap_1, key = "variables", value = "values", -record_id) %>%
  filter(is.na(values))

# Any missing scale data?
redcap_1 %>%
  select(record_id, starts_with("sobbs", "isos", "mhi")) %>%
  gather(key = "item", value = "value", -record_id) %>%
  filter(is.na(value))

# Original gender variables
unique(redcap_1$gender)

# Transgender collapse into one variable
redcap_1 %>%
  mutate(
    gender = recode(gender, "trans_woman" = "trans", "trans_man" = "trans")
  ) %>% 
  count(gender)

# WRITE TO FILE -----------------------------------------------------------

write_csv(redcap_1, file = "data/raw/sex_obj_cleaned.csv")
write_sav(redcap_1, path = "data/raw/sex_obj_cleaned.sav")
