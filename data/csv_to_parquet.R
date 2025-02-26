library(arrow)
library(tidyverse)
library(ryr)

df <- read_csv_arrow(
  "depression.csv",
  na = c("", "NA", "Missing", "None")
)

df_proc <- df %>%
  mutate(across(starts_with("Rx_day"), as.double)) %>%
  mutate(depression = factor(depression, levels = c("Not Depressed", "Depressed"), labels = c("No", "Yes"))) %>%
  mutate(gender = fct_case_match(gender, 
    0 ~ "Male", 
    1 ~ "Female"
  )) %>%
  mutate(age_class = fct_case_when(
    age >= 18 & age <= 39 ~ "Young adult",
    age >= 40 & age <= 64 ~ "Middle-aged adult",
    age >= 65 ~ "Older adult"
  )) %>%
  mutate(education_level = fct_case_match(education_level, 
    0 ~ NA,
    c(1, 2) ~ "No high school diploma",
    3 ~ "High school graduate",
    c(4, 5) ~ "College graduate or above"
  )) %>%
  mutate(marital_status = fct_case_match(marital_status,
    "Missing" ~ NA,
    "Married" ~ "Married",
    .default = "Other"
  )) %>%
  mutate(BMI = na_if(BMI, 0)) %>%
  mutate(BMI_class = fct_case_when(
    is.na(BMI) ~ NA,
    BMI < 18.5 ~ "Underweight",
    BMI < 25 ~ "Normal",
    BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obese"
  )) %>%
  mutate(household_income = fct_case_match(household_income,
    1 ~ "0 to 4999",
    2 ~ "5000 to 9999",
    3 ~ "10000 to 14999",
    4 ~ "15000 to 19999",
    5 ~ "20000 to 24999",
    6 ~ "25000 to 34999",
    7 ~ "35000 to 44999",
    8 ~ "45000 to 54999",
    9 ~ "55000 to 64999",
    10 ~ "65000 to 74999",
    11 ~ "75000 and Over",
    12 ~ "Over 20000"
  )) %>%
  mutate(across(
    where(~ all(.x %in% c("Yes", "No", NA))), 
    ~ factor(.x, levels = c("No", "Yes"), labels = c("No", "Yes"))
  )) %>%
  mutate(across(
    where(~ all(.x %in% c(0, 1, NA))), 
    ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
  )) %>%
  mutate(across(
    c(weight:height, pulse, systolic:MPV, sleep_hours),
    ~ na_if(.x, 0)
  )) %>%
  mutate(across(where(is.character), as.factor))

write_parquet(df_proc, "depression.parquet")
