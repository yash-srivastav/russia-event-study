setwd("/Users/yashsrivastav/Dropbox (Personal)/Personal_Projects/Russia")
library(readxl); library(dplyr); library(tidyr); library(readr); library(stringr)
source("code/firm_cleaning.R")

## Loading in data ------------------------------------------------------------
# Firm status in Russia
russia_status <- read_excel("data/raw/companies_russia.xlsx") |>
  mutate(`Status Classification` = case_when(Status == 1 ~ "Stay",
                   Status == 2 ~ "Wait",
                   Status == 3 ~ "Leave",
                   Status == 4 ~ "Exited"),
         `Sub-Status Classification` = case_when(`Sub-Status` == 1 ~ "Continue operations",
                                                 `Sub-Status` == 2 ~ "Pausing investments",
                                                 `Sub-Status` == 3 ~ "Scaling back",
                                                 `Sub-Status` == 4 ~ "Suspension",
                                                 `Sub-Status` == 5 ~ "Exit completed"))
russia_status <- russia_status |>
  mutate(firm_clean = cleanFirm(`Firm Name`))

# Returns data
returns <- read.csv("data/raw/crsp_returns.csv") |>
  mutate(RET = as.numeric(RET)) |>
  mutate(COMNAM = ifelse(str_detect(COMNAM, "SALESFORCE"), "SALESFORCE INC", COMNAM))

firm_list <- returns |>
  distinct(COMNAM, TICKER) |>
  mutate(firm_clean = cleanFirm(COMNAM)) 

## Matching firms -------------------------------------------------------------
mrg_firms <- firm_list |>
  inner_join(russia_status,
             by = "firm_clean") |>
  distinct(COMNAM, .keep_all = TRUE) |>
  arrange(Status)
write_rds(mrg_firms, "data/interim/sample_firms.rds")

## Creating sample window -----------------------------------------------------
# Sample window (7/1/2021 - 1/31/2022)
sample_window <- returns |>
  select(date, TICKER, COMNAM, RET, ewretd) |>
  mutate(date = as.character(date),
         date = as.Date(date, format = "%Y%m%d")) |>
  filter(between(date,
                 as.Date("2021-07-01"),
                 as.Date("2022-01-31")))

write_rds(sample_window, "data/interim/sample_window.rds")

## Creating event window ------------------------------------------------------
# Event window (2/1/2022 - 12/31/2022)
event_window <- returns |>
  select(date, TICKER, COMNAM, RET, ewretd) |>
  mutate(date = as.character(date),
         date = as.Date(date, format = "%Y%m%d")) |>
  filter(between(date,
                 as.Date("2022-02-01"),
                 as.Date("2022-12-31")))

write_rds(event_window, "data/interim/event_window.rds")
