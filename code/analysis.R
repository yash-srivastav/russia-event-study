setwd("/Users/yashsrivastav/Dropbox (Personal)/Personal_Projects/Russia")
library(readxl); library(dplyr); library(tidyr); library(readr);
library(broom); library(nlme); library(lubridate); library(fixest)

source("code/prep_functions.R")

## Calculating firm betas ------------------------------------------------------
sample_window <- read_rds("data/interim/sample_window.rds")

mdl <- lmList(RET ~ ewretd | COMNAM,
              data = sample_window,
              na.action = na.exclude)

# Extract coefficients
betas <- coef(mdl) |>
  rename(alpha = `(Intercept)`,
         mkt_beta = ewretd) 
betas <- betas |>
  mutate(COMNAM = row.names(betas)) |>
  relocate(COMNAM, .before = alpha)
rm(mdl)

## Calculating standard deviation over estimation window ----------------------
## (Hagnas & Pynnonen 2014) 
sample_window <- sample_window |>
  left_join(betas,
            by = "COMNAM") |>
  mutate(aret = RET - alpha - mkt_beta*ewretd)
sample_sd <- sample_window |>
  mutate(event_dur = as.Date("2022-01-31") - as.Date("2021-07-01"),
         event_dur = as.numeric(event_dur)) |>
  group_by(COMNAM) |>
  summarise(sd_ar = sqrt((1/event_dur)*sum(aret^2, na.rm = TRUE))) |>
  distinct() |>
  filter(COMNAM != "")


## Merge alpha and beta coefficients with event window data --------------------
event_window <- read_rds("data/interim/event_window.rds")
event_window <- event_window |>
  left_join(betas,
            by = "COMNAM") |>
  distinct()

## Calculating abnormal returns ------------------------------------------------
event_window <- event_window |>
  mutate(ab_ret = RET - alpha - mkt_beta*ewretd)

## Merging in firm position on Russia ------------------------------------------
sample_firms <- read_rds("data/interim/sample_firms.rds")

event_window <- event_window |>
  left_join(sample_firms |>
              select(COMNAM, `Announcement Date`, Status, `Sub-Status`,
                     `Status Classification`, `Sub-Status Classification`),
            by = "COMNAM")

## Looking at CARs for event window of t --------------------------------------
ew1 <- ewprep(event_window, 1)

# Dataframe of CARs
car1 <- build_car(ew1) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(scar = car/sd_ar)

# Testing whether E[AR] = 0 (Campbell, Lo, McKinlay 1997)
j1 <- mean(car1$car)/sqrt((1/(nrow(car1))^2)*sum(car1$sd_ar^2))
j2 <- sqrt((nrow(car1)*(216-4))/(216-2))*mean(car1$scar)

# Calculating variance (KPP 2018)
mean(car1$car)/sqrt(kpp_var(ew1,car1))
kpp_var(ew1, car1)

# Plotting CARs
ggplot(data = car1, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")

ggplot(data = car1, aes(x = scar)) +
  geom_histogram(color = "black", fill = "white")

## Looking at CARs for event window of t, t+1 ---------------------------------
ew2 <- ewprep(event_window, 2)

# Dataframe of CARs
car2 <- build_car(ew2) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(scar = car/sd_ar)

# Testing whether E[AR] = 0 (Campbell, Lo, McKinlay 1997)
#J1-stat
mean(car2$car)/sqrt((1/(nrow(car2))^2)*sum(car2$sd_ar^2))
#J2-stat
sqrt((nrow(car2)*(216-4))/(216-2))*mean(car2$scar)

# Calculating variance (KPP 2018), and corresponding t-stat
mean(car2$car)/sqrt(kpp_var(ew2,car2))
kpp_var(ew2, car2)

# Plotting CARs
ggplot(data = car2, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")

## Looking at CARs for event window of t, t+1, t+2, t+3 ------------------------
ew4 <- ewprep(event_window, 4)

# Dataframe of CARs
car4 <- build_car(ew4) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(scar = car/sd_ar)

# Plotting CARs
ggplot(data = car4, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")

ggplot(data = car4, aes(x = scar)) +
  geom_histogram(color = "black", fill = "white")

# Testing whether E[AR] = 0 (Campbell, Lo, McKinlay 1997)
#J1-stat
mean(car4$car)/sqrt((1/(nrow(car4))^2)*sum(car4$sd_ar^2))
#J2-stat
sqrt((nrow(car4)*(216-4))/(216-2))*mean(car4$scar)

# Calculating variance (KPP 2018), and corresponding t-stat
mean(car4$car)/sqrt(kpp_var(ew4,car4))
kpp_var(ew4, car4)

# T-test
t.test(car_4$car, alternative = "two.sided")
t.test(car4$scar, alternative = "two.sided")
