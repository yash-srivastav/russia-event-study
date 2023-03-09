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

### Firms that left ---------------------------------------------------
# Event window files
ew1 <- ewprep(event_window, 1)
ew2 <- ewprep(event_window, 2)
ew3 <- ewprep(event_window, 3)
ew4 <- ewprep(event_window, 4)

# write_rds(ew1, "data/interim/event_windows/ew1.rds")
# write_rds(ew2, "data/interim/event_windows/ew3.rds")
# write_rds(ew3, "data/interim/event_windows/ew3.rds")
# write_rds(ew4, "data/interim/event_windows/ew4.rds")

# CAR files
car1 <- build_car(ew1, sample_sd) 
car2 <- build_car(ew2, sample_sd) 
car3 <- build_car(ew3, sample_sd) 
car4 <- build_car(ew4, sample_sd)

# Mean CARs
mean(car2$car)
mean(car2$car)
mean(car3$car)
mean(car4$car)

# Mean SCARs
mean(car1$scar)
mean(car2$scar)
mean(car3$scar)
mean(car4$scar)

## Test stats
# Covariance-variance ratio adjusted st. errors and corresponding t-stat (KPP 2018); preferred
mean(car1$scar)/sqrt(vcov_adj_var(ew1, sample_window, 1))
mean(car2$scar)/sqrt(vcov_adj_var(ew2, sample_window, 2))
mean(car3$scar)/sqrt(vcov_adj_var(ew3, sample_window, 3))
mean(car4$scar)/sqrt(vcov_adj_var(ew4, sample_window, 4))

# J-stats, E[AR] = 0 (Campbell, Lo, McKinlay 1997)
# J1-stat
mean(car1$car)/sqrt((1/(nrow(car1))^2)*sum(car1$sd_ar^2))
mean(car2$car)/sqrt((1/(nrow(car2))^2)*sum(car2$sd_ar^2))
mean(car3$car)/sqrt((1/(nrow(car3))^2)*sum(car3$sd_ar^2))
mean(car4$car)/sqrt((1/(nrow(car4))^2)*sum(car4$sd_ar^2))
# J2-stat
sqrt((nrow(car1)*(216-4))/(216-2))*mean(car1$scar)
sqrt((nrow(car2)*(216-4))/(216-2))*mean(car2$scar)
sqrt((nrow(car3)*(216-4))/(216-2))*mean(car3$scar)
sqrt((nrow(car3)*(216-4))/(216-2))*mean(car3$scar)

## CAR histograms
# Event window == 1
ggplot(data = car1, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
ggplot(data = car1, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
# Event window == 2
ggplot(data = car2, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
ggplot(data = car2, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
# Event window == 3
ggplot(data = car3, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
ggplot(data = car3, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
# Event window == 4
ggplot(data = car4, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")
ggplot(data = car4, aes(x = car)) +
  geom_histogram(color = "black", fill = "white")

## Firms that stayed -----------------------------------------------
# Analysis files
ew1_stay <- ewprep_stay(event_window, 1)
ew2_stay <- ewprep_stay(event_window, 2)
ew3_stay <- ewprep_stay(event_window, 3)
ew4_stay <- ewprep_stay(event_window, 4)

# write_rds(ew1_stay, "data/interim/event_windows/ew1_stay.rds")
# write_rds(ew2_stay, "data/interim/event_windows/ew2_stay.rds")
# write_rds(ew3_stay, "data/interim/event_windows/ew3_stay.rds")
# write_rds(ew4_stay, "data/interim/event_windows/ew4_stay.rds")

# CAR files
car1_stay <- build_car_stay(ew1_stay, sample_sd)
car2_stay <- build_car_stay(ew2_stay, sample_sd)
car3_stay <- build_car_stay(ew3_stay, sample_sd)
car4_stay <- build_car_stay(ew4_stay, sample_sd)

# CAR means
mean(car1_stay$car)
mean(car2_stay$car)
mean(car3_stay$car)
mean(car4_stay$car)

# SCAR means
mean(car1_stay$scar)
mean(car2_stay$scar)
mean(car3_stay$scar)
mean(car4_stay$scar)

## Test stats
# Covariance-variance ratio adjusted st. errors and corresponding t-stat (KPP 2018); preferred
mean(car1_stay$scar)/sqrt(vcov_adj_var(ew1, sample_window, 1))
mean(car2_stay$scar)/sqrt(vcov_adj_var(ew2, sample_window, 2))
mean(car3_stay$scar)/sqrt(vcov_adj_var(ew3, sample_window, 3))
mean(car4_stay$scar)/sqrt(vcov_adj_var(ew4, sample_window, 4))

# J-stats, E[AR] = 0 (Campbell, Lo, McKinlay 1997)
# J1-stat
mean(car1_stay$car)/sqrt((1/(nrow(car1_stay))^2)*sum(car1_stay$sd_ar^2))
mean(car2_stay$car)/sqrt((1/(nrow(car2_stay))^2)*sum(car2_stay$sd_ar^2))
mean(car3_stay$car)/sqrt((1/(nrow(car3_stay))^2)*sum(car3_stay$sd_ar^2))
mean(car4_stay$car)/sqrt((1/(nrow(car4_stay))^2)*sum(car4_stay$sd_ar^2))
# J2-stat
sqrt((nrow(car1_stay)*(216-4))/(216-2))*mean(car1_stay$scar)
sqrt((nrow(car2_stay)*(216-4))/(216-2))*mean(car2_stay$scar)
sqrt((nrow(car3_stay)*(216-4))/(216-2))*mean(car3_stay$scar)
sqrt((nrow(car3_stay)*(216-4))/(216-2))*mean(car3_stay$scar)

