setwd("/Users/yashsrivastav/Dropbox (Personal)/Personal_Projects/Russia")
library(ggplot2); library(stargazer); library(lubridate)

sample_firms <- read_rds("data/interim/sample_firms.rds")
sample_window <- read_rds("data/interim/sample_window.rds")
returns <- read.csv("data/raw/crsp_returns.csv")
fnds <- read.csv("data/raw/sample_fundamentals.csv")

## Announcement date distribution ---------------------------------------------
ggplot(data = sample_firms |>
         filter(Status == 3),
       aes(x = `Announcement Date`)) +
  geom_histogram(color = "black", fill = "white") +
  ggtitle("When Firms Announce Exit from Russia")
ggsave("figures/exit_dist.pdf")

## Firm status distribution ---------------------------------------------------
firm_status <- sample_firms |>
  mutate(n = nrow(sample_firms)) |>
  group_by(`Status Classification`, `Sub-Status Classification`) |>
  summarise(proportion = n()/n) |>
  distinct()

## Market cap of firms that leave Russia --------------------------------------
mkt_cap <- returns |>
  mutate(date = as.character(date),
         date = as.Date(date, format = "%Y%m%d")) |>
  inner_join(sample_firms |>
               select(-TICKER),
             by = "COMNAM") |>
  filter(date == as.Date("2022-01-31")) |>
  mutate(mc = (PRC*SHROUT),
         log_mc = log(mc))
# Market cap of exiting firms
mc_dist <- ggplot(mkt_cap |>
                    filter(Status == 3 | Status == 4),
                  aes(x = log_mc)) +
  geom_histogram(color = "black", fill = "white") +
  ggtitle("Distribution of Exiting Firms, by log market cap") +
  xlab("log(Market Cap)")
ggsave("figures/mc_distribution.pdf")

# Boxplot of firm status by market cap
ggplot(data = mkt_cap, aes(x = as.factor(`Status Classification`),
                           y = log_mc,
                           color = as.factor(`Status Classification`))) +
  geom_boxplot() +
  xlab("Status") +
  ylab("log(Market Cap)") + 
  labs(color = "Firm Status") +
  ggtitle("Market Size by Firm Status")
ggsave("figures/mc_boxplot.pdf")

## ARs and SCARs before and after announcement ----------------------------------------
ew <- read_rds("data/interim/event_windows/event_window.rds")
sample_sd <- read_rds("data/interim/sample_sd.rds")
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))

pre_post <- ew |>
  filter(Status == 3 | Status == 4,
         is.na(`Announcement Date`) == F) |>
  mutate(ann_date = case_when(wday(`Announcement Date`, label = TRUE) == "Sat" ~ bizdays::offset(`Announcement Date`, 1, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Sun" ~ bizdays::offset(`Announcement Date`, 1, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Mon" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Tue" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Wed" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Thu" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Fri" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar))) |> 
  mutate(rel_day = case_when(date == bizdays::offset(ann_date, -5, cal = business_calendar) ~ -5,
                             date == bizdays::offset(ann_date, -4, cal = business_calendar) ~ -4,
                             date == bizdays::offset(ann_date, -3, cal = business_calendar) ~ -3,
                             date == bizdays::offset(ann_date, -2, cal = business_calendar) ~ -2,
                             date == bizdays::offset(ann_date, -1, cal = business_calendar) ~ -1,
                             date == ann_date ~ 0,
                             date == bizdays::offset(ann_date, 1, cal = business_calendar) ~ 1,
                             date == bizdays::offset(ann_date, 2, cal = business_calendar) ~ 2,
                             date == bizdays::offset(ann_date, 3, cal = business_calendar) ~ 3,
                             date == bizdays::offset(ann_date, 4, cal = business_calendar) ~ 4,
                             date == bizdays::offset(ann_date, 5, cal = business_calendar) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar))

# pre_post[1,5] <- 0
# pre_post[1,6] <- 0
# pre_post[1,7] <- 0

# SCARs
# ggplot(pre_post, aes(x = rel_day, y = rel_scar)) +
#   geom_line() +
#   ggtitle("SCARs for Leaving/Exiting Firms") +
#   xlab("Day") +
#   ylab("SCAR")

# CARs
# ggplot(pre_post, aes(x = rel_day, y = rel_car)) +
#   geom_line()

# Comparing CARs with firms that stay
pre_post_stay <- ew |>
  filter(Status == 1 | Status == 2,
         is.na(`Announcement Date`) == F) |>
  mutate(ann_date = case_when(wday(`Announcement Date`, label = TRUE) == "Sat" ~ bizdays::offset(`Announcement Date`, 1, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Sun" ~ bizdays::offset(`Announcement Date`, 1, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Mon" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Tue" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Wed" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Thu" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar),
                              wday(`Announcement Date`, label = TRUE) == "Fri" ~ bizdays::offset(`Announcement Date`, 0, cal = business_calendar))) |> 
  mutate(rel_day = case_when(date == bizdays::offset(ann_date, -5, cal = business_calendar) ~ -5,
                             date == bizdays::offset(ann_date, -4, cal = business_calendar) ~ -4,
                             date == bizdays::offset(ann_date, -3, cal = business_calendar) ~ -3,
                             date == bizdays::offset(ann_date, -2, cal = business_calendar) ~ -2,
                             date == bizdays::offset(ann_date, -1, cal = business_calendar) ~ -1,
                             date == ann_date ~ 0,
                             date == bizdays::offset(ann_date, 1, cal = business_calendar) ~ 1,
                             date == bizdays::offset(ann_date, 2, cal = business_calendar) ~ 2,
                             date == bizdays::offset(ann_date, 3, cal = business_calendar) ~ 3,
                             date == bizdays::offset(ann_date, 4, cal = business_calendar) ~ 4,
                             date == bizdays::offset(ann_date, 5, cal = business_calendar) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar))
pre_post <- pre_post |>
  mutate(status = "leave") |>
  rbind(pre_post_stay |>
          mutate(status = "stay"))
pre_post <- pre_post |>
  pivot_longer(!c(rel_day, status))

ggplot(data = pre_post |>
         filter(name == "rel_car"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() +
  ggtitle("Average CAR, relative to event day") +
  xlab("Event Day") +
  ylab("CAR")
ggsave("figures/event_car.pdf",
       width = 7,
       height = 4,
       units = "in")

ggplot(data = pre_post |>
         filter(name == "rel_scar"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() + 
  ggtitle("Average SCAR, relative to event day") +
  xlab("Event Day") +
  ylab("SCAR")
ggsave("figures/event_scar.pdf",
       width = 7,
       height = 4,
       units = "in")

## Looking at firm fundamentals -----------------------------------------------
fnds <- fnds |>
  mutate(gross_margin = (revt - cogs)/cogs,
         liq_ratio = act/lct,
         cash_ratio = ch/lct) |>
  left_join(sample_firms, by = c("tic" = "TICKER"))
fnds_reg <- fnds |>
  mutate(mc = prcc_c*csho,
         lnmc = log(mc)) |>
  select(tic, COMNAM, Status, gross_margin,
         liq_ratio, cash_ratio, lnmc, act)
fnds_reg <- fnds_reg[is.finite(rowSums(fnds_reg[,4:7])),]
fnds_reg <- fnds_reg |>
  mutate(Status = case_when(Status == 1 | Status == 2 ~ 0,
                            Status == 3 | Status == 4 ~ 1))
decision <- lm(Status ~ gross_margin + liq_ratio + cash_ratio + lnmc + act,
               data = fnds_reg)
summary(decision)
stargazer(decision)

## Fundamentals differences between exiting and staying firms ------------------
ggplot(data = fnds |>
         filter(gross_margin < 100 & gross_margin > -100),
       aes(x = as.factor(`Status Classification`),
                            y = gross_margin,
                            color = as.factor(`Status Classification`))) +
  geom_boxplot() +
  xlab("Status") +
  ylab("Gross Margin") + 
  labs(color = "Firm Status") +
  ggtitle("Gross Margin by Firm Status")
ggsave("figures/gross_margin.pdf")

ggplot(data = fnds |>
         filter(cash_ratio < 100 & cash_ratio > -100),
       aes(x = as.factor(`Status Classification`),
           y = cash_ratio,
           color = as.factor(`Status Classification`))) +
  geom_boxplot() +
  xlab("Status") +
  ylab("Cash Ratio") + 
  labs(color = "Firm Status") +
  ggtitle("Cash Ratio by Firm Status")
ggsave("figures/cash_ratio.pdf")

ggplot(data = fnds |>
         filter(liq_ratio < 100 & liq_ratio > -100),
       aes(x = as.factor(`Status Classification`),
           y = liq_ratio,
           color = as.factor(`Status Classification`))) +
  geom_boxplot() +
  xlab("Status") +
  ylab("Liquidity Ratio") + 
  labs(color = "Firm Status") +
  ggtitle("Liquidity Ratio by Firm Status")
ggsave("figures/liquidity_ratio.pdf")

## Relationship between CARs and firm fundamentals -----------------------------
car3 <- read_rds("data/interim/cars/car3.rds")
car3 <- car3 |>
  left_join(sample_firms |>
              select(TICKER, COMNAM),
            by = "COMNAM") |>
  left_join(fnds_reg |>
              select(-COMNAM), by = c("TICKER" = "tic")) |>
  filter(gross_margin > -50)

ggplot(data = car3, aes(x = gross_margin, y = car)) +
  geom_point()
ggplot(data = car3, aes(x = liq_ratio, y = car)) +
  geom_point()

## Looking at market returns over February and March ---------------------------
ggplot(data = event_window |>
         distinct(date, ewretd) |>
         filter(between(date, as.Date("2022-02-01"), as.Date("2022-12-31"))),
       aes(x = date, y = ewretd)) +
  geom_line()

## Comparing ARs for firms that left before/after after July 2022 --------------
after_leave <- ew |>
  filter(`Announcement Date` > as.Date("2022-07-01")) |>
  filter(Status == 3 | Status == 4) |>
  mutate(rel_day = case_when(date == `Announcement Date` - days(5) ~ -5,
                             date == `Announcement Date` - days(4) ~ -4,
                             date == `Announcement Date` - days(3) ~ -3,
                             date == `Announcement Date` - days(2) ~ -2,
                             date == `Announcement Date` - days(1) ~ -1,
                             date == `Announcement Date` ~ 0,
                             date == `Announcement Date` + days(1) ~ 1,
                             date == `Announcement Date` + days(2) ~ 2,
                             date == `Announcement Date` + days(3) ~ 3,
                             date == `Announcement Date` + days(4) ~ 4,
                             date == `Announcement Date` + days(5) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar),
         status = "leave") 

after_stay <- ew |>
  filter(`Announcement Date` > as.Date("2022-07-01")) |>
  filter(Status == 1 | Status == 2) |>
  mutate(rel_day = case_when(date == `Announcement Date` - days(5) ~ -5,
                             date == `Announcement Date` - days(4) ~ -4,
                             date == `Announcement Date` - days(3) ~ -3,
                             date == `Announcement Date` - days(2) ~ -2,
                             date == `Announcement Date` - days(1) ~ -1,
                             date == `Announcement Date` ~ 0,
                             date == `Announcement Date` + days(1) ~ 1,
                             date == `Announcement Date` + days(2) ~ 2,
                             date == `Announcement Date` + days(3) ~ 3,
                             date == `Announcement Date` + days(4) ~ 4,
                             date == `Announcement Date` + days(5) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar),
         status = "stay")
after <- after_leave |>
  rbind(after_stay)
rm(after_leave, after_stay)
after <- after |>
  pivot_longer(!c(rel_day, status))

ggplot(data = after |>
         filter(name == "rel_car"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() +
  ggtitle("Average CAR, relative to event day") +
  xlab("Event Day") +
  ylab("CAR")

ggplot(data = after |>
         filter(name == "rel_scar"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() + 
  ggtitle("Average SCAR, relative to event day") +
  xlab("Event Day") +
  ylab("SCAR")

# Before July
before_leave <- ew |>
  filter(`Announcement Date` < as.Date("2022-07-01")) |>
  filter(Status == 3 | Status == 4) |>
  mutate(rel_day = case_when(date == `Announcement Date` - days(5) ~ -5,
                             date == `Announcement Date` - days(4) ~ -4,
                             date == `Announcement Date` - days(3) ~ -3,
                             date == `Announcement Date` - days(2) ~ -2,
                             date == `Announcement Date` - days(1) ~ -1,
                             date == `Announcement Date` ~ 0,
                             date == `Announcement Date` + days(1) ~ 1,
                             date == `Announcement Date` + days(2) ~ 2,
                             date == `Announcement Date` + days(3) ~ 3,
                             date == `Announcement Date` + days(4) ~ 4,
                             date == `Announcement Date` + days(5) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar),
         status = "leave") 

before_stay <- ew |>
  filter(`Announcement Date` < as.Date("2022-07-01")) |>
  filter(Status == 1 | Status == 2) |>
  mutate(rel_day = case_when(date == `Announcement Date` - days(5) ~ -5,
                             date == `Announcement Date` - days(4) ~ -4,
                             date == `Announcement Date` - days(3) ~ -3,
                             date == `Announcement Date` - days(2) ~ -2,
                             date == `Announcement Date` - days(1) ~ -1,
                             date == `Announcement Date` ~ 0,
                             date == `Announcement Date` + days(1) ~ 1,
                             date == `Announcement Date` + days(2) ~ 2,
                             date == `Announcement Date` + days(3) ~ 3,
                             date == `Announcement Date` + days(4) ~ 4,
                             date == `Announcement Date` + days(5) ~ 5)) |>
  filter(is.na(rel_day) == F) |>
  left_join(sample_sd, by = "COMNAM") |>
  mutate(sar = ab_ret/sd_ar) |>
  group_by(rel_day) |>
  summarise(rel_aret = mean(ab_ret, na.rm = TRUE),
            rel_sar = mean(sar, na.rm = TRUE),
            rel_ewret = mean(ewretd, na.rm = TRUE)) |>
  mutate(rel_car = cumsum(rel_aret),
         rel_cewret = cumsum(rel_ewret),
         rel_scar = cumsum(rel_sar),
         status = "stay")
before <- before_leave |>
  rbind(before_stay)
rm(before_leave,before_stay)
before <- before |>
  pivot_longer(!c(rel_day, status))

ggplot(data = before |>
         filter(name == "rel_car"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() +
  ggtitle("Average CAR, relative to event day") +
  xlab("Event Day") +
  ylab("CAR")

ggplot(data = before |>
         filter(name == "rel_scar"),
       aes(x = rel_day, y = value, color = status)) +
  geom_line() + 
  ggtitle("Average SCAR, relative to event day") +
  xlab("Event Day") +
  ylab("SCAR")
