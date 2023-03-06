setwd("/Users/yashsrivastav/Dropbox (Personal)/Personal_Projects/Russia")
library(ggplot2)

sample_firms <- read_rds("data/interim/sample_firms.rds")

## Announcement date distribution ---------------------------------------------
ggplot(data = sample_firms |>
         filter(Status == 3),
       aes(x = `Announcement Date`)) +
  geom_histogram(color = "black", fill = "white") +
  ggtitle("When Firms Announce Exit from Russia")
