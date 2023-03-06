ewprep <- function(df, win){
  if (win == 1){
    df <- df |>
      filter(COMNAM != "",
             is.na(COMNAM) == F) |>
      arrange(COMNAM, date) |>
      mutate(day = wday(`Announcement Date`, label = TRUE),
             leave = ifelse((Status == 3 & `Sub-Status` == 4) | (Status == 4 & `Sub-Status` == 5),
                            1,
                            0),
             announcement = if_else(`Announcement Date` == date, 1, 0),
             announcement = case_when(day == "Sat" & date <= `Announcement Date` + days(2) & date > `Announcement Date` ~ 1,
                                      day == "Sun" & date <= `Announcement Date` + days(1) & date > `Announcement Date` ~ 1,
                                      TRUE ~ announcement),
             event_date = case_when(announcement == 1 & date == `Announcement Date` ~ `Announcement Date`)) |>
      filter(COMNAM != "")
  }
  else if(win == 2){
    df <- df |>
      filter(COMNAM != "",
             is.na(COMNAM) == F) |>
      arrange(COMNAM, date) |>
      mutate(day = wday(`Announcement Date`, label = TRUE),
             leave = ifelse((Status == 3 & `Sub-Status` == 4) | (Status == 4 & `Sub-Status` == 5),
                            1,
                            0),
             announcement = if_else(`Announcement Date` == date, 1, 0),
             announcement = case_when(day == "Mon" & date <= `Announcement Date` + days(1) & date > `Announcement Date` ~ 1,
                                      day == "Tue" & date <= `Announcement Date` + days(1) & date > `Announcement Date` ~ 1,
                                      day == "Wed" & date <= `Announcement Date` + days(1) & date > `Announcement Date` ~ 1,
                                      day == "Thu" & date <= `Announcement Date` + days(1) & date > `Announcement Date` ~ 1,
                                      day == "Fri" & date <= `Announcement Date` + days(3) & date > `Announcement Date` ~ 1,
                                      day == "Sat" & date <= `Announcement Date` + days(4) & date > `Announcement Date` ~ 1,
                                      day == "Sun" & date <= `Announcement Date` + days(3) & date > `Announcement Date` ~ 1,
                                      TRUE ~ announcement),
             event_date = case_when(announcement == 1 & date == `Announcement Date` ~ `Announcement Date`,
                                    announcement == 1 & date == `Announcement Date` + days(1) ~ `Announcement Date` + days(1))) |>
      filter(COMNAM != "")
  }
  else if(win == 4){
    df <- df |>
      filter(COMNAM != "",
             is.na(COMNAM) == F) |>
      arrange(COMNAM, date) |>
      mutate(day = wday(`Announcement Date`, label = TRUE),
             leave = ifelse((Status == 3 & `Sub-Status` == 4) | (Status == 4 & `Sub-Status` == 5),
                            1,
                            0),
             announcement = if_else(`Announcement Date` == date, 1, 0),
             announcement = case_when(day == "Mon" & date <= `Announcement Date` + days(3) & date > `Announcement Date` ~ 1,
                                      day == "Tue" & date <= `Announcement Date` + days(3) & date > `Announcement Date` ~ 1,
                                      day == "Wed" & date <= `Announcement Date` + days(6) & date > `Announcement Date` ~ 1,
                                      day == "Thu" & date <= `Announcement Date` + days(6) & date > `Announcement Date` ~ 1,
                                      day == "Fri" & date <= `Announcement Date` + days(6) & date > `Announcement Date` ~ 1,
                                      day == "Sat" & date <= `Announcement Date` + days(5) & date > `Announcement Date` ~ 1,
                                      day == "Sun" & date <= `Announcement Date` + days(4) & date > `Announcement Date` ~ 1,
                                      TRUE ~ announcement),
             event_date = case_when(announcement == 1 & date == `Announcement Date` ~ `Announcement Date`,
                                    announcement == 1 & date == `Announcement Date` + days(1) ~ `Announcement Date` + days(1),
                                    announcement == 1 & date == `Announcement Date` + days(2) ~ `Announcement Date` + days(2),
                                    announcement == 1 & date == `Announcement Date` + days(3) ~ `Announcement Date` + days(3))) |>
      filter(COMNAM != "")
  }
}

build_car <- function(df){
  df <- df |>
    filter(leave == 1 & announcement == 1) |>
    group_by(COMNAM) |>
    summarise(car = round(sum(ab_ret, na.rm = TRUE), 3))
}

kpp_var <- function(df, car){
  var_a <- (1/nrow(car)^2)*sum((car$car - mean(car$car))^2)
  
  vardf_b <- df |>
    group_by(event_date) |>
    summarise(ar = sum(ab_ret, na.rm = TRUE)) |>
    mutate(ar_t = ar - mean(ar)) |>
    filter(is.na(event_date) == F)
  var_b <- (1/nrow(car)^2)*sum((vardf_b$ar_t)^2)
  
  ar_bar <- df |>
    group_by(date) |>
    summarise(ar_bar = mean(ab_ret, na.rm = TRUE))
  vardf_ar <- df |>
    filter(announcement == 1) |>
    group_by(COMNAM) |>
    left_join(ar_bar,
              by = "date") |>
    mutate(ab_ret_dev = (ab_ret - ar_bar),
           ab_ret_sq_dev = ab_ret_dev^2) |> 
    summarise(arsd_sum = sum(ab_ret_sq_dev))
  var_ar <- (1/nrow(car)^2)*sum(vardf_ar$arsd_sum)
  
  var_adj <- var_a + var_b - var_ar
  # return(var_a)
  # return(var_b)
  return(var_ar)
  # return(var_adj)
}

vcov_adj_var <- function(df, sample_window, window){
  #Calculating average days of overlapping event windows
  overlap <- df |>
    filter(is.na(event_date) == F) |>
    group_by(event_date) |>
    summarise(num = n())
  delta <- mean(overlap$num, na.rm = TRUE)/window
  #Calculating cov[AR]
  xs <- sample_window |>
    filter(COMNAM != "") |>
    select(date, COMNAM, aret) |>
    mutate(aret = round(aret, 3),
           sar = aret/sd(aret, na.rm = TRUE)) |>
    select(-aret) |>
    pivot_wider(names_from = date,
                values_from = sar,
                values_fill = NA) |>
    select(-COMNAM)
  covmat <- round(cor(xs4, use = "complete.obs"), 5)
  v <- mean(covmat, na.rm = TRUE)
  
  # Calculating variance of single-day standardized ARs
  ftdf <- df |>
    filter(leave == 1) |>
    group_by(COMNAM) |> 
    mutate(sd = sd(ab_ret, na.rm = TRUE)) |>
    ungroup() |>
    mutate(sar = ab_ret/sd,
           mean_sar = mean(sar, na.rm = TRUE)) |>
    group_by(COMNAM) |>
    summarise(dev_sar = sum((sar - mean_sar)^2))
  var_sar <- (1/nrow(ftdf))*sum(ftdf$dev_sar, na.rm = TRUE)
  theta <- v/var_sar
    
  return(((1/nrow(ftdf))*var_sar*window)*(1+delta*(nrow(ftdf)-1))*theta)
}
