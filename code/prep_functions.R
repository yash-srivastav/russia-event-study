ewprep <- function(df, win){
  if (win == 1){
    df <- df |>
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
