library(stringr); library(dplyr)
cleanFirm <- function(string){
  string <- string |>
    tolower() |>
    str_replace_all("\\(.+\\)|( inc\\.?)$|( plc)$|( corp\\.?)$|\\.com|
                    |(?<!&)( co\\.?)$|( ltd\\.?)$|( l\\.?l\\.?c\\.?)$|
                    |( corporation)$|( incorporated)$|(?<= )inc(?= )|(?<= )co(?= )|
                    |(?<= )corp(?= )|hldgs|(?<= )holdings|(?<= )cos(?= )|
                    |( grp)$|( intl)$|( hlds)$", "") |>
    str_replace_all("intercontinentalexchange", "intercontinental exchange") |>
    str_replace_all("expedia group", "expedia") |>
    str_replace_all("biomarin pharmaceutical", "biomarin") |>
    str_replace_all("international business machs cor", "ibm") |>
    str_replace_all("kellogg(?!\\w)", "kelloggs") |>
    str_replace_all("disney walt", "disney") |>
    str_replace_all("dell technologies", "dell") |>
    str_replace_all("ford motor del", "ford") |>
    str_replace_all("valero energy new", "valero energy") |>
    str_replace_all("united parcel service", "ups") |>
    str_replace_all("the new york times", "new york times") |>
    str_replace_all("(?<=royal caribbean).+","") |>
    str_replace_all("nortonlifelock", "norton") |>
    str_replace_all("goldman sachs group", "goldman sachs") |>
    str_replace_all("(?<=gilead).+", "") |>
    str_replace_all("(?<=mccormick).+", "") |>
    str_replace_all("(?<=dupont).+", "") |>
    str_replace_all("schwab charles new", "charles schwab") |>
    str_replace_all("c b r e group", "cbre") |>
    str_replace_all("(?<=akamai) .+", "") |>
    str_replace_all("amazon com", "amazon") |>
    str_replace_all("(?<=levi strauss).+", "") |>
    str_replace_all("lauder estee cos", "estee lauder") |>
    str_replace_all("(?<=goodyear) .+", "") |>
    str_replace_all("-|/", " ") |>
    str_replace_all("([^([:alnum:]&\\s)])", "") |>
    str_replace_all("\\s{2,}", " ") |>
    trimws()
}
