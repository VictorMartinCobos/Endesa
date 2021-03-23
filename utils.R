# ENDESA REPORT GENERATOR UTILS -------------------------------------------
library(tidyverse)
library(lubridate)

#Import each csv
import_csv <- function(file) {
  read_csv(file,
    col_types = cols(
      Fecha = col_character(),
      Hora = col_character(),
      `Consumo (Wh)` = col_double(),
      `Precio (€/kWh)` = col_skip(), 
      `Coste por hora (€)` = col_skip() 
    ),
    skip = 6)
}
  
#Tidy each csv
tidy_csv <- function(file) {
  file %>% 
    drop_na() %>%
    transmute(
      date = Fecha,
      hour = hm(str_sub(Hora, 1, 5)),
      consume = `Consumo (Wh)`) %>%
    add_count(date) %>% 
    transmute(
      date,
      hour,
      consume,
      ts = case_when(
        n == 25 & hour == "2H 0M" ~ ymd_hms(
          str_c(date, hour - hm("1H 0M"), sep = " "),
          tz = "Europe/Madrid")
        + dhours(),
        n == 25 & hour > "2H 0M" ~ ymd_hms(
          str_c(date, hour - hm("1H 0M"), sep = " "),
          tz = "Europe/Madrid"),
        n == 23 & hour >= "2H 0M" ~ ymd_hms(
          str_c(date, hour + hm("1H 0M"), sep = " "),
          tz = "Europe/Madrid"),
        hour == "0s" ~ ymd_h(
          str_c(date, "0H", sep = " "),
          tz = "Europe/Madrid"),
        TRUE ~ ymd_hms(str_c(date, hour, sep = " "), tz = "Europe/Madrid")
      ),
      n)
  }
      

#25 horas -> CEST a CET. Las 2 se fuerzan a CEST. El resto se resta una hora.
#23 horas -> CET a CEST
