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
      ts = ymd_hm(
        str_c(Fecha,str_sub(Hora, 1, 5)),
        tz = "Europe/Madrid"),
      consume = `Consumo (Wh)`) %>% 
    transmute_if(
      count(date(ts) == 25),
    
    )
}