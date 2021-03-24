# UTILS VERSION PABLO

library(tidyverse)
library(lubridate)

# Import each csv
import_csv <- function(file) {
  read_csv(file,
           col_types = cols(
             Fecha = col_character(),
             Hora = col_character(),
             `Consumo (Wh)` = col_double(),
             `Precio (€/kWh)` = col_skip(),
             `Coste por hora (€)` = col_skip()
           ),
           skip = 6
  )
}

tidy_csv <- function(file) {
  file %>%
    drop_na() %>%
    group_by(Fecha) %>% 
    transmute(
      ts = parse(Fecha),
      consumption = `Consumo (Wh)`
    ) %>%
    ungroup() %>% 
    select(ts, consumption)
}

parse <- function(date) {
  h <- length(date) - 1
  date <- ymd(date, tz = "Europe/Madrid")
  date + dhours(0:h)
}