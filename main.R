# ENDESA REPORT GENERATOR MAIN ----------------------------------------------
library(tidyverse)
library(lubridate)
source("utils.R")

#Select a Directory
directory_path <- rstudioapi::selectDirectory(
  caption = "Select Directory",
  label = "Select",
  path = "~/Documents/"
)

#Obtain a list of all csv contained in directory chosen
list_csv_names <- list.files(
  path = directory_path,
  pattern = "*.csv",
  all.files = TRUE,
  full.names = TRUE
)

#Import each csv
csv_list <- map(list_csv_names, import_csv)

#Join csv in an unique tibble
data <- reduce(csv_list, bind_rows)

#Tidy csv
data_tidy <-tidy_csv(data)

#Generate report
total_consumption <- consumption_func_time(data_tidy, sum) %>% pull(1)
year_consumption <- consumption_func_time(data_tidy, sum, year)
year_con_plot <- ggplot(year_consumption,aes(y = consumption, x = ts))
  + geom_bar(stat = "identity")
mean_month_consumption <- consumption_func_time(
  data_tidy,
  mean,
  month,
  label = TRUE)
mean_day_consumption <- consumption_func_time(data_tidy, mean, day)
mean_wday_consumption <- consumption_func_time(
  data_tidy,
  mean,
  wday,
  label = TRUE,
  week_start = 1)
mean_hour_consumption <- consumption_func_time(data_tidy, mean, hour)
mean_date_consumption <- consumption_func_time(data_tidy, mean, date)

doc_name <- rstudioapi::showPrompt(
  "Nombre de fichero",
  "Selecciona un nombre para el fichero",
  "Endesa_Report"
)

rmarkdown::render(
  "report_endesa.Rmd",
  output_file = str_glue("{doc_name}.html"),
  params = list(
    total_consumption = total_consumption,
    year_consumption = year_consumption,
    mean_month_consumption = mean_month_consumption,
    mean_day_consumption = mean_day_consumption ,
    mean_wday_consumption = mean_wday_consumption,
    mean_hour_consumption = mean_hour_consumption,
    mean_date_consumption = mean_date_consumption,
  )
)

