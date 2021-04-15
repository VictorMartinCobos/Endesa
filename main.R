# ENDESA REPORT GENERATOR MAIN ----------------------------------------------
library(tidyverse)
library(lubridate)
library(gganimate)
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

year_con_plot <- ggplot(
  year_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Año", y = "Consumo (Wh)")  +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

mean_month_consumption <- consumption_func_time(
  data_tidy,
  mean,
  month,
  label = TRUE)

month_con_plot <- ggplot(
  mean_month_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Mes", y = "Consumo (Wh)")  +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

mean_day_consumption <- consumption_func_time(data_tidy, mean, day)

day_con_plot <- ggplot(
  mean_day_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Día del mes", y = "Consumo (Wh)")  +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

mean_wday_consumption <- consumption_func_time(
  data_tidy,
  mean,
  wday,
  label = TRUE,
  week_start = 1)

wday_consumption_facet <- data_tidy %>% 
  group_by(year = year(ts), wday = wday(ts)) %>% 
  summarise (consumption = mean(consumption)) %>% 
  ggplot(aes(y = consumption, x = wday, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Día de la semana", y = "Consumo (Wh)")  +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040") +
    facet_grid(. ~ year)

wday_con_plot <- ggplot(
  mean_wday_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Día de la semana", y = "Consumo (Wh)")  +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

mean_hour_consumption <- consumption_func_time(data_tidy, mean, hour)

hour_con_plot <- ggplot(
  mean_hour_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Hora del día", y = "Consumo (Wh)") +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

mean_week_consumption <- consumption_func_time(data_tidy, mean, week)

week_con_plot <- ggplot(
  mean_week_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_bar(stat = "identity") +
    labs(x = "Semana del año", y = "Consumo (Wh)") +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040") +
    scale_x_continuous(breaks = seq(1, 52, by = 5))

mean_yday_consumption <- consumption_func_time(data_tidy, mean, yday)

yday_con_plot <- ggplot(
  mean_yday_consumption,
  aes(y = consumption, x = ts, fill = consumption)) +
    geom_area() +
    labs(x = "Día del año", y = "Consumo (Wh)") +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040")

data_day_month <- data_tidy %>% 
  group_by(day = day(ts), month = month (ts)) %>% 
  summarise(consumption = mean(consumption))

data_day_month_labeled <- data_day_month %>% 
  transmute(
    day,
    month = case_when(
      month == 1 ~ "Enero", month == 2 ~ "Febrero", month == 3 ~ "Marzo",
      month == 4 ~ "Abril", month == 5 ~ "Mayo", month == 6 ~ "Junio",
      month == 7 ~ "Julio", month == 8 ~ "Agosto", month == 9 ~ "Septiembre",
      month == 10 ~ "Octubre", month == 11 ~ "Noviembre",
      month == 12 ~ "Diciembre"),
    consumption)

#Animated plot (Rmarkdown)
animated_month_plot <- data_day_month %>% 
  ggplot(aes(day, consumption, fill = consumption)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "#2f00ff", high = "#ff0040") +
    labs(
      title = "Mes: {as.integer(frame_time)}",
      x = "Día del mes",
      y = "Consumo (Wh)") +
    transition_time(month) +
    ease_aes("linear")

#Animated plot (Shiny)
animate(animated_month_plot, renderer = gifski_renderer("month_plot.gif"))

#Function to ask for file name
doc_name <- rstudioapi::showPrompt(
  "Nombre de fichero",
  "Selecciona un nombre para el fichero",
  "Endesa_Report"
)

#Run ShinyApp
rmarkdown::run(
  "report_endesa.Rmd",
  render_args = list(
    output_file = str_glue("{doc_name}.html"),
    params = list(
      total_consumption = total_consumption,
      year_consumption = year_consumption,
      mean_month_consumption = mean_month_consumption,
      mean_day_consumption = mean_day_consumption ,
      mean_wday_consumption = mean_wday_consumption,
      mean_hour_consumption = mean_hour_consumption,
      mean_week_consumption = mean_week_consumption,
      year_con_plot = year_con_plot,
      month_con_plot = month_con_plot,
      day_con_plot = day_con_plot,
      wday_con_plot = wday_con_plot,
      hour_con_plot = hour_con_plot,
      week_con_plot = week_con_plot,
      animated_month_plot = animated_month_plot,
      data_day_month_labeled = data_day_month_labeled,
      wday_consumption_facet = wday_consumption_facet
  ))
)

