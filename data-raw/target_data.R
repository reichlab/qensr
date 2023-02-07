## code to prepare `inst/extdata/target_data`
library(readr)
library(dplyr)
library(covidData)

flu_data <- covidData::load_data(spatial_resolution = c("state", "national"),
                                 temporal_resolution = "weekly",
                                 measure = "flu hosp")

flu_data <- flu_data %>%
    dplyr::transmute(location, date, value = inc) %>%
    dplyr::filter(date >= "2021-01-01", date <= "2022-12-31")

readr::write_csv(flu_data, "inst/extdata/target_data/flu_hosps.csv")
