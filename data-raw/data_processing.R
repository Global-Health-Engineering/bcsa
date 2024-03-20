
# description -------------------------------------------------------------

# r script to process uploaded raw black carbon data
# and combine it with metadata to create tidy data sets

# this includes the following steps:

# 1. importing the raw data and metadata
# 2. merging the raw data
# 3. creating tidy data sets of each experiment
# 4. exporting the tidy data sets

# note that the tidy datasets consist of
# raw data in a tidy form, and not the smoothed data.
# this allows the user of the data to apply their own
# smoothing methods.

# r packages --------------------------------------------------------------

library(readr)
library(tidyverse)
library(lubridate)
library(fs)
library(here)
library(tibbletime)
library(dplyr)
library(anytime)
# library(smooth)
library(hms)

# importing data ---------------------------------------------------------------

## ma200 parameters

parameters <- read_csv("data-raw/metadata/MA200-parameters.csv")

## metadata

### data structure (to define the start and end time of each experiment,
### the serial number of the MA200, and the type of the experiment)

#### complete data structure (including all the experiments)

data_structure <- read_csv("data-raw/metadata/data-structure.csv")

#### data structure for mobile monitoring separating highways from remote roads

data_structure_mm_roads <- read_csv("data-raw/metadata/id-mobile-monitoring-roads.csv")

### id's (details of each experiment)

#### mobile monitoring id's
#### (location where the monitoring was done, the person who did the monitoring)

id_mm <- read_csv("data-raw/metadata/id-mobile-monitoring.csv")

#### personal monitoring id's
#### (location where the monitoring was done, the person who did the monitoring)

id_pm <- read_csv("data-raw/metadata/id-personal-monitoring.csv")

#### information of the times at which open waste burning was observed during personal monitoring

id_pm_trips <- read_csv("data-raw/metadata/id-personal-monitoring-trips.csv")

#### stationary monitoring id's (location where the monitoring was done)

id_sm <- read_csv("data-raw/metadata/id-stationary-monitoring.csv")

#### sensor collocation id's
#### (information on the experiment phase during which the sensors were collocated)

id_sc <- read_csv("data-raw/metadata/id-sensor-collocation.csv")

## raw data from the two ma200 monitors

raw_data <- dir_ls("data-raw/")

raw_data_files <- list.files("data-raw/")

raw_data_csv <- str_subset(raw_data, pattern = ".csv$")

raw_data_csv_paths <- path(here::here(raw_data_csv))

raw_data_list <- list()

# merging data ---------------------------------------------------------------

## combine all the raw ma200 data in one dataset

for (i in seq_along(raw_data_csv)) {
  raw_data_list[[i]] <- read_csv(file = here::here(raw_data_csv)[[i]])  |>
    mutate(file_name = raw_data_files[[i]])
}

raw_data_all <- raw_data_list |>
  bind_rows()

## create a subset of ma200 raw data containing only required variables

raw_data_subset <- raw_data_all              |>
  select("Serial number",
         "Session ID",
         "Date local (yyyy/MM/dd)",
         "Time local (hh:mm:ss)",
         "GPS lat (ddmm.mmmmm)",
         "GPS long (dddmm.mmmmm)",
         "UV BCc",
         "Blue BCc",
         "IR BCc")                  |>
  rename(serial_number = "Serial number",
         session_id = "Session ID",
         date = "Date local (yyyy/MM/dd)",
         time = "Time local (hh:mm:ss)",
         lat = "GPS lat (ddmm.mmmmm)",
         long = "GPS long (dddmm.mmmmm)",
         uv_bcc = "UV BCc",
         blue_bcc = "Blue BCc",
         ir_bcc = "IR BCc") |>
  mutate(uv_babs = uv_bcc*parameters$MAC[1],
         blue_babs = blue_bcc*parameters$MAC[2],
         ir_babs = ir_bcc*parameters$MAC[5],
         date_time = ymd_hms(paste(date, time))) |>
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))|>
  # 57745 observations
  # the observations during the startup time of MA200 are NAs. Hence, they
  # were removed
  drop_na(uv_babs, blue_babs, ir_babs)
  # 56668 observations

## select data between different experiments time intervals

### prepare the raw data to be used with filter_time function

raw_data_time <- as_tbl_time(raw_data_subset, index = date_time)

### prepare the data structure to be used with filter_time function

data_structure <- data_structure |>
  as_tbl_time(index = date_start)      |>
  mutate(start_time = ymd_hms(
    paste(date_start, start_time))) |>
  mutate(end_time = ymd_hms(
    paste(date_end, end_time)))

### divide the data of 0416 and 0420 monitors

df_0416 <- tibble()
df_0420 <- tibble()

raw_data_time_0416 <- raw_data_time |>
  filter(serial_number == "MA200-0416") |>
  arrange(date_time)

raw_data_time_0420 <- raw_data_time                            |>
  filter(serial_number == "MA200-0420")  |>
  arrange(date_time)

data_structure_0416 <- data_structure    |>
  filter(serial_number == "MA200-0416")  |>
  arrange(start_time)

data_structure_0420 <- data_structure    |>
  filter(serial_number == "MA200-0420")  |>
  arrange(start_time)

### select data between experiments time intervals

for(i in seq_along(unique(data_structure_0416$id)))
{
  test_0416 <- raw_data_time_0416                         |>
    filter_time(
      data_structure_0416$start_time[i]
      ~ data_structure_0416$end_time[i])

  test_0416 <- test_0416                       |>
    mutate(
      id = rep(
        data_structure_0416$id[i],nrow(test_0416)))

  df_0416 <- df_0416                           |>
    bind_rows(test_0416)
}

for(i in seq_along(unique(data_structure_0420$id)))
{
  test_0420 <- raw_data_time_0420                         |>
    filter_time(
      data_structure_0420$start_time[i]
      ~ data_structure_0420$end_time[i])

  test_0420 <- test_0420                       |>
    mutate(
      id = rep(
        data_structure_0420$id[i],nrow(test_0420)))

  df_0420 <- df_0420                           |>
    bind_rows(test_0420)
}

### combine the data of 0416 and 0420

df_all_raw <- df_0416 |>
  bind_rows(df_0420) |>
  left_join(data_structure,
            by=c("id","session_id","serial_number"))

### select the mobile monitoring roads data

data_structure_mm_roads <- data_structure_mm_roads |>
  as_tbl_time(index = date_start)      |>
  mutate(start_time = ymd_hms(
    paste(date_start, start_time))) |>
  mutate(end_time = ymd_hms(
    paste(date_end, end_time)))

### divide the data of 0416 and 0420 monitors

df_all_raw_exc <- df_all_raw |>
  select(- start_time, -end_time)

df_all_raw_time <- as_tbl_time(df_all_raw_exc, index = date_time)

df_mm_0416 <- tibble()
df_mm_0420 <- tibble()

raw_data_time_mm_0416 <- df_all_raw_time |>
  filter(serial_number == "MA200-0416") |>
  arrange(date_time)

raw_data_time_mm_0420 <- df_all_raw_time  |>
  filter(serial_number == "MA200-0420")  |>
  arrange(date_time)

data_structure_mm_0416 <- data_structure_mm_roads    |>
  filter(serial_number == "MA200-0416")  |>
  arrange(start_time)

data_structure_mm_0420 <- data_structure_mm_roads    |>
  filter(serial_number == "MA200-0420")  |>
  arrange(start_time)

### select data between experiments time intervals

for(i in seq_along(unique(data_structure_mm_0416$id_road_type))){
  test_0416 <- raw_data_time_mm_0416                         |>
    filter_time(
      data_structure_mm_0416$start_time[i]
      ~ data_structure_mm_0416$end_time[i])

  test_0416 <- test_0416                       |>
    mutate(
      id_road_type = rep(
        data_structure_mm_0416$id_road_type[i],nrow(test_0416)))

  df_mm_0416 <- df_mm_0416                           |>
    bind_rows(test_0416)
}

for(i in seq_along(unique(data_structure_mm_0420$id_road_type))){
  test_0420 <- raw_data_time_mm_0420                         |>
    filter_time(
      data_structure_mm_0420$start_time[i]
      ~ data_structure_mm_0420$end_time[i])

  test_0420 <- test_0420                       |>
    mutate(
      id_road_type = rep(
        data_structure_mm_0420$id_road_type[i],nrow(test_0420)))

  df_mm_0420 <- df_mm_0420                           |>
    bind_rows(test_0420)
}

### combine the data of 0416 and 0420

df_mm_road_type <- df_mm_0416 |>
  bind_rows(df_mm_0420) |>
  left_join(id_mm,
            by=c("id")) |>
  left_join(data_structure_mm_roads,
            by = c("id_road_type","id", "serial_number", "session_id", "date_start", "date_end","exp_type"))|>
  #4041 observations - the zero values in lat long were removed
  filter(!lat %in% 0,
         !long %in% 0) |>
  #4038 values (only 3 were removed)
  #remove values during sensor collocation (remove 420 monitor - random)
  filter(!id %in% c(17:24,65:72))
  #3240 values

# create data files of each experiment ---------------------------------------------

## aae experiments data

df_aae <- df_all_raw |>
  filter(exp_type %in% c("waste_burning", "cooking", "vehicles")) |>
  select(-comment)

## mobile monitoring (mm) data

df_mm <- df_all_raw |>
  filter(exp_type == "mobile_monitoring") |>
  left_join(id_mm, by = "id") |>
  filter(!lat %in% c(0, NA),
         !long %in% c(0, NA)) |>
  filter(!id %in% c(17:24,65:72)) |>
  select(-emission_source,
         -comment,
         -collocation,
         -dc_1,
         -dc_2)

## personal monitoring (pm) data

df_pm_wo_kacheri <- df_all_raw |>
  filter(exp_type == "personal_monitoring") |>
  left_join(id_pm, by = "id") |>
  filter(!lat %in% c(0, NA),
         !long %in% c(0, NA)) |>
  filter(!settlement_id %in% "Kacheri")

df_pm <- df_all_raw |>
  filter(exp_type == "personal_monitoring") |>
  filter(!time %in% parse_hms(c("14:24:30",
                                "14:31:30",
                                "14:32:00",
                                "14:32:30",
                                "14:33:00",
                                "14:33:31",
                                "14:34:00",
                                "14:34:01",
                                "14:34:30",
                                "14:35:01",
                                "14:35:30",
                                "14:50:31",
                                "14:51:00",
                                "14:57:00",
                                "14:57:30",
                                "14:58:00")))|>
  left_join(id_pm, by = "id") |>
  filter(settlement_id == "Kacheri",
         !lat %in% c(0, NA),
         !long %in% c(0, NA)) |>
  bind_rows(df_pm_wo_kacheri) |>
  select(-emission_source,
         -comment,
         -dc)

df_pm_trips <- data_structure |>
  filter(exp_type == "personal_monitoring") |>
  left_join(id_pm_trips, by = c("id")) |>
  select(-emission_source,
         -comment,
         -comments)

## stationary monitoring data

df_sm <- df_all_raw |>
  filter(exp_type == "stationary_monitoring") |>
  left_join(id_sm, by = "serial_number") |>
  select(-emission_source,
         -comment)

## sensor collocation data

df_collocation <- df_all_raw |>
  right_join(id_sc, by = "id") |>
  select(-emission_source,
         -comment)

# exporting data --------------------------------------------------------------

usethis::use_data(df_aae,
                  df_mm,
                  df_mm_road_type,
                  df_pm,
                  df_pm_trips,
                  df_sm,
                  df_collocation,
                  overwrite = TRUE)

# Export processed data to csv and xlsx files ----------------------------------

readr::write_csv(df_aae, here::here("inst", "extdata", "df_aae.csv"))
readr::write_csv(df_mm, here::here("inst", "extdata", "df_mm.csv"))
readr::write_csv(df_mm_road_type, here::here("inst", "extdata", "df_mm_road_type.csv"))
readr::write_csv(df_pm, here::here("inst", "extdata", "df_pm.csv"))
readr::write_csv(df_pm_trips, here::here("inst", "extdata", "df_pm_trips.csv"))
readr::write_csv(df_sm, here::here("inst", "extdata", "df_sm.csv"))
readr::write_csv(df_collocation, here::here("inst", "extdata", "df_collocation.csv"))

openxlsx::write.xlsx(df_aae, here::here("inst", "extdata", "df_aae.xlsx"))
openxlsx::write.xlsx(df_mm, here::here("inst", "extdata", "df_mm.xlsx"))
openxlsx::write.xlsx(df_mm_road_type, here::here("inst", "extdata", "df_mm_road_type.xlsx"))
openxlsx::write.xlsx(df_pm, here::here("inst", "extdata", "df_pm.xlsx"))
openxlsx::write.xlsx(df_pm_trips, here::here("inst", "extdata", "df_pm_trips.xlsx"))
openxlsx::write.xlsx(df_sm, here::here("inst", "extdata", "df_sm.xlsx"))
openxlsx::write.xlsx(df_collocation, here::here("inst", "extdata", "df_collocation.xlsx"))

