
# description -------------------------------------------------------------

# r script to process uploaded raw data into a tidy dataframe

# r packages --------------------------------------------------------------

library(readr)
library(tidyverse)
library(lubridate)
library(fs)
library(here)
library(tibbletime)
library(dplyr)
library(anytime)
library(smooth)

# read data ---------------------------------------------------------------

## ma200 parameters
parameters <- read_csv("data-raw/metadata/MA200-parameters.csv")

## metadata

### complete data structure

data_structure <- read_csv("data-raw/metadata/data-structure.csv")

### mobile monitoring id's

id_mm <- read_csv("data-raw/metadata/id-mobile-monitoring.csv")

### personal monitoring id's

id_pm <- read_csv("data-raw/metadata/id-personal-monitoring.csv")

### stationary monitoring id's

id_sm <- read_csv("data-raw/metadata/id-stationary-monitoring.csv")

## bc monitoring raw data
raw_data <- dir_ls("data-raw/")

raw_data_files <- list.files("data-raw/")

raw_data_csv <- str_subset(raw_data, pattern = ".csv$")

raw_data_csv_paths <- path(here::here(raw_data_csv))

raw_data_list <- list()

# tidy data ---------------------------------------------------------------

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
         ends_with(" BCc"))                  |>
  rename(serial_number = "Serial number",
         session_id = "Session ID",
         date = "Date local (yyyy/MM/dd)",
         time = "Time local (hh:mm:ss)",
         lat = "GPS lat (ddmm.mmmmm)",
         long = "GPS long (dddmm.mmmmm)",
         uv_bcc = "UV BCc",
         blue_bcc = "Blue BCc",
         green_bcc = "Green BCc",
         red_bcc = "Red BCc",
         ir_bcc = "IR BCc") |>
  mutate(uv_babs = uv_bcc*parameters$MAC[1],
         blue_babs = blue_bcc*parameters$MAC[2],
         green_babs = green_bcc*parameters$MAC[3],
         red_babs = red_bcc*parameters$MAC[4],
         ir_babs = ir_bcc*parameters$MAC[5],
         date_time = ymd_hms(paste(date, time))) |>
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")) |>
  drop_na(lat, long, uv_babs,blue_babs,
          green_babs,red_babs,ir_babs)

## select data between different experiments time intervals

### prepare the raw data to be used with filter_time function

FB <- as_tbl_time(raw_data_subset, index = date_time)

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

FB_0416 <- FB |>
  filter(serial_number == "MA200-0416") |>
  arrange(date_time)

FB_0420 <- FB                            |>
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
  test_0416 <- FB_0416                         |>
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
  test_0420 <- FB_0420                         |>
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

df <- df_0416 |>
  bind_rows(df_0420) |>
  left_join(data_structure,
            by=c("id","session_id","serial_number"))


# data smoothing ----------------------------------------------------------

## count number of negative values in each experiment in raw data

df_negative_raw <- df |>
  group_by(exp_type) |>
  summarise(neg_irbcc = sum(ir_bcc < 0),
            neg_bluebcc = sum(blue_bcc < 0),
            count = n())

### lot of negative values - smoothing needed

## apply cma of order 3, 5 and 7 and create a temporary dataset

df_smooth_raw <- df

list_df_raw <- list()

for (i in seq_along(unique(df_smooth_raw$id))) {

  list_df_raw[[i]] <- df_smooth_raw |>
    filter(id == i)

  list_df_raw[[i]] <- list_df_raw[[i]] |>
    mutate(ir_bcc_3 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 3, silent = TRUE)$fitted,
           blue_bcc_3 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 3, silent = TRUE)$fitted,
           ir_bcc_5 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 5, silent = TRUE)$fitted,
           blue_bcc_5 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 5, silent = TRUE)$fitted,
           ir_bcc_7 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 7, silent = TRUE)$fitted,
           blue_bcc_7 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 7, silent = TRUE)$fitted)
}

df_smooth_temp <- bind_rows(list_df_raw)

## count number of negative values after smoothing

df_negative_count <- df_smooth_temp |>
  group_by(exp_type) |>
  summarise(neg_irbcc = sum(ir_bcc < 0)/n()*100,
            neg_bluebcc = sum(blue_bcc < 0)/n()*100,
            neg_irbcc_3 = sum(ir_bcc_3 < 0)/n()*100,
            neg_bluebcc_3 = sum(blue_bcc_3 < 0)/n()*100,
            neg_irbcc_5 = sum(ir_bcc_5 < 0)/n()*100,
            neg_bluebcc_5 = sum(blue_bcc_5 < 0)/n()*100,
            neg_irbcc_7 = sum(ir_bcc_7 < 0)/n()*100,
            neg_bluebcc_7 = sum(blue_bcc_7 < 0)/n()*100)

### create a bar plot of the percentage of negative values
### number of negative values decreases with increase in the order
### of smoothing. we select the order of 5, as the decrease in the
### number of negative values between order 5 and 7 is not more than 1%

## smooth df with selected cma order (now 5)

df_smooth <- df

list_df <- list()

for (i in seq_along(unique(df_smooth$id))) {

  list_df[[i]] <- df_smooth |>
    filter(id == i)

  list_df[[i]] <- list_df[[i]] |>
    mutate(ir_bcc = smooth::cma(list_df[[i]]$ir_bcc, order = 5, silent = TRUE)$fitted,
           blue_bcc = smooth::cma(list_df[[i]]$blue_bcc, order = 5, silent = TRUE)$fitted,
           ir_babs = smooth::cma(list_df[[i]]$ir_babs, order = 5, silent = TRUE)$fitted,
           blue_babs = smooth::cma(list_df[[i]]$blue_babs, order = 5, silent = TRUE)$fitted)
}

### add aae at each observation of smooth dataframe

df_smooth <- bind_rows(list_df) |>
  mutate(aae = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5])))

# calculate aae -----------------------------------------------------------

## the aae is calculated with and without background correction

## correct bc abs data for background absorption

### calculate background absorption from stationary monitoring data

df_backgr <- df_smooth |>
  filter(exp_type == "stationary_monitoring") |>
  summarise(mean_ir = mean(ir_babs),
            mean_blue = mean(blue_babs))

### extract mean bc abs values at blue and IR wavelengths

mean_irbabs = df_backgr$mean_ir
mean_bluebabs = df_backgr$mean_blue

### calculate bc abs by removing the background

df_aae_raw <- df_smooth |>
  filter(exp_type %in% c("waste_burning", "cooking", "vehicles"),
         ir_babs  > mean_irbabs,
         blue_babs  > mean_bluebabs) |>
  mutate(ir_babs_bg_corr  = ir_babs  - mean_irbabs,
         blue_babs_bg_corr  = blue_babs - mean_bluebabs)

### calculate mean absorption of each experiment

#### select the aae experiments from data structure

data_structure_aae <- data_structure |>
  filter(exp_type %in% c("waste_burning", "cooking", "vehicles"))

## calculate aae of each experiment

aae <- df_aae_raw |>
  group_by(id, emission_source, exp_type) |>
  summarise(across(c(blue_babs, ir_babs, ir_babs_bg_corr, blue_babs_bg_corr), mean))|>
  mutate(aae_wo_bg_corr = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5])),
         aae_bg_corr = -log(blue_babs_bg_corr/ir_babs_bg_corr)/log((parameters$wavelength[2])/(parameters$wavelength[5])))|>
  arrange(by = exp_type)

## calculate aae mean and sd for each emission source

aae_summary <- aae |>
  group_by(emission_source) |>
  summarise(mean_aae_wo_bg_corr = mean(aae_wo_bg_corr),
            sd_aae_wo_bg_corr = sd(aae_wo_bg_corr),
            mean_aae_bg_corr = mean(aae_bg_corr),
            sd_aae_bg_corr = sd(aae_bg_corr),
            count = n())


# bc from bb and ff calculation ------------------------------------------

## using aethalometer model

### aae from fossil fuel (ff)

#### we selected diesel vehicles as they were pure ff
#### however, the plastics that we used, have wrappers around

aae_ff <- aae_summary |>
  filter(emission_source == "Diesel pickup-truck") |>
  select(mean_aae_bg_corr)

### aae from biomass burning (bb)

#### we selected garden waste as they were pure biomass
#### however, the wood experiments were done where people cook
#### and may contain contamination. also, the cardboard that
#### we burnt, contains plastic films, and cannot represent pure
#### biomass

aae_bb <- aae_summary |>
  filter(emission_source == "Garden waste") |>
  select(mean_aae_bg_corr)

### calculate bc from ff and bb

df_main <- df_smooth |>
  mutate(bc_ff = ir_bcc*(1/(1-((1-(ir_babs/blue_babs)*((parameters$wavelength[5]/parameters$wavelength[2])^aae_ff$mean_aae_bg_corr))/(1-(ir_babs/blue_babs)*((parameters$wavelength[5]/parameters$wavelength[2])^aae_bb$mean_aae_bg_corr))))),
         bc_ff = ifelse(bc_ff < 0, 0, bc_ff),
         bc_bb = ir_bcc*(1/(1-((1-(ir_babs/blue_babs)*((parameters$wavelength[5]/parameters$wavelength[2])^aae_bb$mean_aae_bg_corr))/(1-(ir_babs/blue_babs)*((parameters$wavelength[5]/parameters$wavelength[2])^aae_ff$mean_aae_bg_corr))))),
         bc_bb = ifelse(bc_bb < 0, 0, bc_bb),
         bc_bb = ifelse(bc_ff == 0, ir_bcc, bc_bb),
         bc_ff = ifelse(bc_bb == 0, ir_bcc, bc_ff),
         ratio_check = (bc_ff + bc_bb)/ir_bcc,
         bc_bb_percent = bc_bb/ir_bcc*100)

## using k-means

#### three clusters of aae were created using mobile
#### monitoring data. the three clusters should represent
#### dominant ff, dominant bb, mix of both

### prepare dataset to find clusters

df_k_means <-  df_main |>
  filter(exp_type == "mobile_monitoring") |>
  filter_all(all_vars(!is.infinite(.))) |>
  left_join(id_mm, by = "id") |>
  mutate(aae = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5]))) |>
  filter_all(all_vars(!is.infinite(.))) |>
  filter(!time_of_day %in% "Morning") |>
  select(aae) |>
  drop_na()

### perform k-means clustering

k <- 3

result <- kmeans(df_k_means$aae, centers = k)

cluster_centers <- tapply(df_k_means$aae, result$cluster, mean)

### calculate the range of each cluster ('aae' max - 'aae' min)

cluster_ranges <- tapply(df_k_means$aae, result$cluster, function(cluster_aae) {
  max_aae <- max(cluster_aae)
  min_aae <- min(cluster_aae)
  cluster_range <- cbind(max_aae, min_aae)
  return(cluster_range)
})

unlist(cluster_ranges)

## the clusters interpretation is - 25% of ff means ff dominating
## and vice-versa
# Create a new variable 'aae_range' based on 'aae'

# data <- df_k_means %>%
#   mutate(aae_range = case_when(
#     aae >= 0 & aae <= 1.328 ~ "ff_dominant",
#     aae > 1.328 & aae <= 1.759 ~ "mixed",
#     aae > 1.759 ~ "bb_dominant"
#   )) |>
#   drop_na()  |>
# group_by(settlement_id, aae_range)  |>
#   summarize(observation_count = n(),
#             bc_ff = mean(ir_bcc),
#             bc_bb = mean(ir_bcc))


# create processed data files ---------------------------------------------

## aae experiments data

df_aae_exp <- df_aae_raw

## aae calculated for each aae experiment

aae_calculated <- aae

## mobile monitoring (mm) data

df_mm <- df_smooth |>
  filter(exp_type == "mobile_monitoring") |>
  left_join(id_mm, by = "id")


### burning events during mm

## personal monitoring (pm) data

df_pm <- df_smooth |>
  filter(exp_type == "personal_monitoring") |>
  left_join(id_pm, by = "id")

### burning events during pm

## stationary monitoring data

df_sm <- df_smooth |>
  filter(exp_type == "stationary_monitoring") |>
  left_join(id_sm, by = "serial_number")

## data quality check

### sensor collocation

df_collocation <- df_smooth |>
  filter(exp_type == "sensor_collocation")

### reduction in negative values after cma

df_negative_count_cma <- df_negative_count

##### for Lars:
##### following files needs to created

# 1. df_aae_exp
# 2. aae_calculated
# 3. df_mm
# 4. df_pm
# 5. df_sm
# 6. df_collocation
# 7. df_negative_count



