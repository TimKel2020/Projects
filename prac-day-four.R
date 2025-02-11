library(tidyverse)
library(nycflights13)
library(tibble)
library(dplyr)
# stats::filter()
# stats::lag()

# Questions
# Q1
data(flights, package = "nycflights13")
dplyr::glimpse(flights) # displaying data to the side
flights |>
  head() |>
  as_tibble()

# flights
# View(flights)
# print(flights, width = Inf) # to show all columns


# Q2 - Rewriting code using dplyr and the pipe
flight1 <- flights |> 
  filter(month == 1)
carrier_vec <- flight1 |> 
  distinct(carrier) |> 
  pull(carrier)
carrier_dist_vec_mean <- carrier_vec |>
  length() |>
  numeric()
carrier_dist_vec_sd<- carrier_vec |>
  length() |>
  numeric()
# carrier_dist_vec_mean <- numeric(length(carrier_vec))
# carrier_dist_vec_sd <- numeric(length(carrier_vec))

for (i in seq_along(carrier_vec)) {
  carrier_dist_vec_mean[i] <- flight1 |>
    filter(carrier == carrier_vec[i]) |>
    summarise(mean_distance = mean(distance, na.rm = TRUE)) |>
    pull(mean_distance)
  
  carrier_dist_vec_sd[i] <- flight1 |>
    filter(carrier == carrier_vec[i]) |>
    summarise(sd_distance = sd(distance, na.rm = TRUE)) |>
    pull(sd_distance)
}

carrier_vec
carrier_dist_vec_mean
carrier_dist_vec_sd

dist_tbl <- flights |>
  filter(month == 1) |>
  group_by(carrier) |>
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE)
  ) |>
  arrange(mean_distance)

dist_tbl


# Q3
# UtilsDataRSV::view_cols(flight1)
# Zero for: YV, F9, AS, HA
# NA for: OO
distance_table <- flight1 |>
  group_by(carrier) |>
  select(carrier, distance) |>
  distinct() |>
  count(name = 'No. of distinct distances') |>
  arrange(carrier)

distance_table <- flight1 |>
  group_by(carrier) |>
  summarize(
    `No. of distinct distances` = n_distinct(distance),
    `Total number of distances` = n()
  ) |>
  arrange(carrier)

distance_table 

# The standard deviation is NA when all values in a group are NA or if there's only one data point in the group.
# The standard deviation is 0 when all the values in the group are identical. 


# Q4
delay_table <- flights |>
  group_by(month, carrier) |>
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  pivot_wider(names_from = carrier, values_from = avg_dep_delay)

delay_table

# Q5
proportion <- flights |>
  filter(dep_delay > 0, arr_delay <= 0) |>
  summarise(proportion = n() / nrow(flights))

proportion


# Q6
## i 
mult_airlines <- flights |>
  group_by(origin, dest) |>
  filter(n_distinct(carrier) > 1)

mult_airlines

## ii
avg_delay <- mult_airlines |>
  group_by(origin, dest, carrier) |>
  summarise(avg_arrival_delay = mean(arr_delay, na.rm = TRUE)) |>
  ungroup()

avg_delay


## iii
performers <- avg_delay |>
  summarise(
    best_avg_delay = carrier[which.min(avg_arrival_delay)],
    worst_avg_delay = carrier[which.max(avg_arrival_delay)]
  )

performers

## iv
route_performance_diff <- avg_delay |>
  group_by(origin, dest) |>
  summarise(
    diff = max(avg_arrival_delay) - min(avg_arrival_delay)
  ) |>
  arrange(desc(diff))

route_performance_diff

route_performance_diff[1, ]

## v


# Q7
prac_df <- structure(list(id = c("id_1", "id_2", "id_3", "id_4", "id_5", 
"id_6", "id_7", "id_8", "id_9", "id_10", "id_11", "id_12", "id_13", 
"id_14", "id_15", "id_16", "id_17", "id_18", "id_19", "id_20", 
"id_21", "id_22", "id_23", "id_24", "id_25", "id_26", "id_27", 
"id_28", "id_29", "id_30", "id_31", "id_32", "id_33", "id_34", 
"id_35", "id_36", "id_37", "id_38", "id_39", "id_40", "id_41", 
"id_42", "id_43", "id_44", "id_45", "id_46", "id_47", "id_48", 
"id_49", "id_50"), age = c(50L, 34L, 70L, 33L, 22L, 61L, 69L, 
73L, 62L, 56L, 71L, 33L, 73L, 44L, 45L, 46L, 24L, 70L, 46L, 76L, 
47L, 76L, 28L, 48L, 54L, 27L, 45L, 26L, 61L, 28L, 38L, 55L, 33L, 
36L, 62L, 58L, 72L, 31L, 34L, 51L, 61L, 64L, 26L, 28L, 60L, 29L, 
42L, 46L, 79L, 72L), gender = c("male", "male", "male", "female", 
"female", "male", "female", "male", "male", "female", "female", 
"male", "male", "female", "male", "male", "male", "male", "female", 
"male", "male", "male", "male", "female", "femal", "male", "female", 
"female", "female", "female", "male", "female", "female", "female", 
"male", "male", "female", "male", "female", "female", "male", 
"female", "female", "male", "male", "female", "male", "male", 
"male", "female"), height = c(174.4, 197.7, 174.1, 194.5, NA, 
180.4, 170.5, 157.4, 196.8, 165.1, 153, 197.4, 186, 157.1, 177.5, 
197.7, 179.3, 170.2, 182.4, NA, 165.4, 161, 168.5, 199.2, 157.7, 
154.6, 157.1, 184.5, 181, 194.6, 183.6, 186.9, 176.1, 183, 191.1, 
189.3, 199, 172, 165.6, 170.5, 150.5, 159.2, 192.1, 161.6, 162, 
153.8, 162.3, 186.6, 192.4, 174.9), weight = c(69.4, 62.3, 55.6, 
69.5, 78.6, 60.8, 72.2, 60.9, 75.1, 67.7, 82.5, 68.7, 67.8, 76.7, 
87, 61.1, 70.6, 63.3, 81.5, 59.2, 93.2, 87.3, 83.4, 80.9, 68.6, 
76.5, 93.7, 79.1, 92, 65.6, 85.4, 63.3, 79.7, 74.1, 63.3, 78.2, 
95.7, 95.1, 63.7, 66.1, 99.3, 81, 96.9, 73.3, 70.3, 83, 57.6, 
78.6, 61.9, 98.1), blood_type = c("O", "A", "O", "O", "B", "AB", 
"O", "O", "O", "AB", "A", "O", "O", "O", "B", "A", "B", "AB", 
"O", "AB", "A", "AB", "O", "B", "A", "A", "B", "AB", "A", "B", 
"B", "A", "O", "O", "O", "B", "O", "A", "A", "B", "A", "O", "AB", 
"A", "A", "O", "O", "B", "A", "O"), disease_status = c("diseased", 
"healthy", "healthy", "healthy", "healthy", "healthy", "diseased", 
"healthy", "diseased", "Healthy", "diseased", "healthy", "diseased", 
"healthy", "diseased", "healthy", "healthy", "healthy", "healthy", 
"healthy", "healthy", "diseased", "healthy", "diseased", "healthy", 
"healthy", "healthy", "healthy", "diseased", "diseased", "healthy", 
"healthy", "healthy", "diseased", "diseased", "diseased", "healthy", 
"diseased", "healthy", "healthy", "healthy", "healthy", "healthy", 
"diseased", "diseased", "diseased", "healthy", "healthy", "diseased", 
"diseased"), cholesterol = c(228, 223, 213, 198, 166, 151, 195, 
199, 189, 196, 221, 156, 185, 230, 234, 174, 185, 236, 235, 180, 
165, 220, 160, 153, 250, 153, 184, 242, 212, 179, 224, 233, 181, 
199, 220, 214, 214, 248, 191, 162, 203, 173, 199, 187, 248, 189, 
173, 212, 164, 247), glucose = c(96, 78, 101, 119, 103, 91, 86, 
NA, 77, 80, 115, 85, 88, 109, NA, 71, 90, 94, 91, 87, 113, 93, 
97, 118, 109, 80, 85, 119, 99, 108, 89, 108, 97, 116, 79, 84, 
75, 81, 119, NA, 106, 109, 75, 82, 84, 75, 76, 120, 119, 77), 
    smoker = c("yes", "yes", "yes", "yes", "no", "yes", "no", 
    "yes", "no", "no", "no", "no", "no", "yes", "no", "yes", 
    "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", 
    "no", "yes", "yes", "yes", "no", "no", "yes", "no", "yes", 
    "no", "yes", "no", "yes", "yes", "yes", "no", "no", "yes", 
    "no", "no", "no", "no", "no", "no", "yes"), exercise = c("occasional", 
    "regular", "occasional", "regular", "none", "occasional", 
    "regular", "none", "occasional", "none", "occasional", "none", 
    "none", "regular", "occasional", "none", "regular", "regular", 
    "none", "occasional", "none", "occasional", "occasional", 
    "occasional", "regular", "occasional", "regular", "regular", 
    "regular", "occasional", "occasional", "none", "none", "regular", 
    "occasional", "occasional", "none", "none", "none", "none", 
    "occasional", "regular", "regular", "none", "regular", "occasional", 
    "occasional", "none", "occasional", "regular"), income = c(84820L, 
    81547L, 22588L, 72490L, 74533L, 25338L, 41469L, 57315L, 63629L, 
    88662L, 62615L, 56261L, 58499L, 82232L, 77584L, 77275L, 38468L, 
    54510L, 91326L, 78611L, 31402L, 29586L, 21441L, 58269L, 84173L, 
    88295L, 37940L, 43750L, 69750L, 92356L, 82518L, 91455L, 68866L, 
    51178L, 68275L, 27689L, 35418L, 81318L, 62405L, 86851L, 25654L, 
    47553L, 74474L, 51409L, 22607L, 55360L, 96351L, 21516L, 41927L, 
    55810L), education = c("master", "bachelor", "PhD", "master", 
    "bachelor", "highschool", "PhD", "highschool", "PhD", "PhD", 
    "bachelor", "highschool", "master", "bachelor", "PhD", "PhD", 
    "PhD", "bachelor", "master", "highschool", "PhD", "highschool", 
    "bachelor", "master", "highschool", "highschool", "master", 
    "master", "bachelor", "PhD", "highschool", "PhD", "master", 
    "master", "master", "PhD", "highschool", "master", "master", 
    "highschool", "bachelor", "highschool", "bachelor", "PhD", 
    "bachelor", "highschool", "master", "highschool", "bachelor", 
    "bachelor"), region = c("North", "South", "North", "West", 
    "North", "West", "South", "South", "West", "South", "West", 
    "South", "West", "East", "North", "West", "North", "North", 
    "West", "North", "East", "West", "South", "North", "North", 
    "East", "East", "North", "North", "West", "South", "West", 
    "West", "East", "West", "North", "West", "North", "East", 
    "North", "West", "South", "South", "East", "North", "West", 
    "West", "East", "North", "East"), marital_status = c("divorced", 
    "single", "divorced", "divorced", "divorced", "divorced", 
    "divorced", "married", "divorced", "married", "divorced", 
    "widowed", "married", "single", "widowed", "widowed", "single", 
    "divorced", "widowed", "widowed", "single", "married", "single", 
    "married", "widowed", "married", "single", "single", "widowed", 
    "married", "widowed", "divorced", "single", "married", "single", 
    "widowed", "widowed", "married", "widowed", "divorced", "married", 
    "married", "divorced", "single", "married", "widowed", "divorced", 
    "divorced", "single", "divorced")), row.names = c(NA, -50L
), class = c("tbl_df", "tbl", "data.frame"))

UtilsDataRSV::view_cols(prac_df)
# disease_status contains three unique entries ("healthy", "diseased", and the typo "Healthy")
# gender contains three unique entries ("male", "female", and the typo "femal")
# height and glucose has missing values - NA
# 50 unique id's

