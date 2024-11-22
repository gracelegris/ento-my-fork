# ==========================================================================================================================================
# Script Name: Ento Analysis and Plots
# Author: Eniola Bamgboye, ebamgboye@luc.edu
# Edited: Grace Legris (gracebea@gmail.com), 11/21/24
# Purpose: Prepare CDC and PSC Wet Season Data
# ==========================================================================================================================================

# load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(ggrepel)

# set user directories
user <- Sys.getenv("USERNAME")
drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
lu_dir <- file.path(drive, "Documents")
lu_pdir <- file.path(drive, "Downloads")

user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
} else if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  NuDir <- file.path(Drive, "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan", "kano_ibadan_ento", "Osun-excel")
  WetData <- file.path(NuDir, "data", "nigeria", "kano_ibadan", "kano_ibadan_ento", "Wet Season Data_Ibadan")
  ResultDir <- file.path(NuDir, "projects/Manuscripts/ongoing/dry season entomology_manuscript/Grace/figures/plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan/kano_ibadan_shape_files")
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EntoDat <- file.path(NuDir, "data", "nigeria",  "kano_ibadan", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}


## =========================================================================================================================================
### CDC Data
## =========================================================================================================================================

# read and preprocess cdc data for the wet season
ento_cdc_w_all <- read_xlsx(file.path(WetData, "WET_SEASON_ENTO_COLLECTION_CDC_-_all_versions_-_labels_-_2024-08-12-21-20-44.xlsx")) %>%
  slice(-(1:3))

# filter and select relevant data for week 1
unique_cdc_wet1 <- ento_cdc_w_all %>%
  dplyr::filter(`Night of Data Collection` == 5, `Location of CDC` == "Outdoor")

ento_cdc_w1 <- unique_cdc_wet1 %>%
  select("Ward Name", "Settlement Type", "Household Code/Number",
         "_Household coordinates_latitude", "_Household coordinates_longitude") %>%
  mutate(type = "CDC")

# rename columns for consistency
colnames(ento_cdc_w1)[4:5] <- c("Latitude", "Longitude")

# filter and select relevant data for week 2
unique_cdc_wet2 <- ento_cdc_w_all %>%
  dplyr::filter(`Night of Data Collection` == 15, `Location of CDC` == "Outdoor")

ento_cdc_w2 <- unique_cdc_wet2 %>%
  select("Ward Name", "Settlement Type", "Household Code/Number",
         "_Household coordinates_latitude", "_Household coordinates_longitude") %>%
  mutate(type = "CDC2")

# rename columns for consistency
colnames(ento_cdc_w2)[4:5] <- c("Latitude", "Longitude")



## =========================================================================================================================================
### PSC Data
## =========================================================================================================================================

# read and preprocess psc data for the wet season
ento_psc_w_all <- read_xlsx(file.path(WetData, "WET_SEASON_ENTO_COLLECTION_PSC_-_all_versions_-_labels_-_2024-08-12-21-20-23.xlsx")) %>%
  slice(-(1:4)) %>%
  mutate(Anopheles_Caught = ifelse(`Total Number of Anopheles` > 0, "Yes", "No"))

# convert to spatial dataframe
ento_psc_w_all_df <- st_as_sf(ento_psc_w_all,
                              coords = c('_Household coordinates_longitude', '_Household coordinates_latitude'),
                              crs = 4326)

# select relevant columns and add type for psc data
ento_psc_w <- ento_psc_w_all %>%
  select("Ward Name", "Settlement Type", "Household Code/Number",
         "_Household coordinates_latitude", "_Household coordinates_longitude") %>%
  mutate(type = "PSC")

# rename columns for consistency
colnames(ento_psc_w)[4:5] <- c("Latitude", "Longitude")


## =========================================================================================================================================
### Combine CDC and PSC Data + Process
## =========================================================================================================================================

# combine cdc and psc data
ento_wet <- rbind(ento_cdc_w1, ento_cdc_w2, ento_psc_w)

# convert combined data to spatial dataframe
ento_wet_df <- st_as_sf(ento_wet, coords = c('Longitude', 'Latitude'), crs = 4326)

# read ibadan ward shapefiles
df_ib <- st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

# create a base plot for ibadan wards
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_text_repel(data = df_ib, aes(label = WardName, geometry = geometry), color = 'black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1) +
  map_theme() +
  labs(title = "Wards in Ibadan") +
  coord_sf()

# split ibadan shapefile into specific wards
df_ib_c <- df_ib %>% dplyr::filter(WardName == 'Challenge')
df_ib_a <- df_ib %>% dplyr::filter(WardName == 'Agugu')
df_ib_o <- df_ib %>% dplyr::filter(WardName == 'Olopomewa')

# save a shapefile for agugu ward
st_write(df_ib_a, file.path(lu_dir, "Agugu.shp"))

# ensure consistent coordinate reference systems
st_crs(df_ib_a) <- 4326
st_crs(df_ib_c) <- 4326
st_crs(ento_wet_df) <- 4326

# perform spatial intersections for specific wards
ento_wet_df_int <- st_intersection(ento_wet_df, df_ib)
ento_wet_df_int_a <- st_intersection(ento_wet_df, df_ib_a)
ento_wet_df_int_c <- st_intersection(ento_wet_df, df_ib_c)

# fill missing entries in agugu ward data if necessary
if (is.na(ento_wet_df_int_a[206, 2]) || df[206, 2] == "") {
  ento_lav_w_all[206, 2] <- "Slum"
}

## =========================================================================================================================================
### Plots - Locations of Wet Season Collections in Agugu and Challenge
## =========================================================================================================================================

# plot locations for wet season collections in agugu
ggplot(df_ib_a) +
  geom_sf(fill = "NA") +
  geom_point(data = ento_wet_df_int_a, aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`),
             stat = "sf_coordinates") +
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen", PSC = "tomato", LAV = "plum")) +
  scale_shape_manual(values = c(Formal = 16, Informal = 17, Slum = 14)) +
  geom_text_repel(data = df_ib_a, aes(label = WardName, geometry = geometry), color = 'black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  labs(title = "Sites for Wet Season Ento (Agugu)") +
  coord_sf()

## plot location by type of collection
## cdc
ggplot(df_ib_a) +
  geom_sf(fill = "NA") +
  geom_point(
    data = dplyr::filter(ento_wet_df_int_a, type == "CDC" | type == "CDC2"),
    aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`),
    stat = "sf_coordinates"
  ) +
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen")) +
  scale_shape_manual(values = c(Formal = 16, Informal = 17, Slum = 14)) +
  geom_text_repel(
    data = df_ib_a,
    aes(label = `WardName`, geometry = geometry),
    color = 'black',
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2.5,
    force = 1,
    max.overlaps = Inf
  ) +
  guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Sites for Wet Season CDC Ento Collection (Agugu)") +
  coord_sf()

## plot location (challenge)
ggplot(df_ib_c) +
  geom_sf(fill = NA) +
  geom_point(
    data = ento_wet_df_int_c,
    aes(geometry = geometry, size = 0.01, alpha = 0.001, col = `type`, shape = `Settlement.Type`),
    stat = "sf_coordinates"
  ) +
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen", PSC = "tomato", LAV = "plum")) +
  scale_shape_manual(values = c(Formal = 16, Informal = 17, Slum = 14)) +
  geom_text_repel(
    data = df_ib_c,
    aes(label = `WardName`, geometry = geometry),
    color = 'black',
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2.5,
    force = 1,
    max.overlaps = Inf
  ) +
  guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Sites for Wet Season Ento (Challenge)") +
  coord_sf()

## plot location by type of collection
## cdc
ggplot(df_ib_c) +
  geom_sf(fill = "NA") +
  geom_point(
    data = dplyr::filter(ento_wet_df_int_c, type == "CDC" | type == "CDC2"),
    aes(geometry = geometry, size = 0.05, alpha = 0.01, col = `type`, shape = `Settlement.Type`),
    stat = "sf_coordinates"
  ) +
  scale_color_manual(values = c(CDC = "navyblue", CDC2 = "seagreen")) +
  scale_shape_manual(values = c(Formal = 16, Informal = 17, Slum = 14)) +
  geom_text_repel(
    data = df_ib_c,
    aes(label = `WardName`, geometry = geometry),
    color = 'black',
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 2.5,
    force = 1,
    max.overlaps = Inf
  ) +
  guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Sites for Wet Season CDC Ento Collection (Challenge)") +
  coord_sf()

## =========================================================================================================================================
### CDC Data - Processing
## =========================================================================================================================================

# rename vars in cdc wet df for clarity
colnames(ento_cdc_w_all) <- c(
  "start", "end", "date", "city", "ward_name", "settlement_type", "ea_name_code", 
  "household_code_number", "cdc_location", "night_collection", 
  "cdc_6_7pm", "6_7_pm", "rainfall_6_7pm", "humidity_6_7pm", "temperature_6_7pm", 
  "total_anopheles_6_7pm", "total_gambiae_6_7pm", "total_funestus_6_7pm", 
  "total_culicine_6_7pm", "cdc_7_8pm", "7_8_pm", "rainfall_7_8pm", 
  "humidity_7_8pm", "temperature_7_8pm", "total_anopheles_7_8pm", 
  "total_gambiae_7_8pm", "total_funestus_7_8pm", "total_culicine_7_8pm", 
  "cdc_8_9pm", "8_9_pm", "rainfall_8_9pm", "humidity_8_9pm", 
  "temperature_8_9pm", "total_anopheles_8_9pm", "total_gambiae_8_9pm", 
  "total_funestus_8_9pm", "total_culicine_8_9pm", "cdc_9_10pm", 
  "9_10_pm", "rainfall_9_10pm", "humidity_9_10pm", "temperature_9_10pm", 
  "total_anopheles_9_10pm", "total_gambiae_9_10pm", "total_funestus_9_10pm", 
  "total_culicine_9_10pm", "cdc_10_11pm", "10_11_pm", 
  "rainfall_10_11pm", "humidity_10_11pm", "temperature_10_11pm", 
  "total_anopheles_10_11pm", "total_gambiae_10_11pm", "total_funestus_10_11pm", 
  "total_culicine_10_11pm", "cdc_11_12am", "11_12_am", 
  "rainfall_11_12am", "humidity_11_12am", "temperature_11_12am", 
  "total_anopheles_11_12am", "total_gambiae_11_12am", "total_funestus_11_12am", 
  "total_culicine_11_12am", "cdc_12_1am", "12_2_am", 
  "rainfall_12_1am", "humidity_12_1am", "temperature_12_1am", 
  "total_anopheles_12_1am", "total_gambiae_12_1am", "total_funestus_12_1am", 
  "total_culicine_12_1am", "cdc_1_2am", "1_2_am", 
  "rainfall_1_2am", "humidity_1_2am", "temperature_1_2am", 
  "total_anopheles_1_2am", "total_gambiae_1_2am", "total_funestus_1_2am", 
  "total_culicine_1_2am", "cdc_2_3am", "2_3_am", 
  "rainfall_2_3am", "humidity_2_3am", "temperature_2_3am", 
  "total_anopheles_2_3am", "total_gambiae_2_3am", "total_funestus_2_3am", 
  "total_culicine_2_3am", "cdc_3_4am", "3_4_am", 
  "rainfall_3_4am", "humidity_3_4am", "temperature_3_4am", 
  "total_anopheles_3_4am", "total_gambiae_3_4am", "total_funestus_3_4am", 
  "total_culicine_3_4am", "cdc_4_5am", "4_5_am", 
  "rainfall_4_5am", "humidity_4_5am", "temperature_4_5am", 
  "total_anopheles_4_5am", "total_gambiae_4_5am", "total_funestus_4_5am", 
  "total_culicine_4_5am", "cdc_5_6am", "5_6_am", 
  "rainfall_5_6am", "humidity_5_6am", "temperature_5_6am", 
  "total_anopheles_5_6am", "total_gambiae_5_6am", "total_funestus_5_6am", 
  "total_culicine_5_6am", "household_coordinates", "latitude", 
  "longitude", "altitude", "precision", "household_picture", 
  "picture_url", "data_collector", "comments", "collection_time", 
  "name_anopheles_1", "name_anopheles_2", "time_collection_1", 
  "name_anopheles_1_2", "name_anopheles_2_2", "time_collection_2", 
  "name_anopheles_1_3", "name_anopheles_2_3", "time_collection_3", 
  "name_anopheles_1_4", "name_anopheles_2_4", "time_collection_4", 
  "name_anopheles_1_5", "name_anopheles_2_5", "_id", "_uuid", 
  "_submission_time", "_validation_status", "_notes", "_status", 
  "_submitted_by", "_version", "_tags", "_index"
)

# add up mosquito counts over the night for each species
ento_cdc_w_all$total_anopheles <- cdc_wet$total_anopheles_6_7pm + cdc_wet$total_anopheles_7_8pm + cdc_wet$total_anopheles_8_9pm +
                           cdc_wet$total_anopheles_9_10pm + cdc_wet$total_anopheles_10_11pm + cdc_wet$total_anopheles_11_12am +
                           cdc_wet$total_anopheles_12_1am + cdc_wet$total_anopheles_1_2am + cdc_wet$total_anopheles_2_3am +
                           cdc_wet$total_anopheles_3_4am + cdc_wet$total_anopheles_4_5am + cdc_wet$total_anopheles_5_6am

ento_cdc_w_all$total_gambiae <- cdc_wet$total_gambiae_6_7pm + cdc_wet$total_gambiae_7_8pm + cdc_wet$total_gambiae_8_9pm +
                         cdc_wet$total_gambiae_9_10pm + cdc_wet$total_gambiae_10_11pm + cdc_wet$total_gambiae_11_12am +
                         cdc_wet$total_gambiae_12_1am + cdc_wet$total_gambiae_1_2am + cdc_wet$total_gambiae_2_3am +
                         cdc_wet$total_gambiae_3_4am + cdc_wet$total_gambiae_4_5am + cdc_wet$total_gambiae_5_6am

ento_cdc_w_all$total_funestus <- cdc_wet$total_funestus_6_7pm + cdc_wet$total_funestus_7_8pm + cdc_wet$total_funestus_8_9pm +
                          cdc_wet$total_funestus_9_10pm + cdc_wet$total_funestus_10_11pm + cdc_wet$total_funestus_11_12am +
                          cdc_wet$total_funestus_12_1am + cdc_wet$total_funestus_1_2am + cdc_wet$total_funestus_2_3am +
                          cdc_wet$total_funestus_3_4am + cdc_wet$total_funestus_4_5am + cdc_wet$total_funestus_5_6am

ento_cdc_w_all$total_culicine <- cdc_wet$total_culicine_6_7pm + cdc_wet$total_culicine_7_8pm + cdc_wet$total_culicine_8_9pm +
                          cdc_wet$total_culicine_9_10pm + cdc_wet$total_culicine_10_11pm + cdc_wet$total_culicine_11_12am +
                          cdc_wet$total_culicine_12_1am + cdc_wet$total_culicine_1_2am + cdc_wet$total_culicine_2_3am +
                          cdc_wet$total_culicine_3_4am + cdc_wet$total_culicine_4_5am + cdc_wet$total_culicine_5_6am

# save variables of interest in new df for plotting (no environmental or time data)
cdc_wet <- ento_cdc_w_all %>%
  dplyr::select(date, city, ward_name, settlement_type, household_code_number, cdc_location, total_anopheles, total_gambiae, total_funestus, total_culicine)

# save this formatted df
write.xlsx(cdc_wet, file.path(WetData, "cdc_wet_no_env_time.xlsx"))

## cdc
# load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

ento_cdc_working_df <- ento_cdc_w_all %>%
  dplyr::select(
    "Ward Name", "Settlement Type",
    "EA Name and Code", "Household Code/Number",
    "Location of CDC", "Night of Data Collection",
    "6-7pm(CDC Collection)", "6-7pm", "Total Number of Anopheles...16",
    "7-8pm(CDC Collection)", "7-8pm", "Total Number of Anopheles...25",
    "8-9pm(CDC Collection)", "8-9pm", "Total Number of Anopheles...34",
    "9-10pm(CDC Collection)", "9-10pm", "Total Number of Anopheles...43",
    "10-11pm(CDC Collection)", "10-11pm", "Total Number of Anopheles...52",
    "11-12am(CDC Collection)", "11-12am", "Total Number of Anopheles...61",
    "12-1am(CDC Collection)", "12-1am", "Total Number of Anopheles...70",
    "1-2am(CDC Collection)", "1-2am", "Total Number of Anopheles...79",
    "2-3am(CDC Collection)", "2-3am", "Total Number of Anopheles...88",
    "3-4am(CDC Collection)", "3-4am", "Total Number of Anopheles...97",
    "4-5am(CDC Collection)", "4-5am", "Total Number of Anopheles...106",
    "5-6am(CDC Collection)", "5-6am", "Total Number of Anopheles...115",
    "_Household coordinates_latitude", "_Household coordinates_longitude", "_index"
  )

## generate variable for hhs where anopheles was caught
columns_to_check <- c(
  "Total Number of Anopheles...16",
  "Total Number of Anopheles...25",
  "Total Number of Anopheles...34",
  "Total Number of Anopheles...43",
  "Total Number of Anopheles...52",
  "Total Number of Anopheles...61",
  "Total Number of Anopheles...70",
  "Total Number of Anopheles...79",
  "Total Number of Anopheles...88",
  "Total Number of Anopheles...97",
  "Total Number of Anopheles...106",
  "Total Number of Anopheles...115"
)

ento_cdc_working_dfn <- ento_cdc_working_df %>%
  rowwise() %>%
  mutate(Anopheles_Caught = if_else(any(c_across(all_of(columns_to_check)) > 0), "Yes", "No")) %>%
  ungroup()

ento_cdc_working_dfng <- ento_cdc_working_dfn %>%
  group_by(`Ward Name`, `Location of CDC`) %>%
  summarise(
    tot67 = sum(as.numeric(`Total Number of Anopheles...16`), na.rm = TRUE),
    tot78 = sum(as.numeric(`Total Number of Anopheles...25`), na.rm = TRUE),
    tot89 = sum(as.numeric(`Total Number of Anopheles...34`), na.rm = TRUE),
    tot910 = sum(as.numeric(`Total Number of Anopheles...43`), na.rm = TRUE),
    tot1011 = sum(as.numeric(`Total Number of Anopheles...52`), na.rm = TRUE),
    tot1112 = sum(as.numeric(`Total Number of Anopheles...61`), na.rm = TRUE),
    tot121 = sum(as.numeric(`Total Number of Anopheles...70`), na.rm = TRUE),
    tot12 = sum(as.numeric(`Total Number of Anopheles...79`), na.rm = TRUE),
    tot23 = sum(as.numeric(`Total Number of Anopheles...88`), na.rm = TRUE),
    tot34 = sum(as.numeric(`Total Number of Anopheles...97`), na.rm = TRUE),
    tot45 = sum(as.numeric(`Total Number of Anopheles...106`), na.rm = TRUE),
    tot56 = sum(as.numeric(`Total Number of Anopheles...115`), na.rm = TRUE)
  )

ento_cdc_long <- ento_cdc_working_dfng %>%
  pivot_longer(
    cols = c(
      `tot67`, `tot78`, `tot89`, `tot910`, `tot1011`, `tot1112`,
      `tot121`, `tot12`, `tot23`, `tot34`, `tot45`, `tot56`
    ), # list all relevant columns here
    names_to = "Time",
    values_to = "Number_of_Anopheles"
  )

## group by ward name, location, and time, and then summarize the total number of anopheles
ento_cdc_summary <- ento_cdc_long %>%
  group_by(`Ward Name`, `Location of CDC`, Time) %>%
  summarize(Total_Anopheles = sum(Number_of_Anopheles)) %>%
  ungroup()


## CDC Anopheles
ento_cdc_w_all_df <- ento_cdc_w_all %>%
  mutate(Total_Anopheles_6_7pm = sum(`Total Number of Anopheles...16`))

# calculate total anopheles for each hour
Total_Anopheles_6_7pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...16`, na.rm = TRUE)
Total_Anopheles_7_8pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...25`, na.rm = TRUE)
Total_Anopheles_8_9pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...34`, na.rm = TRUE)
Total_Anopheles_9_10pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...43`, na.rm = TRUE)
Total_Anopheles_10_11pm <- sum(ento_cdc_w_all$`Total Number of Anopheles...52`, na.rm = TRUE)
Total_Anopheles_11_12am <- sum(ento_cdc_w_all$`Total Number of Anopheles...61`, na.rm = TRUE)
Total_Anopheles_12_1am <- sum(ento_cdc_w_all$`Total Number of Anopheles...70`, na.rm = TRUE)
Total_Anopheles_1_2am <- sum(ento_cdc_w_all$`Total Number of Anopheles...79`, na.rm = TRUE)
Total_Anopheles_2_3am <- sum(ento_cdc_w_all$`Total Number of Anopheles...88`, na.rm = TRUE)
Total_Anopheles_3_4am <- sum(ento_cdc_w_all$`Total Number of Anopheles...97`, na.rm = TRUE)
Total_Anopheles_4_5am <- sum(ento_cdc_w_all$`Total Number of Anopheles...106`, na.rm = TRUE)
Total_Anopheles_5_6am <- sum(ento_cdc_w_all$`Total Number of Anopheles...115`, na.rm = TRUE)

## PSC Anopheles
# remove unnecessary rows
ento_psc_w_all <- slice(ento_psc_w_all, -(11))

# modify the 'settlement_type' column based on the 'Ward' column
ento_psc_w_all <- ento_psc_w_all %>%
  mutate(`Settlement Type` = if_else(
    `Ward Name` == "Agugu" & `Settlement Type` == "Informal",
    "Slum",
    `Settlement Type`
  ))

# convert to spatial dataframe
ento_psc_wet_df <- sf::st_as_sf(
  ento_psc_w_all,
  coords = c('_Household coordinates_longitude', '_Household coordinates_latitude'),
  crs = 4326
)

# group data by settlement type and ward name
psc_ib <- ento_psc_w_all %>%
  group_by(`Settlement Type`, `Ward Name`) %>%
  summarise(total_mosquitoes = sum(`Total Number of Anopheles`, na.rm = TRUE)) %>%
  ungroup()

psc_hh_ib <- ento_psc_w_all %>%
  group_by(`Ward Name`, `Household Code/Number`, Anopheles_Caught) %>%
  summarise(total_mosquitoes = sum(`Total Number of Anopheles`, na.rm = TRUE)) %>%
  ungroup()

psc_hh_ib_sum <- psc_hh_ib %>%
  dplyr::filter(Anopheles_Caught == "Yes")

# plot number of anopheles by site
p1 <- ggplot(data = psc_ib, aes(
  x = `Ward Name`, y = total_mosquitoes,
  group = `Settlement Type`, colour = `Settlement Type`
)) +
  geom_point() +
  labs(
    y = "Total number of anopheles mosquitoes caught",
    x = "Location of collection (Ibadan)"
  ) +
  ggtitle("Anopheles mosquito collected through PSC, July 2024") +
  geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12)) +
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0, 100)

# plot location of households with anopheles (PSC)
ggplot(df_ib_a) +
  geom_sf(fill = NA) +
  geom_point(
    data = dplyr::filter(ento_psc_wet_df, `Ward Name` == "Agugu", Anopheles_Caught == "Yes"),
    aes(geometry = geometry, size = `Total Number of Anopheles`, alpha = 0.001, col = `Settlement Type`),
    stat = "sf_coordinates"
  ) +
  scale_color_manual(values = c(Formal = "navyblue", Informal = "tomato", Slum = "plum")) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Household in Agugu with Anopheles for Wet Season Ento (PSC)") +
  coord_sf()

# check location of new CDC households
cdc_new <- data.frame(
  latitude = c(7.381532, 7.381533, 7.380827, 7.381327),
  longitude = c(3.920199, 3.919552, 3.919823, 3.920117)
)

cdc_new_df <- sf::st_as_sf(cdc_new, coords = c('longitude', 'latitude'), crs = 4326)