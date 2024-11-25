# ==========================================================================================================================================
# Script Name: Ento Analysis and Plots
# Author: Eniola Bamgboye, ebamgboye@luc.edu
# Edited: Grace Legris (gracebea@gmail.com), 11/20/24
# Purpose: Conduct analyses of data in Ibadan, compare wet/dry season data, create figures for manuscript
# ==========================================================================================================================================

# clear current workspace
rm(list=ls())

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

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
### Required Libraries and Functions
## =========================================================================================================================================

# load necessary libraries
library(readxl)
library(sf)
library(vcd)
library(ggplot2)
#library(tmap)
library(ggrepel)
library(tidyverse)
library(geometry)
library(dplyr)
#library(rgdal)
library(fun)
library(patchwork)
#library(rgeos)
#library(maptools)
library(purrr)
library(DescTools)
library(conflicted)
library(stringr)
library(readxl)
library(dplyr)
library(gridExtra)
library(sf)
library(ggrepel)

# define a custom theme for maps
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

# function to create a ggplot object for geographic data
con_gplot <-function(df,fill,label){
  ggplot()+
    geom_sf(data=df, mapping=aes(fill = !!fill)) +
    map_theme() +
    geom_text_repel(
      data = df,
      aes(label = !!label, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

# define a custom theme for manuscript-style plots
theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Excel files
## -----------------------------------------------------------------------------------------------------------------------------------------

# list all Excel files in the specified directory
files <- list.files(
  path = EntoDat,
  pattern = ".xlsx",
  full.names = TRUE,
  recursive = FALSE # do not search subdirectories
)

# read all Excel files into a list of data frames
excel_dfs <- sapply(files, read_xlsx, simplify = FALSE)

# display the names of the data frames in the list
names(excel_dfs)

# combine the third, fourth, and fifth data frames into one (all CDC light trap data)
cdc <- rbind(excel_dfs[[3]], excel_dfs[[4]], excel_dfs[[5]])

# filter for only Ibadan data (remove Kano data)
cdc <- cdc %>%
  filter(City == "Ibadan")

## =========================================================================================================================================
### Indoor Transmission: Ibadan
## =========================================================================================================================================

# filter the dataset for indoor CDC collections and summarize by group
indoor_cdc <- cdc %>%
  dplyr::filter(Location == "Indoor") %>% 
  group_by(`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = TRUE)) %>% # calculate total mosquitoes
  ungroup()

# create a plot for hourly indoor biting of anopheles mosquitoes (Ibadan only)
indoor_anopheles_plot <- ggplot(data = indoor_cdc, aes(
  x = `Time of Collection`, 
  y = total_mosquitoes, 
  group = `Settlement Classification`, 
  colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am")) + 
  geom_point() + 
  labs(
    y = "Total Number of Anopheles \nMosquitos Caught per Hour",
    x = "Time of Collection"
  ) +
  geom_line() + 
  ggtitle("Hourly Indoor Biting") + 
  geom_point(size = 3.0) + 
  theme(plot.title = element_text(size = 12)) + 
  theme_manuscript() + 
  theme(
    legend.position = c(.27, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.x = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(color = "black", size = 0.5)
  )

# save the indoor plot as a PDF file
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_indoor_cdc_ibadan.pdf'), plot = indoor_anopheles_plot, width = 8, height = 6)

## =========================================================================================================================================
### Outdoor Transmission: Ibadan
## =========================================================================================================================================

# filter the dataset for outdoor CDC collections and summarize by group
outdoor_cdc <- cdc %>%
  dplyr::filter(Location == "Outdoor") %>%
  group_by(`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% # calculate total mosquitoes
  ungroup()

# create a plot for hourly outdoor biting of anopheles mosquitoes
outdoor_anopheles_plot <- ggplot(data = outdoor_cdc, aes(
  x = `Time of Collection`, 
  y = total_mosquitoes, 
  group = `Settlement Classification`, 
  colour = `Settlement Classification`
)) + 
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am")) + 
  geom_point() + 
  labs(
    y = "Total Number of Anopheles \nMosquitos Caught per Hour",
    x = "Time of Collection"
  ) + 
  geom_line() + 
  ggtitle("Hourly Outdoor Biting") + 
  geom_point(size = 3.0) + 
  theme(plot.title = element_text(size = 12)) + 
  theme_manuscript() + 
  theme(
    legend.position = c(.27, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.x = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(color = "black", size = 0.5)
  )

# save the outdoor plot as a PDF file
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_outdoor_cdc_ibadan.pdf'), plot = outdoor_anopheles_plot, width = 8, height = 6)

# remove x axis label from indoor biting plot
indoor_anopheles_plot <- indoor_anopheles_plot + theme(axis.title.x = element_blank())

# arrange indoor biting and outdoor biting plots into a grid
hourly_biting_plots <- grid.arrange(indoor_anopheles_plot, outdoor_anopheles_plot, nrow = 2, ncol = 1)

# save combined plots as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_bitingrate_cdc_ibadan.pdf'), plot = hourly_biting_plots, width = 8, height = 8)

## =========================================================================================================================================
### FIGURE 1 - MAPS (Wards Sampled and PSC Households Visited)
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Ibadan Wards Sampled
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Ibadan metro area and correct ward name if necessary
ibadan.shp <- st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == "Oranyan" & LGACode == "31007", "Oranyan_7", WardName))

# assign variable based on whether wards were sampled or not (will be used to color map)
ibadan.shp$ward_color <- ifelse(ibadan.shp$WardName %in% c("Agugu", "Olopomewa", "Challenge"), 
                                "Sampled", "Unsampled")

# extract ward names and save them to a CSV
ib_w <- ibadan.shp$WardName
write.csv(ib_w, file.path(NuDir, "ib_wards.csv"), row.names = FALSE)

# plot: ibadan metro area showing selected wards
wards_ibadan_plot <- ggplot(ibadan.shp) +
  geom_sf(aes(fill = ward_color)) +
  scale_fill_manual(
    values = c("Sampled" = "#6699CC", "Unsampled" = "#F1F1F1"),
    na.value = "transparent"
  ) +
  # geom_text_repel(
  #   data = ibadan.shp,
  #   aes(label = WardName, geometry = geometry), color = "black",
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1
  # ) +
  map_theme() +
  labs(
    title = "Wards Selected for Entomological Survey",
    fill = NULL
  ) +
  coord_sf() +
  theme(legend.title = element_blank())

wards_ibadan_plot

# save plot
ggsave(paste0(ResultDir, "/", Sys.Date(), "_wards_sampled_ibadan.pdf"), wards_ibadan_plot, width = 8, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### PSC: Ibadan
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine relevant dataframes for Pyrethrum Spray Catches (PSC)
psc <- rbind(excel_dfs[[6]], excel_dfs[[7]], excel_dfs[[8]])

# filter PSC data for only Ibadan (Oyo State)
psc <- psc %>%
  dplyr::filter(State == "Oyo")

# filter PSC data for Oyo state and aggregate by settlement classification and month
psc_grouped <- psc %>%
  dplyr::filter(State == "Oyo") %>%
  group_by(`Settlement Classification`, Month) %>%
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = TRUE)) %>%
  ungroup()

# plot: total anopheles mosquitoes caught through PSC in Ibadan
psc_anopheles_plot <- ggplot(data = psc_grouped, aes(x = Month, y = total_mosquitoes, group = `Settlement Classification`, colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("January", "February", "March")) +
  geom_point(size = 3.0) +
  geom_line() +
  labs(
    y = "Total Number of Anopheles Mosquitos Caught",
    x = "Month of Collection (Ibadan)",
    title = "Anopheles Mosquites Collected Through Pyrethrum Spray Catches, \n January–March, 2023"
  ) +
  theme(plot.title = element_text(size = 12)) +
  theme_manuscript() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0, 4)
psc_anopheles_plot

# save the plot
ggsave(paste0(ResultDir, "/", Sys.Date(), "_mosquitoes_collected_psc_ibadan.pdf"), psc_anopheles_plot, width = 8, height = 6)

# plot: locations of PSC data collection in Ibadan
psc_locations_plot <- ggplot(ibadan.shp) +
  geom_sf(fill = "#F1F1F1") +
  geom_point(
    data = dplyr::filter(psc, State == "Oyo"), 
    mapping = aes(x = Longitude, y = Latitude), 
    colour = "#6699CC", size = 2
  ) +
  # geom_text_repel(
  #   data = ibadan.shp,
  #   aes(label = WardName, geometry = geometry), color = "black",
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1
  # ) +
  map_theme() +
  labs(
    title = "Households Visited for PSC Mosquito Collection"
  ) +
  coord_sf() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
psc_locations_plot

# save the plot
ggsave(paste0(ResultDir, "/", Sys.Date(), "_locations_psc_ibadan.pdf"), psc_locations_plot, width = 8, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Ward Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# arrange wards selected and PSC households visited plots into a grid
ward_maps <- grid.arrange(wards_ibadan_plot, psc_locations_plot, nrow = 1, ncol = 2)

# save combined plots as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_ward_maps.pdf'), plot = ward_maps, width = 12, height = 8)


## =========================================================================================================================================
### RELATIVE ABUNDANCE OF SPECIES ANALYSIS
# 1) Compile a species inventory from all datasets. 
# 2) Calculate the proportion of each species relative to the total mosquitoes collected. 
# 3) Compare species composition and relative abundance across seasons (wet vs. dry) and collection methods. 
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Cleaning/Prep
## -----------------------------------------------------------------------------------------------------------------------------------------

# change names of cdc and psc dfs to specify they are dry season
cdc_dry <- cdc %>%
  rename(settlement_type = `Settlement Classification`)
psc_dry <- psc %>%
  rename(settlement_type = `Settlement Classification`)

# load in wet season data and name dfs appropriately
cdc_wet <- readxl::read_xlsx(file.path(WetData, "cdc_wet_no_env_time.xlsx"))
psc_wet <- readxl::read_xlsx(file.path(WetData, "WET_SEASON_ENTO_COLLECTION_PSC_-_all_versions_-_labels_-_2024-08-12-21-20-23.xlsx"))

# rename variables in cdc_wet for consistency
cdc_wet_counts <- cdc_wet %>%
  mutate(method = "CDC") %>%
  mutate(Other = NA) %>%
  mutate(season = "wet") %>%
  rename(Anopheles = total_anopheles, An.gambiae = total_gambiae, An.funestus = total_funestus, Culicine = total_culicine,
         household_code = household_code_number, location = cdc_location, ) %>%
  dplyr::select(date, city, ward_name, settlement_type, household_code, method, season, location, Anopheles, An.gambiae, An.funestus, Culicine, Other)

# select relevant columns of psc_wet and rename for consistency with cdc_wet column names, filter out Kano data
psc_wet_counts <- psc_wet %>%
  mutate(method = "PSC") %>%
  mutate(location = NA) %>%
  mutate(Culicine = NA) %>%
  mutate(season = "wet") %>%
  rename(date = Date, city = City, ward_name = `Ward Name`, settlement_type = `Settlement Type`, 
         household_code = `Household Code/Number`, Anopheles = `Total Number of Anopheles`, 
         An.gambiae = `Total Number of Anopheles Gambiae`, An.funestus = `Total Number of Anopheles Funestus`, Other = `Total Number of Other Species`,
         fed_An.gambiae = `Number of Fed Gambiae`, unfed_An.gambiae = `Number of Unfed Gambiae`,
         fed_An.funestus = `Number of Fed Funestus`, unfed_An.funestus = `Number of Unfed Funestus`,
         fed_Other = `Number of Fed specie(Other)`, unfed_Other = `Number of Unfed specie(Other)`) %>%
  select(date, city, ward_name, settlement_type, household_code, method, season, location, 
         Anopheles, An.gambiae, An.funestus, Culicine, Other, fed_An.gambiae, unfed_An.gambiae,
         fed_An.funestus, unfed_An.funestus, fed_Other, unfed_Other) %>%
  dplyr::filter(psc_wet$City != "Kano") %>%
  mutate(date = as.Date(date)) # remove time from the date

# data cleaning for cdc_dry data
cdc_dry <- cdc_dry %>%
  mutate(`Type of Anopheles_1` = recode(`Type of Anopheles_1`, 
                                        "Gambiens" = "An.gambiae", 
                                        "An. gambiense" = "An.gambiae", 
                                        "An. gambiae" = "An.gambiae", 
                                        "An. funestus" = "An.funestus"))

# new columns for An.gambiae and An.funestus
cdc_dry_counts <- cdc_dry %>%
  mutate(
    An.gambiae = ifelse(grepl("An.gambiae", `Type of Anopheles_1`), `Total Anopheles`, 0),
    An.funestus = ifelse(grepl("An.funestus", `Type of Anopheles_1`), `Total Anopheles`, 0)
  ) %>%
  mutate(Other = NA) %>%
  mutate(season = "dry") %>%
  rename(city= City, ward_name = `Ward Name`, day = Day, month = Month, year = Year, method = Method, location = Location,
         Anopheles = `Total Anopheles`, Culicine = `Total Culicine`, household_code = `Household Code`) %>%
  mutate(date = as.POSIXct(paste(year, month, day), format="%Y %B %d")) %>%
  dplyr::select(date, city, ward_name, settlement_type, household_code, method, season, location, Anopheles, An.gambiae, An.funestus, Culicine, Other)

# data cleaning for psc dry data
psc_dry_counts <- psc_dry %>%
  rename(city = City, ward_name = Ward, day = Day, month = Month, year = Year, method = Method, household_code = `Household Code`,
           An.gambiae = `An. Gambiae`, An.funestus = `An.Funestus`, Other = Others_1, fed_An.gambiae = `Number Fed Gambiae`, 
           unfed_An.gambiae = `Number Unfed Gambiae`, fed_An.funestus = `Number Fed. Funestus`, unfed_An.funestus = `Number Unfed Funestus`, 
           fed_Other = `Number Fed.Others_1`, unfed_Other = `Number Unfed Others_1`) %>%
  mutate(
    location = NA,
    Culicine = NA,
    season = "dry",
    month = match(month, month.name), # converts month name to numeric value
    date = as.POSIXct(paste(year, month, day), format="%Y %m %d"),
    Anopheles = An.gambiae + An.funestus
  ) %>%
  dplyr::select(
    date, city, ward_name, settlement_type, household_code, method, season, location, 
    Anopheles, An.gambiae, An.funestus, Culicine, Other, 
    fed_An.gambiae, unfed_An.gambiae, fed_An.funestus, unfed_An.funestus, fed_Other, unfed_Other
  )

# make household code columns same format (char)
psc_wet_counts$household_code <- as.character(psc_wet_counts$household_code)
cdc_wet_counts$household_code <- as.character(cdc_wet_counts$household_code)
cdc_dry_counts$household_code <- as.character(cdc_dry_counts$household_code)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine the 4 Dfs - CDC dry, CDC wet, PSC dry, PSC wet
## -----------------------------------------------------------------------------------------------------------------------------------------

all_ento_data <- bind_rows(cdc_dry_counts, cdc_wet_counts, psc_dry_counts, psc_wet_counts)

# save this formatted df
write.xlsx(all_ento_data, file.path(EntoDat, "all_ento_dry_wet_data.xlsx"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### 1) Species Inventory + Relative Abundance Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

palette <- c("#e8dab2", "#dd6e42", "#4f6d7a", "#c0d6df")

# species inventory
species_inventory <- all_ento_data %>%
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "species", values_to = "count") %>%
  # add total count and calculate proportion of each mosquito species
  mutate(
    total_count = sum(count), # total count of mosquitoes
    proportion = count / total_count * 100  # proportion for each species
  )

# bar plot for total counts by species
species_inv_plot <- ggplot(species_inventory, aes(x = species, y = count, fill = species)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = palette, 
    name = "Species",
    labels = c("An.funestus", "An.gambiae", "Culicine", "Other")
  ) +
  geom_text(aes(label = count), vjust = -3, size = 4.5) +
  geom_text(aes(label = paste0("(", round(proportion, 1), "%)")), vjust = -1, size = 4.5) +
  scale_x_discrete(labels = c("An.funestus", "An.gambiae", "Culicine", "Other")) +
  labs(
    title = "Total Mosquito Counts by Species", 
    subtitle = "Both PSC and CDC Collection Methods", 
    x = "Species", 
    y = "Count",
  ) +
  theme_manuscript() +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500), limits = c(0, 2700))

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_inv_plot.pdf'), plot = species_inv_plot, width = 8, height = 8)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 2) Comparison of Species Composition Across Seasons Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

# df to calculate counts of each species by season
species_inventory_by_season <- all_ento_data %>%
  group_by(season) %>%
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -season, names_to = "species", values_to = "count") %>% 
  # add total count and calculate proportion of each mosquito species per season
  group_by(season) %>%
  mutate(
    total_count = sum(count),  # total count of mosquitoes for the season
    proportion = count / total_count * 100  # proportion for each species
  ) %>%
  ungroup()

# stacked bar plot
species_season_plot <- ggplot(species_inventory_by_season, aes(x = season, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = palette,
    name = "Species",
    labels = c("An.gambiae", "An.funestus", "Culicine", "Other")
  ) +
  labs(
    title = "Mosquito Species Composition by Season",
    x = "Season",
    y = "Count"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_by_season_plot.pdf'), plot = species_season_plot, width = 8, height = 8)

# export this data as a table (hard to see in plot)
library(officer)
library(knitr)

# create doc and add table
species_seasons_table <- species_inventory_by_season %>%
  mutate(
    proportion = round(proportion, 2),  # Round proportions to 2 decimal places
    species = gsub("total_", "", species)  # Remove 'total_' from species names
  )

# create the Word document
doc <- read_docx()

# add a title to the Word document
doc <- doc %>%
  body_add_par("Mosquito Species Composition by Season", style = "heading 1")

# add the table to the Word document with a default style
doc <- doc %>%
  body_add_table(value = species_seasons_table, style = "table_template")

# save the Word document to the specified directory
output_file <- file.path(ResultDir, "species_seasons_table.docx")
print(doc, target = output_file)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3) Comparison of Species Composition by Collection Method (Indoor CDC, Outdoor CDC, PSC)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a summary dataframe to calculate counts
method_df <- all_ento_data %>%
  dplyr::filter(!is.na(settlement_type)) %>% # remove observations for which settlement_type is NA
  mutate( # separate CDC by indoor and outdoor, PSC remains as-is
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(settlement_type, collection_type) %>%
  summarise(
    An_gambiae_sum = sum(An.gambiae, na.rm = TRUE),
    An_funestus_sum = sum(An.funestus, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(An_gambiae_sum, An_funestus_sum), names_to = "species", values_to = "count")

method_palette <- c("#696d7d", "#8d9f87", "#f0dcca")
             
# plot species composition data
species_by_method <- ggplot(method_df, aes(x = species, y = count, fill = collection_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~settlement_type) +
  scale_fill_manual(
    values = method_palette,
    name = "Collection Method",
    labels = c("Indoor CDC", "Outdoor CDC", "PSC")
  ) +
  scale_x_discrete(labels = c("An. funestus", "An. gambiae")) +
  labs(
    title = "Mosquito Species by Collection Method",
    x = "Species",
    y = "Number of Mosquitoes",
    fill = "Collection Method"
  ) +
  theme_manuscript()
species_by_method

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_method_plot.pdf'), plot = species_by_method, width = 12, height = 8)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 4) Blood Meal Status (PSC Data Only) by Species and Settlement Type
## -----------------------------------------------------------------------------------------------------------------------------------------

psc_bm_df <- bind_rows(psc_dry_counts, psc_wet_counts)

# make df with counts of blood meal status by species and settlement type
bm_status_by_settlement <- psc_bm_df %>%
  group_by(settlement_type) %>%
  summarise(
    total_fed_An.gambiae = sum(fed_An.gambiae, na.rm = TRUE),
    total_unfed_An.gambiae = sum(unfed_An.gambiae, na.rm = TRUE),
    total_fed_An.funestus = sum(fed_An.funestus, na.rm = TRUE),
    total_unfed_An.funestus = sum(unfed_An.funestus, na.rm = TRUE),
    total_fed_Other = sum(fed_Other, na.rm = TRUE),
    total_unfed_Other = sum(unfed_Other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -settlement_type,
    names_to = "blood_species",
    values_to = "count"
  ) %>%
  mutate(
    species = case_when(
      blood_species %in% c("total_fed_An.gambiae", "total_unfed_An.gambiae") ~ "An.gambiae",
      blood_species %in% c("total_fed_An.funestus", "total_unfed_An.funestus") ~ "An.funestus",
      blood_species %in% c("total_fed_Other", "total_unfed_Other") ~ "Other",
      TRUE ~ NA_character_
    ),
    blood_status = case_when(
      blood_species %in% c("total_fed_An.gambiae", "total_fed_An.funestus", "total_fed_Other") ~ "fed",
      blood_species %in% c("total_unfed_An.gambiae", "total_unfed_An.funestus", "total_unfed_Other") ~ "unfed",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

blood_palette = c("#a20f1b", "#f9bfbf")

blood_status_plot <- ggplot(bm_status_by_settlement, aes(x = species, y = count, fill = blood_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = blood_palette,
    name = "Blood Meal Status",
    labels = c("Fed", "Unfed")) +
  facet_wrap(~settlement_type)+
  labs(title = "Distribution of Adult Larvae (Mosquitoes) by Blood Meal Status", x = "Species", y = "Number of Blood-Fed Adult Mosquitoes") +
  labs(subtitle = "By PSC Collection Only") +
  theme_manuscript() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(legend.position = "right",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)
  )
blood_status_plot

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_blood_status_plot.pdf'), plot = blood_status_plot, width = 12, height = 8)


## =========================================================================================================================================
### MOLECULAR ID ANALYSIS
## =========================================================================================================================================

molecular_df <- readxl::read_xlsx(file.path(WetData, "Copy of Molecular analysis of ibadan samples.xlsx"))

# remove larval data (anything not PSC or CDC), rename var names, delete empty column, data cleaning
molecular_df <- molecular_df %>%
  rename_with(~ gsub(" ", "_", tolower(.))) %>% 
  rename(lab_id = lab_i.d, lf_coinfection = "lf_result", ward_name = "area", blood_status = "status") %>% 
  dplyr::filter(method %in% c("PSC", "CDC")) %>% 
  mutate(
    # recode "half gravid" to "gravid"
    blood_status = case_when(blood_status == "Half gravid" ~ "Gravid", TRUE ~ blood_status),
    blood_status = case_when(blood_status == "gravid" ~ "Gravid", TRUE ~ blood_status),
    blood_status = case_when(blood_status == "fed" ~ "Fed", TRUE ~ blood_status),
    # create a separate variable for location (indoor/outdoor) for CDC data
    location = case_when(blood_status %in% c("Indoor") ~ "Indoor", blood_status %in% c("Outdoor") ~ "Outdoor", TRUE ~ NA_character_),
    # set "status" to NA for CDC collections (Fed/Unfed/Gravid was not reported for CDC data)
    blood_status = case_when(method %in% c("CDC") ~ NA, TRUE ~ blood_status)
  ) %>%
  # set time variable to match format of other dfs
  mutate(time = case_when(
    time == "6 to 7" ~ "6-7pm",
    time == "7 to 8" ~ "7-8pm",
    time == "8 to 9" ~ "8-9pm",
    time == "9 to 10" ~ "9-10pm",
    time == "10 to 11" ~ "10-11pm",
    time == "11 to 12" ~ "11-12am",
    time == "12 to 1" ~ "12-1am",
    time == "1 to 2" ~ "1-2am",
    time == "2 to 3" ~ "2-3am",
    time == "3 to 4" ~ "3-4am",
    time == "4 to 5" ~ "4-5am",
    time == "5 to 6" ~ "5-6am",
    time == "2 to 1" ~ NA, # data entry error??? recode these, or remove them?
    time == "11 to 2" ~ NA,  # data entry error??? recode these, or remove them?
    TRUE ~ time # keep other values as is
  )) %>%
  select(-`...8`)


## -----------------------------------------------------------------------------------------------------------------------------------------
### 5) Sporozoite Rate in CDC vs PSC Collections and by Species
## -----------------------------------------------------------------------------------------------------------------------------------------

# make df with counts of sporozoite positivity by collection method
sporozoite_data <- molecular_df %>%
  mutate( # separate CDC by indoor and outdoor, PSC remains as-is
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(collection_type, species, sporozoite_result) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sporozoite_result, values_from = count, values_fill = 0)

# An. gambiae is the only species with sporozoite positivity, so filter out other species + reshape data for plotting
sporozoite_gambiae_data <- sporozoite_data %>%
  dplyr::filter(species == "An. gambiae s.l") %>%
  select(collection_type, Negative, Positive, "NA") %>%
  tidyr::pivot_longer(cols = c(Negative, Positive, "NA"), 
                      names_to = "result", 
                      values_to = "count")

# calculate percentages of positivity by collection method
sporozoite_gambiae_data <- sporozoite_gambiae_data %>%
  group_by(collection_type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    label = paste0(round(percentage, 1), "%")
  )

# plot sporozoite positivity data
spor_palette = c("#4b6043", "#9dba9a", "#ECECEC")

sporozoite_plot <- ggplot(sporozoite_gambiae_data, aes(x = collection_type, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "An. Gambiae Sporozoite Results by Collection Method",
       x = "Collection Method",
       y = "Count",
       fill = "Sporozoite Result") +
  theme_manuscript() +
  scale_fill_manual(
    values = spor_palette,
    name = "Sporozoite Positivity",
    labels = c("Positive", "Negative"))
sporozoite_plot

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_sporozoite_method_plot.pdf'), plot = sporozoite_plot, width = 12, height = 8)

# make another plot that is separated by ward (Agugu and Challenge)
sporozoite_data_wards <- molecular_df %>%
  mutate(
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(ward_name, collection_type, species, sporozoite_result) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sporozoite_result, values_from = count, values_fill = 0)

# filter for An. gambiae s.l and reshape data for plotting
sporozoite_gambiae_data_wards <- sporozoite_data_wards %>%
  dplyr::filter(species == "An. gambiae s.l") %>%
  select(ward_name, collection_type, Negative, Positive, `NA`) %>%
  pivot_longer(cols = c(Negative, Positive, `NA`), 
               names_to = "result", 
               values_to = "count")

# calculate percentages
sporozoite_gambiae_data_wards <- sporozoite_gambiae_data_wards %>%
  group_by(ward_name, collection_type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    label = ifelse(percentage > 0, paste0(round(percentage, 1), "%"), "") # only show label if percentage > 0
  )

# plot
sporozoite_plot_wards <- ggplot(sporozoite_gambiae_data_wards, aes(x = collection_type, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "An. Gambiae Sporozoite Results by Collection Method and Ward",
       x = "Collection Method",
       y = "Count",
       fill = "Sporozoite Result") +
  theme_manuscript() +
  scale_fill_manual(
    values = spor_palette,
    name = "Sporozoite Positivity",
    labels = c("Positive", "Negative", "NA")
  ) +
  facet_wrap(~ ward_name)
sporozoite_plot_wards

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_sporozoite_method_ward_plot.pdf'), plot = sporozoite_plot_wards, width = 12, height = 8)


## =========================================================================================================================================
### HUMAN BITING RATE (HBR)
## =========================================================================================================================================

# subset data where total anopheles count is greater than zero
subset_cdc <- cdc[cdc$`Total Anopheles` > 0, ]

# summarize anopheles caught by settlement classification and location
ano_caught_cdc <- subset_cdc %>%
  dplyr::filter(State == "Oyo") %>%
  group_by(`Settlement Classification`, `Location`) %>% 
  summarise(anopheles_caught = sum(`Total Anopheles`)) %>%
  ungroup()

# set number of night baits
ano_caught_cdc$no_night_bait <- 14

# calculate HBR (# of mosquitoes collected / (number of nights x number of humans slept in the house as bait))
ano_caught_cdc <- ano_caught_cdc %>%
  mutate(HBR = anopheles_caught / no_night_bait)

# filter for indoor hbr
hbr_indoor <- ano_caught_cdc %>% dplyr::filter(Location == "Indoor")

# plot indoor hbr
hbr_indoor_plot <- ggplot(data = hbr_indoor, aes(x = `Settlement Classification`, y = HBR, 
                                   group = `Settlement Classification`,
                                   colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("Formal", "Informal", "Slum")) +
  geom_point(size = 3.0) +
  geom_line() +
  labs(y = "Human Biting Rate", x = "Settlement Type",
       title = "Indoor Human Biting Rate by settlement type, 2023") +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
hbr_indoor_plot

# filter for outdoor hbr
hbr_outdoor <- ano_caught_cdc %>% dplyr::filter(Location == "Outdoor")

# plot outdoor hbr
hbr_outdoor_plot <- ggplot(data = hbr_outdoor, aes(x = `Settlement Classification`, y = HBR, 
                                    group = `Settlement Classification`,
                                    colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("Formal", "Informal", "Slum")) +
  geom_point(size = 3.0) +
  geom_line() +
  labs(y = "Human Biting Rate", x = "Settlement Type",
       title = "Outdoor Human Biting Rate by settlement type, 2023") +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
hbr_outdoor_plot
