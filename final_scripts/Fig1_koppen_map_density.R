
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(sf)
library(maps)

# paths
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"

# load case-crossover dataset
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)

# load county and state shapefiles (copied data from ADRD mapping project)
county_sf <- read_sf(paste0(dir_data, "shapefiles/county/cb_2015_us_county_20m.shp")) %>%
  filter(!STATEFP %in% c("02","15", "66", "72", "60", "69", "78")) %>% 
  mutate(fips = as.numeric(GEOID))
#Oglala Lakota County, SD. Shannon County, SD (FIPS code = 46113) was renamed Oglala Lakota County and assigned anew FIPS code (46102) effective in 2014
county_sf$fips[county_sf$fips == 46102] <- 46113 

state_sf <- read_sf(paste0(dir_data, "shapefiles/state/cb_2015_us_state_20m.shp")) %>%
  filter(!STATEFP %in% c("02","15", "66", "72", "60", "69", "78"))




# load Koppen county data (copied data from heat/ADRD paper)
k_fips <- fread(paste0(dir_data, "Koppen_counties.csv"))
k_fips$Koppen <- factor(k_fips$Koppen, 
                        c("BSh", "BSk", "BWh", "BWk",
                          "Dfa", "Dfb", "Dfc", "Dsa", "Dsb", "Dwa", "Dwb",
                          "Cfa", "Cfb", "Csa", "Csb",
                          "Af", "Am", "Aw"),
                        c("Semi-arid hot", "Semi-arid cold", "Desert hot", "Desert cold",
                          "Continental hot summer", "Continental warm summer",
                          "Continental cold summer", "Continental dry hot summer",
                          "Continental dry warm summer", "Continental dry winter hot summer",
                          "Continental dry winter warm summer",
                          "Temperate hot summer", "Temperate warm summer",
                          "Temperate dry hot summer", "Temperate dry warm summer",
                          "Tropical rainforest", "Tropical monsoon", 
                          "Tropical savanna dry winter"))

county_sf <- merge(county_sf, k_fips, by.x = "fips", by.y = "StCoFIPS", all.x = TRUE)

# remove county with missing koppen
county_sf <- county_sf %>%
  filter(fips != 8014)

########################################################################
########################### NEW FIGURE 1 ###############################
########################################################################


################### Figure 1a: map

# map of Koppen zones by county
county_sf %>%
  ggplot() +
  geom_sf(aes(fill = Koppen), lwd = 0.1) +
  geom_sf(data = state_sf, col = "black", fill = NA, lwd = 0.3) +
  coord_sf(crs = st_crs(5070)) +
  labs(fill = "") + 
  scale_fill_manual(values = c(

    # arid
    "Semi-arid hot" = "#f3bed8",
    "Semi-arid cold" = "#eb93be",
    "Desert hot" = "#db3d89",
    "Desert cold" = "#971c57",

    # continental
    "Continental hot summer" = "#dabff2",
    "Continental warm summer" = "#c195ea",
    "Continental cold summer" = "#a96be1",
    "Continental dry hot summer" = "#9040d8",
    "Continental dry warm summer" = "#7627bf",
    "Continental dry winter hot summer" = "#5c1e94",
    "Continental dry winter warm summer" = "#42156a",

    # temperate
    "Temperate hot summer" = "#cde5cd",
    "Temperate warm summer" = "#8ac28a",
    "Temperate dry hot summer" = "#4f974f",
    "Temperate dry warm summer" = "#2c542c",

    # tropical
    "Tropical rainforest" = "#bfdff2",
    "Tropical monsoon" = "#6ab4e1",
    "Tropical savanna dry winter" = "#2685bf")) +
  theme_void(base_family = "Arial") +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(16, 16, 16, 16, NA, NA), # Controls the items per row
        size = c(3, 3, 3, 3, NA, NA)
      ),
      nrow = 2
    )
  )
ggsave(paste0(dir_figs, "new_fig1.png"), width = 10, height = 5, dpi = 300)


################### Figure 1b: density

# # load heat index data (saved in final_data_script.R)
# hi <- read_fst(paste0(dir_data, 'heat_index.fst'), as.data.table = TRUE)
# 
# # need to get zip to fips in hi data--use case crossover dataset
# 
# # merge with Koppen (can't because data has zip, not fips)
# hi <- merge(hi, k_fips)

# look at distribution across case/control days only
ggplot(data) +
  geom_density(aes(x = HI_lag_0, 
                   fill = factor(Koppen_s, 
                                 levels = c("Arid", 
                                            "Continental", "Temperate", 
                                            "Tropical"))), 
               bw = .9, alpha = 0.5, linewidth = 0.7) +
  facet_wrap(~Koppen_s) +
  scale_x_continuous(limits = c(0, 50)) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "Arid" = "plum2",
    "Continental" = "darkorchid",
    "Temperate" = "lightgreen",
    "Tropical" = "skyblue"
  )) +
  labs(x = expression("Heat Index ("*degree*"C)"), y = "Density")


################### Figure 1b: tables to go with density

data[, .(#n = sum(case == 1),
         med = quantile(HI_lag_0, c(.5)),
         q99 = quantile(HI_lag_0, c(.99))), by = Koppen_s]




