#load packages and data ----
library(ggplot2)  
library(dplyr)  
library(rgdal)  
library(raster) 
library(ggsn)  
library(rworldmap)  
library(plotrix)
library(sf)
library(rnaturalearth)
library(lwgeom)
library(maps)
library(RColorBrewer) 
library(tidyr)
library(tidyverse)
library(patchwork)
library(viridis)
library(rgeos)
library(scales)
library(stringr)




#--------------------SECTION A --------------------

# read data ----
trait_data_new <- read.csv("workdata_.csv")


# Specify the trait names of interest.
traits_to_remove <- c(
  "Leaf carbon/nitrogen (C/N) ratio",
  "Leaf carbon (C) content per leaf dry mass",
  "Leaf carbon (C) content per leaf area",
  "Leaf nitrogen/phosphorus (N/P) ratio",
  "Leaf phosphorus (P) content per leaf dry mass",
  "Leaf magnesium (Mg) content per leaf dry mass",
  "Leaf potassium (K) content per leaf dry mass",
  "Leaf sodium (Na) content per leaf dry mass",
  "Leaf calcium (Ca) content per leaf dry mass"
)

trait_data <- trait_data_new %>%
  filter(!TraitName %in% traits_to_remove)



# Filter out rows where StdValue is less than 0
trait_data <- trait_data %>%
  filter(StdValue > 0)



# Remove unknown species in the AccSpeciesName column
trait_data <- trait_data %>%
  filter(AccSpeciesName != "unknown")



# Keep only the columns we need ----
vars <- c("AccSpeciesName", "TraitName", "StdValue", 
          "UnitName", "Latitude", "Longitude", "Exposition", "Plant_dev_status", "Reference")

trait_workdata<- trait_data %>% dplyr::select(one_of(vars))


# Define a named vector with old trait names as names and corresponding new trait names as values
trait_name_mapping <- c(
  "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, petiole excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, petiole included)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, petiole excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, petiole included)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)" = "Leaf Area",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" = "leaf area per leaf dry mass",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf area (Farquhar model)" = "Vcmax/LA",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf dry mass (Farquhar model)" = "Vcmax/LMA")

# Replace old trait names with new trait names
new_trait_workdata <- trait_workdata %>% 
  mutate(TraitName = ifelse(TraitName %in% names(trait_name_mapping), trait_name_mapping[TraitName], TraitName))




# converting units and changing units names to match JULES default unit for LMA (That is SLA=1/LMA) AND Nmass
# Step 1: Filter for the trait "leaf area per leaf dry mass"
leaf_area_mask <- new_trait_workdata$TraitName == "leaf area per leaf dry mass"

# Step 2: Convert SLA (mm2/mg) to LMA (kg/m2) by taking the reciprocal
new_trait_workdata$StdValue[leaf_area_mask] <- 1 / new_trait_workdata$StdValue[leaf_area_mask]

# Step 3: Update the UnitName for this trait to "kg/m2"
new_trait_workdata$UnitName[leaf_area_mask] <- "kg m-2"


# Step 1: Filter for the trait "Leaf nitrogen (N) content per leaf dry mass" and convert its values
# Replace the "TraitName" and "StdValue" columns with the actual column names in your dataset
leaf_nitrogen_mask <- new_trait_workdata$TraitName == "Leaf nitrogen (N) content per leaf dry mass"

# Step 2: Convert mg/g to g/g by dividing by 1000 for the corresponding StdValue column
new_trait_workdata$StdValue[leaf_nitrogen_mask] <- new_trait_workdata$StdValue[leaf_nitrogen_mask] / 1000

# Step 3: Update the UnitName for this specific trait to "kg/kg"
new_trait_workdata$UnitName[leaf_nitrogen_mask] <- "kg/kg"




# Specify the trait names of interest 
# working with just Nmass and LMA 
traits_of_interest <- c(
  "leaf area per leaf dry mass",
  "Leaf nitrogen (N) content per leaf dry mass"
)

# Filter the data for the specified trait names and sort by TraitName
new_trait_workdata <- new_trait_workdata %>%
  filter(TraitName %in% traits_of_interest) %>%
  arrange(TraitName)


new_trait_workdata <- new_trait_workdata %>%
  mutate(TraitName = if_else(
    TraitName == "leaf area per leaf dry mass",
    "Leaf Mass per Area",
    TraitName
  ))




# compare the observed species for Global and Africa observation 
species_count_global <- new_trait_workdata %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



# compare the observed species for exposition status 
species_exposition <- new_trait_workdata %>%
  group_by(Exposition) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# compare the observed species for plant dev status 
species_dev_status <- new_trait_workdata %>%
  group_by(Plant_dev_status) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





# 1st LEVEL ----

## Make a prelim plot ----

(prelim_plot <- ggplot(new_trait_workdata, aes(x = Longitude, y = Latitude, 
                                           colour = TraitName)) +
   geom_point())

## Further cleaning - Filter out rows with latitude and longitude outside of a specified range ----
new_trait_workdata <- new_trait_workdata %>%
  filter(Latitude <= 100)


## Make global map of traits ----
# World map data in a Robinson projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world_robinson <- st_transform(world, crs = "+proj=robin")

# Aggregate data by TraitName and geographic coordinates
data_aggregated <- new_trait_workdata %>%
  group_by(Latitude, Longitude, TraitName) %>%
  summarise(TraitCount = n()) %>%
  ungroup()

# Convert dataset to an sf object
data_sf <- st_as_sf(data_aggregated, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to Robinson projection
data_robinson <- st_transform(data_sf, crs = "+proj=robin")

# Plot the heat map with traits
global_map <- ggplot() +
  geom_sf(data = world_robinson, fill = "gray95", color = "gray80", linewidth = 0.2) +
  geom_sf(data = data_robinson, aes(color = TraitName, size = TraitCount), alpha = 0.7) +
  scale_color_viridis_d(
    option = "plasma",
    name = "Trait",
    labels = c("LMA", "Nmass"),
    guide = guide_legend(
      override.aes = list(shape = 15, size = 5)
    )
  ) +
  scale_size_continuous(
    range = c(1, 12),
    breaks = c(100, 1000, 3000, 5000, 7000, 10000),
    labels = c("100", "1k", "3k", "5k", "7k", "10k"),
    name = "Observation\nCount"
  ) +
  coord_sf(expand = TRUE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(color = NA),
    panel.grid = element_line(color = "gray80"),
    legend.position = "right",
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 22, face = "bold"),
    text = element_text(size = 20, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    #legend.box.spacing = unit(0.6, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

global_map


ggsave("trait_global_map22.png", plot = global_map, width = 20, height = 10, dpi = 600, bg = "white")




# Plot the map
ggplot() +
  geom_sf(data = world_robinson, fill = "gray95", color = "gray80") +
  geom_sf(data = data_robinson, aes(color = TraitName), size = 2, alpha = 0.8) +
  scale_color_viridis_d(
    option = "plasma",
    name = "Trait Name",
    guide = guide_legend(
      override.aes = list(shape = 15, size = 5) # Square legend keys
    )
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_line(color = "gray80"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  ggtitle("Global Distribution of Plant Trait Observations") +
  theme(
    text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )





# 2nd LEVEL  ---- 

## Make regional map of traits for Africa ----

# Get world map and filter for Africa

africa <- sf::st_make_valid(world[world$continent == "Africa", ])


# Filter trait data for Africa
trait_africa <- new_trait_workdata %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(africa)

# Plot map
ggplot() +
  geom_sf(data = africa, fill = NA, color = "black", size = 0.5) +  # Add country borders
  geom_sf(data = trait_africa, aes(color = TraitName), size = 2, alpha = 0.8) +  # Plot points
  coord_sf() +
  scale_color_viridis_d(option = "plasma", name = "Trait") +  # Use a visually appealing color scale
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Trait") +
  guides(color = guide_legend(
    title = "Trait",
    override.aes = list(size = 4, alpha = 1)  # Enhance legend appearance
  )) +
  ggtitle("Plant Trait Observations in Africa") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))





#export both dataset
#write.csv(new_trait_workdata, "trait_data.csv", row.names = FALSE)
#write.csv(trait_africa, "trait_africa.csv", row.names = FALSE)



trait_LMA <- trait_africa %>%
  filter(TraitName == "Leaf Mass per Area")

trait_Nmass <- trait_africa %>%
  filter(TraitName == "Leaf nitrogen (N) content per leaf dry mass")


trait_summary <- trait_africa %>%
  group_by(TraitName) %>%
  summarise(
    n_obs = n(),
    n_species = n_distinct(AccSpeciesName),
    .groups = "drop"
  )

trait_summary


# compare the observed species for Africa observation 
species_count_africa <- trait_africa %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 



# compare the observed species for exposition status 
species_exposition_africa <- trait_africa %>%
  group_by(Exposition) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# compare the observed species for plant dev status 
species_dev_status_africa <- trait_africa %>%
  group_by(Plant_dev_status) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



## Make separate plots for each trait in the African region ----
#trait_plots_africa <- list()

# Define alphabet letters
#letters <- LETTERS[1:length(unique(trait_africa$TraitName))]

#for (i in seq_along(unique(trait_africa$TraitName))) {
 # trait <- unique(trait_africa$TraitName)[i]
 # trait_plot <- ggplot() +
  #  geom_sf(data = africa, fill = NA, color = "black") +
  #  geom_sf(data = filter(trait_africa, TraitName == trait), aes(color = TraitName)) +
  #  coord_sf() +
 #   theme_void() +
  #  labs(x = "Longitude", y = "Latitude", color = "Trait") +
  #  guides(color = guide_legend(title = paste(letters[i]))) +  # Set legend title
  #  ggtitle(letters[i]) +  # Use alphabet letters as titles
  #  theme(legend.position = "right")  # Position legend on the right
  
 # trait_plots_africa[[as.character(trait)]] <- trait_plot
#}

# Combine and display the plots on a single page with a common legend
#trait_plots_africa_combined <- wrap_plots(trait_plots_africa, ncol = 4) +
 # plot_layout(ncol = 4, guides = 'collect')  # Adjust the number of columns as needed

#trait_plots_africa_combined




## Make Africa map of traits ----
## Make Africa map of traits ----

Multi_trait_map <- ggplot(trait_africa, aes(x = Longitude, y = Latitude)) +
  geom_sf(
    data = africa,
    aes(geometry = geometry),
    fill = NA,
    color = "black",
    inherit.aes = FALSE,
    linewidth = 0.25
  ) +
  geom_hex(binwidth = 2, alpha = 0.85) +
  scale_fill_viridis_c(
    name = "Observation\ncount"
  ) +
  facet_wrap(
    ~TraitName,
    ncol = 2,
    labeller = as_labeller(c(
      "Leaf Mass per Area" = "LMA",
      "Leaf nitrogen (N) content per leaf dry mass" = "Nmass"
    ))
  ) +
  coord_sf(
    xlim = c(-25, 55),
    ylim = c(-38, 40),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic() +
  guides(
    fill = guide_colorbar(
      barheight = unit(5.5, "cm"),
      barwidth  = unit(0.8, "cm"),
      title.position = "top"
    )
  ) +
  theme(
    strip.text = element_text(
      size = 30,
      face = "bold",
      margin = margin(2, 2, 2, 2)
    ),
    axis.title = element_text(size = 24, face = "bold"),
    axis.text  = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 26, face = "bold"),
    legend.text  = element_text(size = 26, face = "bold"),
    legend.key.height = unit(1.6, "cm"),
    legend.key.width  = unit(1.0, "cm"),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(2, 2, 2, 2, "pt")
  )


Multi_trait_map


ggsave("Afri_trait_multi_map1.png", plot = Multi_trait_map, width = 21, height = 13, dpi = 300, bg = "white")






# 3rd LEVEL ----

## Comparison between global and African observation-------------------

global_frequency <- table(new_trait_workdata$TraitName)
print(global_frequency)

# Frequency table for the filtered dataset with only African countries
africa_frequency <- table(trait_africa$TraitName)
print(africa_frequency)



# 4th LEVEL ----

# Data load and preparation ----

PFT_data <- read_csv("Mapped_PFT_Harmonized.csv")

Trait_species_data <- read_csv("new_trait_africa.csv")




# Perform a left join to add PFT information to Trait_species
Trait_species_with_PFT <- Trait_species_data %>%
  dplyr::left_join(PFT_data %>%
                     dplyr::select(AccSpeciesName, PFT), 
                   by = "AccSpeciesName")



# omit NA data. these are species that could not be classified due to limited information and available resources

Trait_species_with_PFT <- Trait_species_with_PFT[!is.na(Trait_species_with_PFT$PFT), ]

# remove columns not useful 
Trait_species_with_PFT <- subset(Trait_species_with_PFT, select = -c(geometry))


# filter using `%in%` to exclude "in situ" but keep NA values
Trait_species_with_PFT <- Trait_species_with_PFT %>%
  filter(is.na(Exposition) | Exposition != "in situ")



#write.csv(Trait_species_with_PFT, "Trait_species_with_PFT.csv", row.names = FALSE)




## Visualization of data to ascertain the value of PFT classes in my data and what steps can be taken further ----
# PFT Density Plot----
# This plot will visualize the density of PFTs in different regions. Could use a hexbin plot or kernel density estimation.

pft_counts <- Trait_species_with_PFT %>%
  count(PFT) %>%
  rename(count = n)


# Prepare custom legend labels
custom_labels <- pft_counts %>%
  mutate(label = paste(PFT, " (", count, " obs)", sep = ""))



(heat_map_density <- ggplot() +
    geom_hex(data = Trait_species_with_PFT, aes(x = Longitude, y = Latitude, fill = PFT), bins = 35) +
    geom_sf(data = africa, fill = NA, color = "black") +  # Add country borders
    coord_sf() +  # Use coord_sf instead of coord_quickmap
    theme_void() +
    labs(x = "Longitude", y = "Latitude", fill = "PFT") +
    scale_fill_viridis_d()) + 
  scale_fill_viridis_d(labels = custom_labels$label)



# Get PFT counts for the legend
pft_counts <- Trait_species_with_PFT %>%
  count(PFT) %>%
  rename(count = n)

# Create label with counts for each facet
Trait_species_with_PFT <- Trait_species_with_PFT %>%
  mutate(PFT_label = case_when(
    PFT == "BDT" ~ "Broadleaf Deciduous Trees",
    PFT == "BET-Tr" ~ "Tropical Broadleaf Evergreen Trees",
    PFT == "BET-Te" ~ "Temperate Broadleaf Evergreen Trees",
    PFT == "C3" ~ "C3 Grasses",
    PFT == "C4" ~ "C4 Grasses",
    PFT == "DSH" ~ "Deciduous Shrubs",
    PFT == "ESH" ~ "Evergreen Shrubs",
    PFT == "NET" ~ "Needleleaf Evergreen Trees",
    TRUE ~ PFT
  ))





# Plot: facet by PFT_label
pft_density <- ggplot() +
  geom_hex(data = Trait_species_with_PFT, aes(x = Longitude, y = Latitude), bins = 25) +
  geom_sf(data = africa, fill = NA, color = "black", linewidth = 0.3) +
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 37)) +
  facet_wrap(~PFT_label, ncol = 4) +
  scale_fill_viridis_c(option = "C", name = "No. of Observations", trans = "sqrt") +  # square-root scale evens extremes
  labs(
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 20, face = "bold")
  )


pft_density



ggsave("pft_density.png", plot = pft_density, width = 20, height = 12, dpi = 300, bg = "white")


# Calculate the summary statistics
summary_stats <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(
    mean   = mean(StdValue, na.rm = TRUE),
    median = median(StdValue, na.rm = TRUE),
    q25    = quantile(StdValue, 0.25, na.rm = TRUE, type = 7),
    q75    = quantile(StdValue, 0.75, na.rm = TRUE, type = 7),
    IQR    = q75 - q25,
    min    = min(StdValue, na.rm = TRUE),
    max    = max(StdValue, na.rm = TRUE),
    .groups = "drop"
  )

# View the results
print(summary_stats)


write.csv(summary_stats, "summary_stats_new.csv", row.names = FALSE)






# ---------SECTION B --------------------
# 1. PFT order------------------
pft_order <- c("BET-Tr", "BET-Te", "BDT", "NET", "C3", "C4", "ESH", "DSH")


# 2. JULES values-----------------
jules_df <- tibble::tribble(
  ~PFT,     ~LMA,   ~Nmass,
  "BET-Tr", 0.1039, 0.0170,
  "BET-Te", 0.1403, 0.0144,
  "BDT",    0.0823, 0.0210,
  "NET",    0.2263, 0.0115,
  "C3",     0.0498, 0.0219,
  "C4",     0.1370, 0.0113,
  "ESH",    0.1515, 0.0136,
  "DSH",    0.0550, 0.0238
)


# 3. Optional check-----------------
dplyr::count(Trait_species_with_PFT, TraitName, UnitName, sort = TRUE)


# 4. Helper for quantiles-----------------
qfun <- function(x, p) {
  unname(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}


# 5. Build Africa TRY summary--------------------
make_trait_summary <- function(df, source_name) {
  df %>%
    dplyr::mutate(
      trait_key = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(TraitName), "mass per area") ~ "LMA",
        stringr::str_detect(stringr::str_to_lower(TraitName), "nitrogen") &
          stringr::str_detect(stringr::str_to_lower(TraitName), "dry mass") ~ "Nmass",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(trait_key), PFT %in% pft_order) %>%
    dplyr::group_by(PFT, trait_key) %>%
    dplyr::summarise(
      n      = sum(!is.na(StdValue)),
      q10    = qfun(StdValue, 0.10),
      q25    = qfun(StdValue, 0.25),
      median = median(StdValue, na.rm = TRUE),
      q75    = qfun(StdValue, 0.75),
      q90    = qfun(StdValue, 0.90),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = trait_key,
      values_from = c(n, q10, q25, median, q75, q90),
      names_sep   = "_"
    ) %>%
    dplyr::mutate(source = source_name)
}

africa_sum <- make_trait_summary(Trait_species_with_PFT, "Africa TRY")


# 6. JULES in same structure-------------------
jules_sum <- jules_df %>%
  dplyr::transmute(
    PFT,
    n_LMA = NA_real_,
    q10_LMA = NA_real_,
    q25_LMA = NA_real_,
    median_LMA = LMA,
    q75_LMA = NA_real_,
    q90_LMA = NA_real_,
    n_Nmass = NA_real_,
    q10_Nmass = NA_real_,
    q25_Nmass = NA_real_,
    median_Nmass = Nmass,
    q75_Nmass = NA_real_,
    q90_Nmass = NA_real_,
    source = "JULES"
  )


# 7. JULES count file for panel labels-----------------
jules_counts <- readr::read_csv("Anna_H_count_PFT_traits.csv", show_col_types = FALSE)

jules_counts2 <- jules_counts %>%
  dplyr::rename(
    n_LMA = LMA,
    n_Nmass = NMASS
  ) %>%
  dplyr::mutate(
    PFT = dplyr::case_when(
      PFT %in% c("Esh", "ESH") ~ "ESH",
      PFT %in% c("DSh", "DSH", "Dsh") ~ "DSH",
      TRUE ~ PFT
    )
  ) %>%
  dplyr::filter(PFT %in% pft_order) %>%
  dplyr::select(PFT, n_LMA, n_Nmass)



# 8. Remove Africa plotting values for NET only (keeps label, but removes Africa point/spread from NET panel)---------------------
africa_sum_plot <- africa_sum %>%
  dplyr::mutate(
    across(
      c(q10_LMA, q25_LMA, median_LMA, q75_LMA, q90_LMA,
        q10_Nmass, q25_Nmass, median_Nmass, q75_Nmass, q90_Nmass),
      ~ ifelse(PFT == "NET", NA, .x)
    )
  )




# 9. Plotting data------------------
plot_df <- dplyr::bind_rows(africa_sum_plot, jules_sum) %>%
  dplyr::mutate(
    PFT = factor(PFT, levels = pft_order),
    source = factor(source, levels = c("Africa TRY", "JULES"))
  )

spread_df <- plot_df %>%
  dplyr::filter(source == "Africa TRY")




# 10. Panel label dataframe----------------------
africa_labels <- africa_sum %>%
  dplyr::transmute(
    PFT,
    africa_line = dplyr::if_else(
      PFT == "NET",
      "Africa: insufficient data",
      paste0("Africa: nLMA=", n_LMA, ", nNmass=", n_Nmass)
    )
  )

jules_labels <- jules_counts2 %>%
  dplyr::transmute(
    PFT,
    jules_line = paste0("JULES: nLMA=", n_LMA, ", nNmass=", n_Nmass)
  )

n_panel_df <- africa_labels %>%
  dplyr::left_join(jules_labels, by = "PFT") %>%
  dplyr::mutate(
    label = paste(africa_line, jules_line, sep = "\n"),
    PFT = factor(PFT, levels = pft_order),
    x = 0.365,
    y = 0.043
  )


# 11. Plot----------------
p_pft_traits <- ggplot() +
  
  # Africa spread: 10th to 90th percentile
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = q10_LMA, xend = q90_LMA,
      y = median_Nmass, yend = median_Nmass,
      colour = source
    ),
    linewidth = 0.5,
    alpha = 0.45,
    show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = median_LMA, xend = median_LMA,
      y = q10_Nmass, yend = q90_Nmass,
      colour = source
    ),
    linewidth = 0.5,
    alpha = 0.45,
    show.legend = FALSE
  ) +
  
  # Africa spread: 25th to 75th percentile
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = q25_LMA, xend = q75_LMA,
      y = median_Nmass, yend = median_Nmass,
      colour = source
    ),
    linewidth = 1.1,
    show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = median_LMA, xend = median_LMA,
      y = q25_Nmass, yend = q75_Nmass,
      colour = source
    ),
    linewidth = 1.1,
    show.legend = FALSE
  ) +
  
  # Africa point
  ggplot2::geom_point(
    data = plot_df %>% dplyr::filter(source == "Africa TRY"),
    aes(
      x = median_LMA,
      y = median_Nmass,
      colour = source,
      fill = source,
      shape = source
    ),
    size = 4.2,
    stroke = 1.2
  ) +
  
  # JULES point
  ggplot2::geom_point(
    data = plot_df %>% dplyr::filter(source == "JULES"),
    aes(
      x = median_LMA,
      y = median_Nmass,
      colour = source,
      shape = source
    ),
    size = 4.2,
    stroke = 1.2
  ) +
  
  # panel labels
  ggplot2::geom_text(
    data = n_panel_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 1,
    size = 5.8,
    lineheight = 1.0,
    colour = "black",
    face = "bold"
  ) +
  
  ggplot2::facet_wrap(~PFT, nrow = 4) +
  
  ggplot2::scale_colour_manual(
    breaks = c("Africa TRY", "JULES"),
    values = c(
      "Africa TRY" = "red",
      "JULES"      = "blue3"
    )
  ) +
  
  ggplot2::scale_fill_manual(
    breaks = c("Africa TRY", "JULES"),
    values = c(
      "Africa TRY" = "red",
      "JULES"      = NA
    )
  ) +
  
  ggplot2::scale_shape_manual(
    breaks = c("Africa TRY", "JULES"),
    values = c(
      "Africa TRY" = 21,
      "JULES"      = 4
    )
  ) +
  
  ggplot2::guides(
    fill = "none",
    shape = "none",
    colour = ggplot2::guide_legend(
      override.aes = list(
        shape    = c(21, 4),
        fill     = c("red", NA),
        size     = c(4, 4),
        stroke   = c(1.2, 1.2),
        linetype = c(0, 0)
      )
    )
  ) +
  
  ggplot2::labs(
    x = expression(LMA~(kg~m^{-2})),
    y = expression(Nmass~(g~g^{-1})),
    colour = NULL
  ) +
  
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", size = 20),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.8),
    axis.title = ggplot2::element_text(face = "bold", size = 24),
    axis.text = ggplot2::element_text(colour = "black", size = 18, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 16, face = "bold"),
    panel.spacing = grid::unit(1.1, "lines"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

p_pft_traits

ggplot2::ggsave(
  "p_pft_traits_africa_jules_only.png",
  plot = p_pft_traits,
  width = 12,
  height = 15,
  dpi = 300,
  bg = "white"
)

