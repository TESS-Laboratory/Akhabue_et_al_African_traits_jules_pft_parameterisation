#Load library--------
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)


# Load site info------------------
site_info <- read_csv("Site_info.csv", show_col_types = FALSE)


# Clean site names and define ecosystem order------------------
site_info <- site_info %>%
  mutate(
    Site_Code = gsub("_", "-", Site_Code),
    IGBP = factor(
      IGBP,
      levels = c("Forest", "Savanna", "Grassland", "Cropland", "Wetland")
    )
  )




# Get Africa shapefile----------------
africa <- ne_countries(
  scale = "medium",
  continent = "Africa",
  returnclass = "sf"
)


# Plot--------------------
site_map <- ggplot() +
  
  # Africa polygons
  geom_sf(
    data = africa,
    fill = "white",
    colour = "grey35",
    linewidth = 0.3
  ) +
  
  # Site points
  geom_point(
    data = site_info,
    aes(
      x = Site_Longitude,
      y = Site_Latitude,
      colour = IGBP
    ),
    size = 3.4
  ) +
  
  # Site labels
  geom_text_repel(
    data = site_info,
    aes(
      x = Site_Longitude,
      y = Site_Latitude,
      label = Site_Code,
      colour = IGBP
    ),
    size = 5,              
    fontface = "bold",
    box.padding = 0.25,
    point.padding = 0.15,
    segment.size = 0.25,
    segment.color = "grey40",
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  # Colours
  scale_colour_manual(
    values = c(
      "Forest"    = "black",         
      "Savanna"   = "goldenrod3",
      "Grassland" = "dodgerblue3",
      "Cropland"  = "firebrick",
      "Wetland"   = "purple4"
    )
  ) +
  
  # Map extent
  coord_sf(
    xlim = c(-20, 55),
    ylim = c(-35, 38),   
    expand = FALSE
  ) +
  
  # Labels
  labs(
    x = "Longitude",
    y = "Latitude",
    colour = "Ecosystem type\n(IGBP)"
  ) +
  
  # Theme
  theme_classic(base_size = 10) +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12, colour = "black"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# Show plot
site_map




# Save------------------
ggsave(
  filename = "Africa_flux_sites_map.png",
  plot = site_map,
  width = 22,
  height = 18,
  units = "cm",
  dpi = 300,
  bg = "white"
)
