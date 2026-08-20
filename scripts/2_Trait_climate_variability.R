# load libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(patchwork)



# 1) Load and clean --------------------------

df <- read_csv("traits_with_MAT_MAP_ERA5Land_1991_2020_withLonLat.csv") %>%
  clean_names()

# Check column names quickly
names(df)

# Adjust these names if differ after clean_names()
# Expected: trait_name, std_value, mat_c, map_mm
df1 <- df %>%
  filter(!is.na(trait_name), !is.na(std_value), !is.na(mat_c), !is.na(map_mm)) %>%
  mutate(trait_name = as.factor(trait_name))



 
# 2) Plotting theme  --------------------------

theme_clean_panel <- function(base_size = 18) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
      
      axis.title = element_text(face = "bold", size = base_size + 4),
      axis.text  = element_text(size = base_size - 3),
      
      legend.title = element_text(face = "bold", size = base_size + 2),
      legend.text  = element_text(size = base_size),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width  = unit(0.5, "cm"),
      
      plot.margin = margin(10, 15, 10, 10)
    )
}



 
# 3) Plot A: MAT–MAP density (climate envelope)--------------------------

n_bins_hex <- 70

p_climate <- ggplot(df1, aes(x = mat_c, y = map_mm)) +
  geom_hex(bins = n_bins_hex) +
  scale_y_continuous(labels = comma) +
  labs(x = "MAT (°C)", y = "MAP (mm yr⁻¹)", fill = "count") +
  theme_clean_panel(base_size = 18) 

p_climate


ggsave(
  filename = "Fig_climate_envelope_hex.png",
  plot = p_climate,
  width = 8, height = 6, units = "in", dpi = 400, bg = "white"
)
