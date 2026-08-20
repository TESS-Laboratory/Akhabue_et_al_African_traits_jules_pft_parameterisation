#load library--------
library(tidyverse)
library(patchwork)


# Load statistics table----------------
stats <- read_csv(
  "option4_timeseries_heatmaps/performance_stats_all_variables.csv",
  show_col_types = FALSE
)

# Keep only GPP
stats <- stats %>%
  filter(variable == "GPP")


# Site years-----------------
site_years <- tribble(
  ~site,     ~years,
  "BW_GUM",   3,
  "BW_NXR",   3,
  "CG_TCH",   4,
  "GH_ANK",   4,
  "ML_AGG",   4,
  "NE_WAF",  13.5,
  "NE_WAM",  13.5,
  "SD_DEM",   5,
  "SN_RAG",   3,
  "SN_DHR",  10,
  "SN_NKR",   4,
  "UG_JIN",   1,
  "ZA_CATH",  7,
  "ZA_KRU",  14,
  "ZA_WGN",  11,
  "ZM_MON",  10
)




# Define ecosystem order-----------------
site_ecosystem <- tribble(
  ~site, ~ecosystem,
  "BW_GUM", "Wetland",
  "BW_NXR", "Wetland",
  "UG_JIN", "Wetland",
  
  "CG_TCH", "Grassland",
  "ML_AGG", "Grassland",
  "SD_DEM", "Grassland",
  "SN_DHR", "Grassland",
  "ZA_CATH", "Grassland",
  "ZA_WGN", "Grassland",
  
  "GH_ANK", "Forest",
  "ZM_MON", "Forest",
  
  "SN_NKR", "Cropland",
  "SN_RAG", "Cropland",
  
  "NE_WAM", "Cropland-Savanna",
  
  "NE_WAF", "Savanna-Cropland",
  
  "ZA_KRU", "Savanna"
)

ecosystem_order <- c(
  "Wetland",
  "Grassland",
  "Forest",
  "Cropland",
  "Cropland-Savanna",
  "Savanna-Cropland",
  "Savanna"
)




# Join ecosystem + years----------------
stats <- stats %>%
  left_join(site_ecosystem, by = "site") %>%
  left_join(site_years, by = "site")

# quick check
if (any(is.na(stats$years))) {
  warning("Some sites are missing year information.")
}




# Site order and y positions----------------
site_levels <- stats %>%
  distinct(site, ecosystem) %>%
  arrange(factor(ecosystem, levels = ecosystem_order), site) %>%
  pull(site)

site_positions <- tibble(
  site = site_levels,
  y = rev(seq_along(site_levels))
)

stats <- stats %>%
  left_join(site_positions, by = "site")





# Group boundaries (optional subtle separators)-----------------
group_data <- stats %>%
  distinct(site, ecosystem, y) %>%
  group_by(ecosystem) %>%
  summarise(
    ymin = min(y),
    ymax = max(y),
    .groups = "drop"
  ) %>%
  arrange(factor(ecosystem, levels = ecosystem_order))

boundary_data <- group_data %>%
  mutate(yint = ymin - 0.5) %>%
  slice(1:(n() - 1))




# Convert to long format-----------------
plot_data <- stats %>%
  dplyr::select(site, y, years, model, cor, rmse, bias) %>%
  pivot_longer(
    cols = c(cor, rmse, bias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      cor  = "Correlation",
      rmse = "RMSE",
      bias = "Bias"
    ),
    metric = factor(metric, levels = c("Correlation", "RMSE", "Bias")),
    model = recode(
      model,
      Default = "Default",
      Reparam = "Trait-informed"
    ),
    model = factor(model, levels = c("Default", "Trait-informed"))
  )


# Make segment data--------------
seg_data <- plot_data %>%
  dplyr::select(site, y, metric, model, value) %>%
  pivot_wider(names_from = model, values_from = value)




# Reference line for Bias = 0----------------
ref_data <- tibble(
  metric = factor("Bias", levels = c("Correlation", "RMSE", "Bias")),
  xint = 0
)




# Left table data------------------
left_df <- stats %>%
  distinct(site, y, years) %>%
  arrange(desc(y)) %>%
  mutate(
    years_lab = ifelse(years %% 1 == 0, as.character(as.integer(years)), as.character(years))
  )

y_limits <- c(min(site_positions$y) - 0.5, max(site_positions$y) + 0.9)
y_header <- max(site_positions$y) + 0.55





# Left table: Site + Years---------------
left_table <- ggplot() +
  # subtle group separators
  geom_hline(
    data = boundary_data,
    aes(yintercept = yint),
    colour = "grey85",
    linewidth = 0.7
  ) +
  
  # headers
  annotate(
    "text",
    x = 0.02, y = y_header,
    label = "Site",
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +
  annotate(
    "text",
    x = 0.82, y = y_header,
    label = "Years",
    hjust = 1,
    fontface = "bold",
    size = 5
  ) +
  
  # site names
  geom_text(
    data = left_df,
    aes(x = 0.02, y = y, label = site),
    hjust = 0,
    size = 5.1,
    fontface = "bold",
    colour = "grey25"
  ) +
  
  # years column
  geom_text(
    data = left_df,
    aes(x = 0.82, y = y, label = years_lab),
    hjust = 1,
    size = 4.9,
    colour = "grey20"
  ) +
  
  scale_y_continuous(
    limits = y_limits,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, 0.90), clip = "off") +
  theme_void(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 6, 68, 6)
  )





# Main plot----------------
main_plot <- ggplot() +
  
  # subtle group separators
  geom_hline(
    data = boundary_data,
    aes(yintercept = yint),
    colour = "grey85",
    linewidth = 0.7
  ) +
  
  # zero reference for bias only
  geom_vline(
    data = ref_data,
    aes(xintercept = xint),
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.6
  ) +
  
  # connecting line between Default and Reparameterized
  geom_segment(
    data = seg_data,
    aes(
      x = Default,
      xend = `Trait-informed`,
      y = y,
      yend = y
    ),
    colour = "grey75",
    linewidth = 0.6
  ) +
  
  # Default = hollow circle
  geom_point(
    data = plot_data %>% filter(model == "Default"),
    aes(x = value, y = y, shape = model),
    size = 3.5,
    stroke = 1.2,
    colour = "#2C7FB8",
    fill = NA
  ) +
  
  # Reparameterized = hollow triangle
  geom_point(
    data = plot_data %>% filter(model == "Trait-informed"),
    aes(x = value, y = y, shape = model),
    size = 3.8,
    stroke = 1.2,
    colour = "#D95F02",
    fill = NA
  ) +
  
  facet_grid(
    . ~ metric,
    scales = "free_x",
    labeller = labeller(
      metric = as_labeller(
        c(
          "Correlation" = "bold(Correlation)",
          "RMSE" = "bold(RMSE~(g~C~m^{-2}~day^{-1}))",
          "Bias" = "bold(Bias~(g~C~m^{-2}~day^{-1}))"
        ),
        label_parsed
      )
    )
  ) +
  
  scale_y_continuous(
    breaks = site_positions$y,
    labels = rep("", nrow(site_positions)),
    limits = y_limits,
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_shape_manual(
    values = c(
      "Default" = 21,
      "Trait-informed" = 24
    )
  ) +
  
  guides(
    shape = guide_legend(
      override.aes = list(
        colour = "black",
        fill = NA,
        size = 4,
        stroke = 1.2
      )
    )
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "black", linewidth = 0.8),
    
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18, face = "bold"),
    
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    plot.margin = margin(15, 20, 15, 0)
  )




# Combine-------------------
final_plot <- left_table + main_plot +
  plot_layout(widths = c(2.0, 10.0))

# show
final_plot

# save
ggsave(
  filename = "Model_Performance_Comparison_GPP_with_years.png",
  plot = final_plot,
  width = 13,
  height = 13,
  dpi = 300,
  bg = "white"
)
