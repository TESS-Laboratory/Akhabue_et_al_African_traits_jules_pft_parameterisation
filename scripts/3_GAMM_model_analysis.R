# Load libraries ----

library(tidyverse)
library(janitor)
library(mgcv)
library(scales)
library(viridis)


df <- read_csv("Trait_points_TerraClimate_MAP_PET_AI_1991_2020.csv") %>%
  clean_names()


df1 <- df %>%
  filter(!is.na(trait_name), !is.na(std_value), !is.na(mat_c), !is.na(map_mm), 
         !is.na(ai), !is.na(latitude), !is.na(longitude), !is.na(acc_species_name)) %>%
  filter(std_value > 0) %>%
  mutate(
    trait_name = as.factor(trait_name),
    trait_log10 = log10(std_value),
    acc_species_name = as.factor(acc_species_name)
  )

fit_gam_trait <- function(trait_nm, k_te = c(8, 8, 8)) {
  
  dat <- df1 %>% filter(trait_name == trait_nm)
  
  m <- gam(
    trait_log10 ~
      te(mat_c, map_mm, ai, k = k_te) +
      s(acc_species_name, bs = "re"),
    data = dat,
    method = "REML"
  )
  
  list(data = dat, model = m)
}

trait_lma   <- "Leaf Mass per Area"
trait_nmass <- "Leaf nitrogen (N) content per leaf dry mass"

fit_lma   <- fit_gam_trait(trait_lma)
fit_nmass <- fit_gam_trait(trait_nmass)

m_lma   <- fit_lma$model
m_nmass <- fit_nmass$model

summary(m_lma)
gam.check(m_lma)
concurvity(m_lma, full = TRUE)

summary(m_nmass)
gam.check(m_nmass)
concurvity(m_nmass, full = TRUE)




# Saving diagnostics plots and summary documents ----

png("M8_LMA_diagnostics_4panel.png", width = 8, height = 8, units = "in", res = 400)
par(mfrow = c(2,2))
qq.gam(m_lma, main = "QQ plot of residuals")
plot(residuals(m_lma) ~ m_lma$linear.predictors,
     main = "Residuals vs linear predictor", xlab = "Linear predictor", ylab = "Residuals")
hist(residuals(m_lma), main = "Histogram of residuals", xlab = "Residuals")
plot(fitted(m_lma), m_lma$y,
     main = "Response vs fitted values", xlab = "Fitted values", ylab = "Response")
dev.off()

png("M8_Nmass_diagnostics_4panel.png", width = 8, height = 8, units = "in", res = 400)
par(mfrow = c(2,2))
qq.gam(m_nmass, main = "QQ plot of residuals")
plot(residuals(m_nmass) ~ m_nmass$linear.predictors,
     main = "Residuals vs linear predictor", xlab = "Linear predictor", ylab = "Residuals")
hist(residuals(m_nmass), main = "Histogram of residuals", xlab = "Residuals")
plot(fitted(m_nmass), m_nmass$y,
     main = "Response vs fitted values", xlab = "Fitted values", ylab = "Response")
dev.off()

capture.output(summary(m_lma), file = "M8_summary_LMA.txt")
capture.output(summary(m_nmass), file = "M8_summary_Nmass.txt")

capture.output(gam.check(m_lma), file = "M8_gamcheck_LMA.txt")
capture.output(gam.check(m_nmass), file = "M8_gamcheck_Nmass.txt")

capture.output(concurvity(m_lma, full = TRUE), file = "M8_concurvity_LMA.txt")
capture.output(concurvity(m_nmass, full = TRUE), file = "M8_concurvity_Nmass.txt")



# Plot tensor smooth ----

plot_tensor_slices <- function(fit_obj,
                               file_stub,
                               trait_label = "trait",
                               slice_probs = c(0.25, 0.50, 0.75),
                               n = 120,
                               width = 8,
                               height = 4,
                               dpi = 400,
                               back_transform = FALSE) {
  
  model <- fit_obj$model
  dat   <- fit_obj$data %>%
    filter(
      !is.na(mat_c),
      !is.na(map_mm),
      !is.na(ai),
      !is.na(acc_species_name)
    ) %>%
    mutate(acc_species_name = as.factor(acc_species_name))
  
  # AI values to hold fixed
  ai_vals <- quantile(dat$ai, probs = slice_probs, na.rm = TRUE)
  
  ai_labels <- paste0(
    names(ai_vals),
    " AI = ",
    round(as.numeric(ai_vals), 2)
  )
  
  # Prediction grid: MAT x MAP for each selected AI value
  pred_grid <- expand_grid(
    mat_c = seq(min(dat$mat_c, na.rm = TRUE),
                max(dat$mat_c, na.rm = TRUE),
                length.out = n),
    
    map_mm = seq(min(dat$map_mm, na.rm = TRUE),
                 max(dat$map_mm, na.rm = TRUE),
                 length.out = n),
    
    ai_slice = seq_along(ai_vals)
  ) %>%
    mutate(
      ai = as.numeric(ai_vals)[ai_slice],
      ai_label = factor(ai_labels[ai_slice], levels = ai_labels),
      
      # Add a valid species level for prediction
      # The species random effect is excluded below
      acc_species_name = factor(
        levels(dat$acc_species_name)[1],
        levels = levels(dat$acc_species_name)
      )
    )
  
  
  obs_lims <- if (back_transform) {
    range(dat$std_value, na.rm = TRUE)
  } else {
    range(dat$trait_log10, na.rm = TRUE)
  }
  
  
  # Predict the population-level climate smooth
  # Excludes species-level random effect
  pred_grid$pred_log10 <- predict(
    model,
    newdata = pred_grid,
    type = "response",
    exclude = "s(acc_species_name)"
  )
  
  pred_grid <- pred_grid %>%
    mutate(
      pred_value = if (back_transform) 10^pred_log10 else pred_log10,
      pred_plot = scales::squish(pred_value, range = obs_lims)
    )
  
  fill_lab <- if (back_transform) {
    paste0("Predicted ", trait_label)
  } else {
    bquote("Predicted " * log[10] * "(" * .(trait_label) * ")")
  }
  
  p <- ggplot(pred_grid, aes(x = mat_c, y = map_mm, fill = pred_plot)) +
    geom_raster(interpolate = TRUE) +
    geom_contour(aes(z = pred_plot), colour = "white", alpha = 0.55, linewidth = 0.25) +
    facet_wrap(~ ai_label, nrow = 1) +
    scale_fill_viridis_c(name = fill_lab, limits = obs_lims
    ) +
    labs(
      x = "MAT (°C)",
      y = expression("MAP (mm yr"^{-1}*")")
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey90", colour = "grey50"),
      strip.text = element_text(face = "bold"),
      legend.position = "right"
    )
  
  ggsave(
    filename = paste0(file_stub, "_tensor_slices.pdf"),
    plot = p,
    width = width,
    height = height
  )
  
  ggsave(
    filename = paste0(file_stub, "_tensor_slices.png"),
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )
  
  return(p)
}


p_lma <- plot_tensor_slices(
  fit_obj = fit_lma,
  file_stub = "LMA_GAM",
  trait_label = "LMA"
)

p_nmass <- plot_tensor_slices(
  fit_obj = fit_nmass,
  file_stub = "Nmass_GAM",
  trait_label = "Nmass"
)


p_lma
p_nmass


