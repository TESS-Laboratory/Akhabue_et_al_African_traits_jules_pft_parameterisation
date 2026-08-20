# Vcmax25 calculationfrom Karina's formula + slope/intercept visual ============================================================


# Clean session (optional but helpful)
rm(list = ls())

# libraries ------
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Enter your data exactly as shared ----
df <- tibble(
  PFT = c("BET-Tr","BET-Te","BDT","NET","NDT","C3","C4","ESh","DSh"),
  
  LMA_JULES = c(0.1039, 0.1403, 0.0823, 0.2263, 0.1006, 0.0498, 0.1370, 0.1515, 0.0550),
  TRY_LMA_MEDIAN = c(0.0898, 0.1069, 0.0965, 0.0195, 0.1006, 0.0724, 0.0569, 0.1306, 0.0702),
  
  NMASS_JULES = c(0.0170, 0.0144, 0.0210, 0.0115, 0.0186, 0.0219, 0.0113, 0.0136, 0.0238),
  TRY_NMASS_MEDIAN = c(0.0250, 0.0152, 0.0237, 0.0110, 0.0186, 0.0242, 0.0170, 0.0114, 0.0260),
  
  Vint_JULES = c(7.21, 3.90, 5.73, 6.32, 6.32, 6.42, 0.00, 14.71, 14.71),
  Vsl_JULES  = c(19.22, 28.40, 29.81, 18.15, 23.79, 40.96, 20.48, 23.15, 23.15)
)

# ---- 2) Karina's formula (EXACT) ----
# vcmax25 = ( vint + vsl * nmass * lma * 1000.0 ) * 1.0E-6
# Units:
#   lma   = kg m-2
#   nmass = kg/kg
# Output:
#   mol m-2 s-1 (JULES-ready)
vcmax25_karina <- function(vint, vsl, nmass, lma) {
  (vint + vsl * nmass * lma * 1000.0) * 1.0e-6
}

# ---- 3) Compute Na and vcmax25 for JULES traits and TRY medians ----
out <- df %>%
  mutate(
    # Leaf nitrogen per area (g N m-2)
    Na_gm2_JULES = NMASS_JULES * LMA_JULES * 1000.0,
    Na_gm2_TRY   = TRY_NMASS_MEDIAN * TRY_LMA_MEDIAN * 1000.0,
    
    # vcmax25 from Karina formula (mol m-2 s-1)
    vcmax25_mol_JULES   = vcmax25_karina(Vint_JULES, Vsl_JULES, NMASS_JULES, LMA_JULES),
    vcmax25_mol_TRYmed  = vcmax25_karina(Vint_JULES, Vsl_JULES, TRY_NMASS_MEDIAN, TRY_LMA_MEDIAN),
    
    # Convert to umol m-2 s-1 for plotting/interpretation
    vcmax25_umol_JULES  = vcmax25_mol_JULES  * 1e6,
    vcmax25_umol_TRYmed = vcmax25_mol_TRYmed * 1e6
  )

# Print a quick table (optional)
print(out %>%
        dplyr::select(PFT, Na_gm2_JULES, vcmax25_umol_JULES, Na_gm2_TRY, vcmax25_umol_TRYmed))

