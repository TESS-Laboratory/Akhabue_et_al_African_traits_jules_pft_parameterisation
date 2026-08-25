# African traits for JULES PFT parameterisation

This repository contains the analysis workflow used to support trait-informed parameterisation of African plant functional types (PFTs) for the JULES land surface model. 
The work brings together African plant species records, plant trait observations, PFT classification, climate–trait analysis, and model-ready parameter summaries to improve the representation of African ecosystems in land surface modelling.

The repository includes scripts for cleaning and harmonising plant trait data, assigning species to JULES-relevant PFTs, summarising key vegetation traits such as leaf mass per area (LMA) and leaf nitrogen content (Nmass), exploring how traits vary across climatic gradients, and preparing parameter values for JULES model experiments.

This work contributes to a broader effort to make African ecosystem data more usable for model evaluation, parameterisation, and decision-relevant environmental modelling.  


_________________________________________________________________________________________________________________________________________________________________________

The repository contains four folders as described below:

# scripts
This folder contains 7 scripts used for the analysis in this study and they follow in sequential order as follows;
    
    1_Trait_data_analysis.R
    Cleans and harmonises the plant trait data, including trait names and units, and extracts African observations of leaf mass per area (LMA) and leaf nitrogen concentration by mass (Nmass). The script links trait observations to plant functional types (PFTs), calculates PFT-level summary statistics,      maps the distribution of observations, and compares African trait distributions and medians with the corresponding JULES default parameter values.
    
    2_Trait_climate_variability.R
    Prepares the trait dataset matched with mean annual temperature (MAT) and mean annual precipitation (MAP) and visualises the climate space represented by the observations. It produces a hexagonal-density plot of the MAT–MAP climate envelope covered by the trait dataset.

    3_GAMM_model_analysis.R
    Fits generalised additive models to evaluate nonlinear variation in LMA and Nmass across MAT, MAP, and aridity index, using a three-dimensional tensor-product smooth and a species-level random-effect smooth. The script performs model diagnostics and generates predicted trait surfaces across MAT–     MAP space at selected levels of aridity index.
    
    4_JULES_reparameterization_analysis.R
    Processes observed flux-tower data together with output from the default and African trait-informed JULES simulations across the study sites. It harmonises temporal coverage and units, calculates correlation, bias and RMSE against observations, and produces site-level time-series comparisons for     GPP, ecosystem respiration, sensible heat, latent heat, evapotranspiration and reference evapotranspiration.
    
    5_metrics_compare_with_years.R
    Compares GPP performance between the default and trait-informed JULES simulations using correlation, RMSE and bias across the flux-tower sites. Sites are grouped by ecosystem type and accompanied by the number of observational years, producing a summary figure showing how model performance           changes following reparameterisation.
    
    6_vcmax and slope and intercept.R
    Calculates leaf nitrogen per unit area and the resulting maximum Rubisco carboxylation capacity at 25 °C (Vcmax25) for each PFT using both JULES default traits and African TRY-derived median LMA and Nmass values. The calculations retain the JULES PFT-specific intercept and slope parameters,          allowing the physiological consequences of changing the two trait parameters to be compared directly.
    
    7_site_map.R
    Creates a map of the African flux-tower sites used for the JULES evaluation and reparameterisation analysis. Sites are labelled by site code and distinguished according to ecosystem type, and the resulting map is saved as a publication-ready image.

    

# PFT_mapping_lookup_table
This folder contains the lookup table used for mapping species to PFT group. The lookup table is the published looked up table Akhabue, E.F., Cunliffe, A.M., Bett-Williams, K. et al. Critical classification parameters linking species to Plant Functional Type in African ecosystems. Sci Data 13, 336 (2026). https://doi.org/10.1038/s41597-026-06728-z

# Jules_default_simulation_output
This folder contains the output of the simulation runs of JULES default configuration

# Reparameterized_simulation_output
This folder contains the output of the simulation runs of the reparameterized configuration

