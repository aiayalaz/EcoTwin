Temporal Downscaling Methods
----------------------------
This repository implements three methods for temporally downscaling daily meteorological data to hourly resolution.

All methods downscale the following core variables:

Air temperature
Relative humidity
Wind speed
Incoming shortwave radiation
Precipitation

Additional climate variables can be included depending on data availability (Methods 1 and 2).

Method 1 — GRNN-based approach (Ayala et al., 2020)
https://doi.org/10.5194/hess-24-3311-2020

This method applies a Generalized Regression Neural Network (GRNN) to temporally disaggregate daily meteorological data into hourly values.
The GRNN is a non-parametric, data-driven model that learns relationships directly from observations without assuming a predefined functional form. It is trained using historical meteorological data and predicts hourly values based on similarity in feature space.

Inputs:
- Daily aggregated meteorological variables: daily mean, minimum and maximum air temperature; mean relative humidity, wind speed and incoming shortwave radiation; total precipitation.
- Time-related predictors: sine/cosine representation of hour and day of year, daylight flag, time since sunrise, time until sunset, solar hour angle

Method principle: Similarity between input vectors is measured using Euclidean distance. Observations that are closer in feature space receive higher weights in the prediction, allowing the model to capture nonlinear relationships and realistic diurnal cycles.

Performance: This method shows the best overall performance among the three approaches.

Method 2 — Teddy tool (Zabel et al., 2023)
https://doi.org/10.5194/gmd-16-5383-2023

This method identifies, for each model day, the most similar day from a high-resolution reference dataset within a user-defined day-of-year window.
Similarity is computed based on:
- Ranked absolute errors across multiple variables
- Precipitation occurrence (wet/dry state)
- Inter-day wet/dry persistence
Procedure
- Identify most similar reference day
- Extract observed hourly diurnal cycles
- Normalize cycles to remove absolute magnitude
- Rescale to match target daily values
- Apply corrections for consistency and precipitation behavior

The original implementation is in MATLAB and was designed for ISIMIP future projections using WFDE5 as reference. An R implementation is used in this project.

Method 3 — MELODIST (Förster et al., 2016)
https://doi.org/10.5194/gmd-9-2315-2016

MELODIST disaggregates daily meteorological data into hourly values using variable-specific empirical and physically based methods. The model is originally implemented in Python and executed here from R.

Methods
- Air temperature: Sinusoidal interpolation between daily minimum and maximum (sine_min_max), adjusted using solar position (sun_loc_shift).
- Relative humidity: Derived from air temperature using a dew point regression approach.
- Wind speed_ Distributed using a cosine-based diurnal cycle.
- Incoming shortwave radiation: Estimated from potential solar radiation, accounting for solar geometry and daylight conditions.
- Precipitation: Disaggregated using a cascade model that preserves intermittency and sub-daily variability.

General Notes:

All three methods downscale the same core meteorological variable susing example data from Lake Erken (Sweden).
Methods 1 and 2 can incorporate additional climate variables if available.
Method 3 applies variable-specific physical parameterizations.

Training and validation setup:
- Method 1 (GRNN): 8-year dataset total, 6 years training and 2 years testing. Selected R package: GRNNs (chosen for computational efficiency and strong performance compared to alternatives)
- Method 2 (Teddy tool): 30-year dataset, 80% reference / 20% validation
- Method 3 (MELODIST): 30-year dataset, 80% reference / 20% validation
