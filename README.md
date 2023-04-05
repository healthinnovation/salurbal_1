# **SALURBAL MS224**

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# SALURBAL - Does socioeconomic advantage come with better environments? Socioeconomic disparities in PM2.5 exposure in eight large Latin American cities

## Study description
Air pollution caused by particulate matter, especially fine particles less than 2.5 μm in diameter, is a leading environmental hazard globally and causes millions of deaths and disability-adjusted life years every year. Latin America is highly urbanized, and air pollution causes thousands of deaths annually in the region, with three of its cities listed among the most polluted in the world. However, the literature related to the effects of particulate matter in Latin American cities is limited, and understanding the disparities in exposure to particulate matter is crucial to assess the vulnerability of certain groups within urban populations. This is an environmental cross-sectional study conducted in eight Latin American cities using census data and mean annual PM2.5 concentrations at high spatial resolution (census tract-level). Socioeconomic data was obtained from the most recent national census of each city (conducted between 2010 and 2017). To estimate the overall inequality in PM2.5 exposure across different socioeconomic variables, a Slope Index of Inequality (SII) was estimated. A quantile-g-computation mixture approach was conducted to better capture exposure-outcome variable-relationships and joint effects of all socio-economic variables on PM2.5 exposure.
![](https://github.com/healthinnovation/salurbal_1/blob/main/figures/fig1.png)

## Repository structure

1. [scripts](https://github.com/healthinnovation/salurbal_1/tree/main/scripts) -  Scripts used to process and analyze data
2. [data](https://github.com/healthinnovation/salurbal_1/tree/main/data) - Raw data used in the analysis
3. [geometries](https://github.com/healthinnovation/salurbal_1/tree/main/geometries) - Geometries used to analyze data and plot maps
4. [figures](https://github.com/healthinnovation/salurbal_1/tree/main/figures) - Figures in the main text
    - Figure 1. Countries included in the study.
    - Figure 2. Bivariate plot of PM2.5 annual mean concentration and the percentage of school attendance per census track (divided by terciles).
    - Figure 3. Slope index of inequality of each exposure per city at census track level 
    - Figure 4. Quantile G Computation scalled effects of all available variables per country on PM2.5
