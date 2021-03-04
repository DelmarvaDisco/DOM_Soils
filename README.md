# Drivers of wetland microbrial communities

### Repository Purpose
This repository contains calculations supporting Wardinski et al., a study of soil organic matter in seasonally inundated Delmarva Bay wetlands in the Mid-Atlantic US. The study specifically quantifies the extractable dissolved organic matter (DOM) accross gradients in soil moisture and associated redox conditions.

### Description of calculation
The goal of this repository is to estimate water level at each of Wardinski's sampling locations. To complete this calculation, we utilized a topographic survey, continuous wetland and upland water level data, and a simple interpolation scheme. The topographic data was collected using a simple laser level survey (see the xs_survey.csv in the data fodler). Continuous water level was collected in surface water and groundwater wells in the wetland and upland, respectively. Finally, water level at each site [and timestep] was estimated using a linear interpolation procedure. The interpolation occurred between the wetland edge and water surface in the upland well. Notably, we accounted for the dynamic expansion and contraction of the wetland edge over time. 

### Repo description

The *data* folder contains both water level data (*waterLevel.csv*) and cross section elevation data (*xs_survey.csv*). The *analysis* folder contains analyses divided into steps denoted by Roman numeral prefixes. Header files in each script describe their specific purpose. 

### Manuscript Citation
>Wardinski K., Scott D, Mclaughlin DL, Hotchkiss ER, Desmond K, Jones CN, Palmer M. Dissolved Organic Matter Sources from Soil Horizons with Varying Hydrology and Distance from Wetland Edge. Planned submission: Fall 2021.
