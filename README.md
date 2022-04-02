# Historic Fire NE USA
Graduate work from the University of Vermont: code, data and analysis

This research uses a global dynamic vegetation model (LPJ-GUESS) to determine whether past forest fires were driven by climate or intentional ignition by Indigenous people on the pre-colonial landscape of the Northeastern United States. The model requires 1200 years of input climate data in NCO file format. Output contains forest compositional data, including leaf area index, stem density and stand age. 

## Project Goals
The focus of this project is on the interaction between fire and vegetation. We aim to answer two questions: 1. Was fire a prominent driver of forest composition in the NE? 2. How frequent did fire appear to be on the landscape?  We will be using a dynamic global vegetation model, LPJ-GUESS, to predict forest composition over a range of annual fire probabilities. To control for the influence of climate, the model will be driven by 1200 years of paleoclimate data (850-2010). An annual fire probability that yields forest a composition most similar to pre-European settlement data will offer a clearer window into the role that fire played in past forests.

#### R_scripts: 
contains R scripts numbered in order that analysis took place. 
#### data: 
contains target datasets. FIA (forest inventory and analysis) data for present day forests. Witness tree data for forests at the time of European land surveys and settlement (1623-1850). There is also some LPJ-GUESS output in this directory. However, model runs are frequent and output is often deleted becasue it takes up so much storage. Some scripts are written for specific output so re-running the model may be necessary for the script to run. 

What follows is a brief description of LPJ-GUESS, the model implemented for this reasearch project. 

## LPJ-GUESS Ecosystem Model
LPJ-GUESS (Smith et al. 2001), is a dynamic vegetation model (DVM) optimised for regional to global applications. Vegetation dynamics are simulated as the emergent outcome of growth and competition for light, space and soil resources among woody plant individuals and a herbaceous understorey in each of 150 replicate patches representing ‘random samples’ of each simulated locality or grid cell. Multiple patches are simulated to account for the distribution within a landscape representative of the grid cell as a whole of vegetation stands differing in their histories of disturbance and stand development (succession). The simulated plants are classified into one of six plant functional types (PFTs) discriminated by growth form, phenology, photosynthetic pathway (C3 or C4), bioclimatic limits for establishment and survival and, for woody PFTs, allometry and life history strategy (Tables 2 and 3). The simulations of this study were carried out in ‘cohort mode,’ in which, for woody PFTs, cohorts of individuals recruited in the same patch in a given year are represented by a single average individual, and are thus assumed to retain the same size and form as they grow.
Primary production and plant growth follow the approach of LPJ-DGVM (Sitch et al. 2003). Canopy fluxes of carbon dioxide and water vapour are calculated by a coupled photosynthesis and stomatal conductance scheme based on the approach of BIOME3 (Haxeltine & Prentice 1996). The net primary production (NPP) accrued by an average individual plant each simulation year is allocated to leaves, fine roots and, for woody PFTs, sapwood, following a set of prescribed allometric relationships for each PFT, resulting in biomass, height and diameter growth (Sitch et al. 2003). Population dynamics (recruitment and mortality) are represented as stochastic processes, influenced by current resource status, demography and the life history characteristics of each PFT (Hickler et al. 2004). A detailed description of LPJ-GUESS is available in Smith et al. (2001). We used LPJ-GUESS version 4.1 which includes the PFT set and modifications described in Ahlström et al. (2012) and in tables 1 and 2. 

## Model Drivers
Paleoclimate data: Paleo meteorological data compiled by the Paleo Ecological Observatory Network (Rollins C. R. et al. 2019) will be used to drive LPJ-GUESS. It includes incident long and short wave radiation (W m-2), precipitation representing the sum of convective rainfall, stratiform rainfall, and snowfall (kg m-2 s-1), pressure at the surface (Pa), specific humidity measured at the lowest level of the atmosphere (kg kg-1), 2 meter near surface air temperature (K) and wind speed measured with a vertical coordinate in height of 10 m  (m s-1). For each meteorological driver, data is on a grid of half degree resolution from -100 to -60 longitude and 40 to 60 latitude (80 x 30 grid). Drivers run from 850-2010 on a daily time step. 

## Plant Functional Types (PFT’s)

Groups of plant species are defined by similarities in leaf physiognomy, phenology, temperature range and shade tolerance (Table 3). Each of these characteristics are further defined by compiling individual parameters (Table 2).  For example, shade tolerance is defined by a PFT’s rate of establishment, minimum photosynthetically available radiation reaching the forest floor, a minimum rate of growth before mortality, and sapwood to heartwood conversion rate. By altering inputs for each of these parameters, a PFT fit for either low or high light levels can be built.   
LPJ-GUEUSS version 4.1 has 15 global PFT’s: ten trees, two grasses and three shrubs. Of these, four tree species and one grass define vegetation found in the NE US (tables 2 and 3). Another two PFT’s were built to fit the region: temperate broadleaved shade intolerant (TBI) and temperate broadleaved shade intolerant fire resistant (TBIR). We have created three parameters to define TBIR: fire_survival_mod and growth_resp_cost, and seeders_sprouters_resilience.

Many of the scripts in this repository were written to analyse output from exploratory model runs. Specieifically, sensitivity analyses searching for the best input for these three new parameters defining TBIR. 

## Model Validation
To test the model’s ability to predict modern forests, output from LPJ-GUESS was first compared with Forest Inventory and Analysis data. This dataset is updated annually by the United States Forest Service and is freely available at www.fia.fs.fed.us. 

## Observed Forest Composition
Model output was validated using three datasets, representing the entire course of our model run. Forest inventory and analysis (FIA), a national forest census compiled by the united states forest service, provides a comparison for present-day composition. Witness tree data recorded by surveyors to mark property boundaries offers a window into species composition at the time of European settlement (1623-1850). These records still exist today as town proprietor data and has been collected by Charlie Cogbill (Codbill 2002). Finally, pollen and charcoal data taken from Neotoma Paleoecology database offer an indication of species composition and fire frequencies over the last 1200 years (the length of the model run). 





