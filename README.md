# DitchReview

#Data and code associated with the manuscript "The importance of ditches and canals in global inland water greenhouse gas budgets" (working title) 

#This repository includes the R script "Ditch_data_analysis.R", used for data analysis and visualization, as well as the .csv data file "Ditch_data_extraction_2024-07-11.csv". 

#Data dictionary for .csv file: 

#Authors: Data authors

#Title: Data source title	

#Publication_year: Year of publication (or data collection, if unpublished)	

#Publication	Country: Country of publication (or data collection, if unpublished)

#Latitude: Study site latitude in decimal degrees	

#Longitude: study site longitude in decimal degrees	

#Site_name: Name or descriptor of ditch site	

#n_sites: numer of data points averaged per ditch site	

#Sampling_period: Time period where GHG sampling occured	

#Sampling_frequency; Frequency of GHG sampling	

#Elevation_masl: Elevation in m above sea leve. If not mentioned in the study, estimated via Google Earth and the reported coordinates of the sites (or as near as possible). 	

#MATemp_C: Mean annual temperature in degrees celsius. Usually this data is reported as a historic average, rather than the average for the study period, and the latter was less frequently available. If not reported in the study, historic values from another study in the same (or nearby) location is reported. 	

#MAPrecip_mm: Mean annual precipitation in mm. Usually this data is reported as a historic average, rather than the average for the study period, and the latter was less frequently available. If not reported in the study, historic values from another study in the same (or nearby) location is reported.

#MAEvap_mm: Mean annual evaporation in mm. Usually this data is reported as a historic average, rather than the average for the study period, and the latter was less frequently available. If not reported in the study, historic values from another study in the same (or nearby) location is reported.

#Climate_zone: The Koppen Geiger climate zone classification using the coordinates of the study locations. 	

#Mean_width_m: Ditch width in m. When this was not reported by the authors, we used an average of several measurements of the study ditch on Google Earth. In some cases the provided coordinates were not sufficient to identify the specific ditches, or the satellite imagery was difficult to interpret (e.g. trees covering ditches) and width measurements are not included.	

#Mean_water_depth_m: Reported average depth of water in the ditch in m.	

#Mean_velocity_m_s-1: Mean water velocity in m/s. If the water velocity was described qualitatively rather than quantitatively, we interpolated these for numerical values. “Slow flow” (n = 2) was replaced with the 1% percentile of all the velocity values (0.013 m s-1). “Low flow” (n = 2) was replaced with the lowest recorded velocity (0.01 m s-1). “Low/no flow” (n = 3) and “sluggish flow” (n = 2) were replaced with a value half of the lowest reported velocity (0.005 m s-1). “Standing  water” was replaced with 0 m s-1. 	

#Mean_discharge_m3_s-1: Mean discharge in m3/s. 

#Instream vegetation? Presence or absence of in-stream vegetation from site descriptions and/or photos. 	

#Land_use: agriculture, natural/forest, urban, or wetland. Based on the Corine Land Cover classification system.

#Soil_type: Organic or mineral soil dominating the catchment. We used a simple classification of mineral (n = 58) or organic soil (n = 61) dominating the ditch catchment. We classified a soil as organic if it was a peat soil or any soil with organic matter as the major portion of the surface soil horizons. 	

#Nutrient_status: aka trophic status. Trophic status determined using the IPCC table 7.11 from 2019 refinement, based on Smith et al 1999, using nutrient concentration data (TP, TN, chl-a, secchi depth). If not available, trophic status was decided based on site descriptions (e.g. current or historical use of fertilizer) or satellite imagery (i.e. sites in intensive agricultural and urban areas are likely to be eutrophic).	

#Hydrological_regime: We determined the hydrological status of the study ditch based on site descriptions of water flow or measurements of water table depth. If a site was dry with no surface water flow for at least one day per year, it was classified as non-perennial. If a site had surface water all year round, it was classified as perennial.   

#GHG_sampling_method: chamber, dissoled concentration, or both. 	

#g_CO2_m-2_yr-1: Carbon dioxide flux in g CO2 m-2 yr-1

#CH4_diffusive_g_CH4_m-2_yr-1: Diffusive methane flux in g CH4 m-2 yr-1

#CH4_ebullitive_g_CH4_m-2_yr-1	Ebullitive methane flux in g CH4 m-2 yr-1

#g_N2O_m-2_yr-1: nitrous oxide flux in g N2O m-2 yr-1	

#Flux_conversion_notes: Any notes related to conversion to annual fluxes. Calculations following the methodology of Peacock et al 2021 Env R Let.: For boreal sites: Daily flux rate is multiplied by the growing season length, if not given, is assumed to be the length of the measurement period, or estimated from a similar/nearby site. Temperate sites: If included repeated measurements, the mean daily flux is multiplied by 365. For sub/tropical sites: as above; if only a single campaign, we assumed fluxes were indicative of the annual mean. 

#DO_mg_L-1: Dissolved oxygen concentration in mg/L	

#pH: water pH	

#EC_us_cm-1	: Water electrical conductivity in us/cm

#DOC_mg_L-1	: Dissolved organic carbon concentration in mg/L

#TP_mg_L-1	: Total phosphorous concentration in mg/L

#TN_mg_L-1	: total nitrogen concentration in mg/L

#Nitrate_mg_L	: nitrate concentration in mg/L

#Chl-a_mg_L-1	: Chlorophyll-a concentration in mg/L

#Secchi_depth_m	: Sechi disk depth in m

#Notes : Relevant notes









