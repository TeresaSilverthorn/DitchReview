#### Data analysis for review of CO2 and N2O (and CH4) emissions from ditches and canals reported in the literature ####

# Load in necessary packages: 
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)


#Load in lit review data from Google Sheet (saved on PC):

dat <- readxl::read_excel("C:/Users/teres/Documents/Ditch lit review/DitchReview/Data/Ditch_data_extraction_2024-03-20.xlsx", sheet="Data_extraction", range = "A1:AL61", na = "NA") # Replace character NAs with actual NAs  # set range if you want to exclude the "Not included" studies

dat <- as.data.frame(dat)

str(dat)  #77 obs. of  39 variables
colnames(dat)

dat <- dat %>%
  mutate(Soil_type = as.factor(Soil_type))  %>%
  mutate(Country = as.factor(Country)) %>%
  mutate(Climate_zone = as.factor(Climate_zone))  %>%
  mutate(Land_use = as.factor(Land_use))  %>%
  mutate(Hydrological_regime = as.factor(Hydrological_regime)) %>%
  mutate(Nutrient_status = as.factor(Nutrient_status)) %>%
  mutate(GHG_sampling_method = as.factor(GHG_sampling_method))

# Rename columns

colnames(dat)[colnames(dat) == "g_CO2_m-2_yr-1"] <- "g_CO2_m2_yr"
colnames(dat)[colnames(dat) == "CH4_diffusive_g_CH4_m-2_yr-1"] <- "CH4_diffusive_g_CH4_m2_yr"
colnames(dat)[colnames(dat) == "CH4_ebullitive_g_CH4_m-2_yr-1"] <- "CH4_ebullitive_g_CH4_m2_yr"
colnames(dat)[colnames(dat) == "g_N2O_m-2_yr-1"] <- "g_N2O_m2_yr"
colnames(dat)[colnames(dat) == "DO_mg_L-1"] <- "DO_mg_L"
colnames(dat)[colnames(dat) == "EC_us_cm-1"] <- "EC_us_cm"
colnames(dat)[colnames(dat) == "DOC_mg_L-1"] <- "DOC_mg_L"
colnames(dat)[colnames(dat) == "TP_mg_L-1"] <- "TP_mg_L"
colnames(dat)[colnames(dat) == "TN_mg_L-1"] <- "TN_mg_L"
colnames(dat)[colnames(dat) == "Chl-a_mg_L-1"] <- "Chl-a_mg_L"

#### Plots #####

# Publication year frequency plot
hist(dat$Publication_year)

# Elevation
hist(dat$Elevation_masl)

# Mean annual temperature
hist(dat$MATemp_C)

# Mean annual precipitation
hist(dat$MAPrecip_mm)

# Mean width
hist(dat$Mean_width_m)

# Mean depth
hist(dat$Mean_water_depth_m)

### GHG data ###
summary(dat$g_CO2_m2_yr)
summary(dat$CH4_diffusive_g_CH4_m2_yr)
summary(dat$CH4_ebullitive_g_CH4_m2_yr)
summary(dat$g_N2O_m2_yr)



### Correlation plot
numeric_dat <- dat[sapply(dat, is.numeric)] # subset numeric data

numeric_dat <- numeric_dat  %>%
  select(g_CO2_m2_yr, CH4_diffusive_g_CH4_m2_yr, g_N2O_m2_yr, Elevation_masl, MATemp_C, MAPrecip_mm, Mean_width_m, Mean_water_depth_m, pH, EC_us_cm, DOC_mg_L) # have to subset otherwise error if including data with too many NAs

# flattenCorrMatrix
# function to flatten correlation matrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2 <- rcorr(as.matrix(numeric_dat)) # calculate correlation matrix
res2

flattenCorrMatrix(res2$r, res2$P)

res2$r # Extract the correlation coefficients

res2$P # Extract p-values

cor_dat <- cor(numeric_dat, use = "pairwise.complete.obs")

corrplot(res2$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = res2$P, sig.level = 0.05, insig = "blank")


chart.Correlation(my_data, histogram=TRUE, pch=19)



### Sampling method
CO2method <- ggplot(dat, aes(x=GHG_sampling_method , y=g_CO2_m2_yr, fill=GHG_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2method

CH4method <- ggplot(dat, aes(x=GHG_sampling_method , y=CH4_diffusive_g_CH4_m2_yr, fill=GHG_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CH4method

N2Omethod <- ggplot(dat, aes(x=GHG_sampling_method , y=g_N2O_m2_yr, fill=GHG_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Omethod

### GHG and trophic status

CO2trophic <- ggplot(dat, aes(x=Nutrient_status , y=g_CO2_m2_yr, fill=Nutrient_status)) +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2trophic

CH4trophic <- ggplot(dat, aes(x=Nutrient_status , y=CH4_diffusive_g_CH4_m2_yr , fill=Nutrient_status)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CH4trophic

N2Otrophic <- ggplot(dat, aes(x=Nutrient_status , y=g_N2O_m2_yr , fill=Nutrient_status)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Otrophic


### GHG and land use

CO2landuse <- ggplot(dat, aes(x=Land_use , y=g_CO2_m2_yr, fill=Land_use)) +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2landuse

CH4landuse <- ggplot(dat, aes(x=Land_use , y=CH4_diffusive_g_CH4_m2_yr, fill=Land_use)) +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CH4landuse

N2Olanduse <- ggplot(dat, aes(x=Land_use , y=g_N2O_m2_yr, fill=Land_use)) +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Olanduse


### Hydrological regime
CO2hydro <- ggplot(dat, aes(x=Hydrological_regime , y=g_CO2_m2_yr, fill=Hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2hydro

CH4hydro <- ggplot(dat, aes(x=Hydrological_regime , y=CH4_diffusive_g_CH4_m2_yr, fill=Hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CH4hydro

N2Ohydro <- ggplot(dat, aes(x=Hydrological_regime , y=g_N2O_m2_yr, fill=Hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()  + scale_y_log10()
N2Ohydro

### Effects of climate variables
CO2temp <- ggplot(dat, aes(x=MATemp_C , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2temp

CH4temp <- ggplot(dat, aes(x=MATemp_C , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4temp

N2Otemp <- ggplot(dat, aes(x=MATemp_C , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Otemp

CO2precip <- ggplot(dat, aes(x=MAPrecip_mm , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2precip

CH4precip <- ggplot(dat, aes(x=MAPrecip_mm , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4precip

N2Oprecip <- ggplot(dat, aes(x=MAPrecip_mm , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oprecip

### GHG and elevation 
CO2elev <- ggplot(dat, aes(x=Elevation_masl , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2elev

CH4elev <- ggplot(dat, aes(x=Elevation_masl , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4elev

N2Oelev <- ggplot(dat, aes(x=Elevation_masl , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oelev

### GHG and width/depth
CO2width <- ggplot(dat, aes(x=Mean_width_m , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2width

CO2depth <- ggplot(dat, aes(x=Mean_water_depth_m , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2depth

CH4width <- ggplot(dat, aes(x=Mean_width_m , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4width

CH4depth <- ggplot(dat, aes(x=Mean_water_depth_m , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4depth

N2Owidth <- ggplot(dat, aes(x=Mean_width_m , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Owidth

N2Odepth <- ggplot(dat, aes(x=Mean_water_depth_m , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Odepth



### GHG and water chemistry
## CO2
CO2DO <- ggplot(dat, aes(x=DO_mg_L , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2DO

CO2pH <- ggplot(dat, aes(x=pH , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2pH

CO2EC <- ggplot(dat, aes(x=EC_us_cm , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2EC

CO2DOC <- ggplot(dat, aes(x=DOC_mg_L , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2DOC

CO2TP <- ggplot(dat, aes(x=TP_mg_L , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2TP

CO2TN <- ggplot(dat, aes(x=TN_mg_L , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2TN

## CH4
CH4DO <- ggplot(dat, aes(x=DO_mg_L , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4DO

CH4pH <- ggplot(dat, aes(x=pH , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4pH

CH4EC <- ggplot(dat, aes(x=EC_us_cm , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4EC

CH4DOC <- ggplot(dat, aes(x=DOC_mg_L , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4DOC

CH4TP <- ggplot(dat, aes(x=TP_mg_L , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4TP

CH4TN <- ggplot(dat, aes(x=TN_mg_L , y=CH4_diffusive_g_CH4_m2_yr)) + geom_point(size = 2) + theme_minimal()
CH4TN

## N2O
N2ODO <- ggplot(dat, aes(x=DO_mg_L , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODO

N2OpH <- ggplot(dat, aes(x=pH , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2OpH

N2OEC <- ggplot(dat, aes(x=EC_us_cm , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2OEC

N2ODOC <- ggplot(dat, aes(x=DOC_mg_L , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODOC

N2OTP <- ggplot(dat, aes(x=TP_mg_L , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2OTP

N2OTN <- ggplot(dat, aes(x=TN_mg_L , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2OTN




