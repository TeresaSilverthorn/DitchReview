#### Data analysis for review of CO2 and N2O (and CH4) emissions from ditches and canals reported in the literature ####

# Load in necessary packages: 
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(rworldmap)
library(terra)
library(geodata)
library(kgc)
library(mapproj)
library(ggpmisc)
library(rstatix)

# Set working directory for figures
setwd("C:/Users/teres/Documents/Ditch lit review/DitchReview/Figures/")

#Load in lit review data from Google Sheet (saved on PC):

dat <- readxl::read_excel("C:/Users/teres/Documents/Ditch lit review/DitchReview/Data/Ditch_data_extraction_2024-03-22.xlsx", sheet="Data_extraction", range = "A1:AL61", na = "NA") # Replace character NAs with actual NAs  # set range if you want to exclude the "Not included" studies

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
colnames(dat)[colnames(dat) == "Mean_velocity_m_s-1"] <- "Mean_velocity_m_s"



#### insert Koppen Geiger climate data ####

# Round coordinates
dat$rndCoord.lon <- RoundCoordinates(dat$Longitude)
dat$rndCoord.lat <- RoundCoordinates(dat$Latitude)

# add a new column with KG climate zone
dat$KG_climatezone <- LookupCZ(dat)

# The Köppen climate classification divides climates into five main climate groups, with each group being divided based on patterns of seasonal precipitation and temperature, add a column for each main group:

dat <- dat %>%
  mutate(KG_climatezone = as.character(KG_climatezone)) %>%
  mutate(KGMain_climate_group = case_when(
    startsWith(KG_climatezone, "A") ~ "Tropical",
    startsWith(KG_climatezone, "B") ~ "Arid",
    startsWith(KG_climatezone, "C") ~ "Temperate",
    startsWith(KG_climatezone, "D") ~ "Continental",
    startsWith(KG_climatezone, "E") ~ "Polar",
    TRUE ~ NA_character_  # If none of the above conditions match, assign NA
  )) 
  
dat <- dat %>%
  mutate(KG_climatezone = as.factor(KG_climatezone)) %>% 
  mutate(KGMain_climate_group = as.factor(KGMain_climate_group)) 

#### Clean data ####
## velocity ## for now keep only the numeric entries

dat <- dat %>%
  filter(!grepl("[A-Za-z]", Mean_velocity_m_s)) %>%  # Filter out entries with letters
  mutate(Mean_velocity_m_s = as.numeric(Mean_velocity_m_s))  


## Soil type 
dat <- dat %>%
  mutate(Soil_type = case_when(
    grepl("Organic", Soil_type, ignore.case = TRUE) ~ "Organic",
    grepl("Mineral", Soil_type, ignore.case = TRUE) ~ "Mineral",
    TRUE ~ Soil_type  # Keep the original value if it doesn't match any condition
  ))

dat <- dat %>%
  mutate(Soil_type = as.factor(Soil_type)) 

## Hydrological regime
dat <- dat %>%
  mutate(Hydrological_regime = case_when(
    grepl("Perennial", Hydrological_regime, ignore.case = TRUE) ~ "Perennial",
    grepl("Intermittent", Hydrological_regime, ignore.case = TRUE) ~ "Intermittent",
    TRUE ~ NA_character_  # NA if it doesn't match any condition
  ))

dat <- dat %>%
  mutate(Hydrological_regime = as.factor(Hydrological_regime))

## Land use
# we could use a variation of the Corine Land Cover categories (made for Europe) : Artificial surfaces, Agricultural areas, Forest and semi-natural areas, Wetlands, Water bodies
levels(dat$Land_use)

dat <- dat %>%
  mutate(Land_useCLC = case_when(
    grepl("peat", Land_use, ignore.case = TRUE) ~ "Wetland",
    grepl("Forest", Land_use, ignore.case = TRUE) ~ "Natural_Forest",
    grepl("Sphagnum", Land_use, ignore.case = TRUE) ~ "Wetland",
    grepl("pasture", Land_use, ignore.case = TRUE) ~ "Agriculture",
    TRUE ~ Land_use  # Keep the original value if it doesn't match any condition
  ))  %>%
  select(-Land_use, everything(), Land_use, Land_useCLC) #reorder

dat <- dat %>%
  mutate(Land_useCLC = as.factor(Land_useCLC))

#### Ditch summary statistics ####
summary(dat$Elevation_masl)
sd(dat$Mean_width_m, na.rm=T)
sd(dat$Mean_water_depth_m, na.rm=T)
sd(dat$Mean_velocity_m_s, na.rm=T)
sd(dat$DO_mg_L, na.rm=T)
sd(dat$pH, na.rm=T)
sd(dat$EC_us_cm, na.rm=T)
sd(dat$DOC_mg_L, na.rm=T)
sd(dat$TN_mg_L, na.rm=T)

sum(!is.na(dat$TN_mg_L))

str(dat)
#### Plots #####

# Publication year frequency plot
hist(dat$Publication_year)

tiff("freq_pub_yr.tiff", units="in", width=6, height=4, res=300)
pub_yr <- ggplot(dat, aes(x = Publication_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(min(dat$Publication_year), max(dat$Publication_year), by = 1)) +   labs(x = "Publication Year", y = "Frequency") +  theme_minimal() + theme(axis.text.x = element_text(angle = 45,  hjust = 1), axis.ticks.x = element_line(color = "black"), text = element_text(size = 13), panel.grid.major= element_blank())
pub_yr
dev.off()

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

#### Global distribution ####

world <- map_data("world")

tiff("global_dis.tiff", units="in", width=6, height=4, res=300)
global_dis <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data = subset(dat, !duplicated(dat[, c("Longitude", "Latitude")])), aes(x=Longitude, y=Latitude), size= 4, colour=	"#097969", alpha=0.5, shape=16) +   theme_void() +  coord_map(xlim=c(-180,180)) 
global_dis
dev.off()


#### GHG data ####
summary(dat$g_CO2_m2_yr)
summary(dat$CH4_diffusive_g_CH4_m2_yr)
summary(dat$CH4_ebullitive_g_CH4_m2_yr)
summary(dat$g_N2O_m2_yr)

max(dat$g_N2O_m2_yr, na.rm=T)

#### Correlation plot ####
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

#corrplot(res2$r, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45, p.mat = res2$P, sig.level = 0.05, insig = "blank")
#not sure what the number of row error is about, because the plot looks fine


chart.Correlation(numeric_dat, histogram=TRUE, pch=19)



#### Sampling method ####
CO2method <- ggplot(dat, aes(x=GHG_sampling_method , y=g_CO2_m2_yr, fill=GHG_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2method

N2Omethod <- ggplot(dat, aes(x=GHG_sampling_method , y=g_N2O_m2_yr, fill=GHG_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Omethod

#### Trophic status ####
# Reorder factor levels more logically
dat$Nutrient_status <- factor(dat$Nutrient_status, levels=c("Mesotrophic", "Eutrophic", "Hypertrophic")) 


tiff("troph_count.tiff", units="in", width=4, height=4, res=300)
troph_count <- ggplot(dat, aes(x = Nutrient_status)) +
geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Trophic state") + theme_minimal() 
troph_count
dev.off()

tiff("CO2trophic.tiff", units="in", width=6, height=4, res=300)
CO2trophic <- ggplot(dat, aes(x=Nutrient_status , y=g_CO2_m2_yr, fill=Nutrient_status)) + xlab("Nutrient status") +  
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values=c("#00BFC4", "#A3A500","#F8766D"))
CO2trophic
dev.off()


tiff("N2Otrophic.tiff", units="in", width=6, height=4, res=300)
N2Otrophic <- ggplot(dat, aes(x=Nutrient_status , y=g_N2O_m2_yr , fill=Nutrient_status)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10() + scale_fill_manual(values=c("#00BFC4", "#A3A500","#F8766D")) + xlab("Nutrient status") + theme(legend.position="none")
N2Otrophic
dev.off()

#### Land use ####
tiff("landuse_count.tiff", units="in", width=4, height=4, res=300)
land_count <- ggplot(data = subset(dat, !is.na(Land_useCLC)), aes(x = Land_useCLC)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Land use (CLC)") + theme_minimal() 
land_count
dev.off()

tiff("CO2landuse.tiff", units="in", width=6, height=4, res=300)
CO2landuse <- ggplot(data = subset(dat, !is.na(Land_useCLC)), aes(x=reorder(Land_useCLC, g_CO2_m2_yr, na.rm=T) , y=g_CO2_m2_yr, fill=Land_useCLC)) + xlab("Land use") +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values=c("#F8766D", "#A3A500",  "#F564E3", "#00BFC4"))
CO2landuse
dev.off()

tiff("N2Olanduse.tiff", units="in", width=6, height=4, res=300)
N2Olanduse <- ggplot(data = subset(dat, !is.na(Land_useCLC)), aes(x=reorder(Land_useCLC, g_N2O_m2_yr, na.rm=T)  , y=g_N2O_m2_yr, fill=Land_useCLC)) + xlab("Land use") +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values=c("#F8766D", "#A3A500",  "#F564E3", "#00BFC4")) + scale_y_continuous(trans='log2')
N2Olanduse
dev.off()

#### Soil type ####
tiff("soiltype_count.tiff", units="in", width=2.5, height=4, res=300)
soil_count <- ggplot(data = subset(dat, !is.na(Soil_type)), aes(x = Soil_type)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Soil type") + theme_minimal() 
soil_count
dev.off()

tiff("CO2soil.tiff", units="in", width=4, height=4, res=300)
CO2soil <- ggplot(subset(dat, !is.na(Soil_type)), aes(x=Soil_type , y=g_CO2_m2_yr, fill=Soil_type)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values=c("#F8766D", "#A3A500"))
CO2soil
dev.off()

tiff("N2Osoil.tiff", units="in", width=4, height=4, res=300)
N2Osoil <- ggplot(subset(dat, !is.na(Soil_type)),  aes(x=Soil_type , y=g_N2O_m2_yr, fill=Soil_type)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() +  theme(legend.position="none") + scale_fill_manual(values=c("#F8766D", "#A3A500"))  
N2Osoil
dev.off()

#### Hydrological regime ####
tiff("hydroregime_count.tiff", units="in", width=4, height=4, res=300)
dry_count <- ggplot(dat, aes(x = Hydrological_regime)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Hydrological regime") + theme_minimal() 
dry_count
dev.off()

tiff("CO2hydro.tiff", units="in", width=4, height=4, res=300)
CO2hydro <- ggplot(dat, aes(x=Hydrological_regime , y=g_CO2_m2_yr, fill=Hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none")
CO2hydro
dev.off()

tiff("N2Ohydro.tiff", units="in", width=4, height=4, res=300)
N2Ohydro <- ggplot(dat, aes(x=Hydrological_regime , y=g_N2O_m2_yr, fill=Hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") + scale_y_continuous(trans='log2')
N2Ohydro
dev.off()

#### Climate zone ####
tiff("climate_count.tiff", units="in", width=6, height=4, res=300)
clim_count <- ggplot(data = subset(dat, !is.na(KGMain_climate_group)), aes(x = KG_climatezone, fill= KGMain_climate_group)) +
  geom_bar() +   labs(y = "Frequency", x = "Climate zones") + theme_minimal() 
clim_count
dev.off()

tiff("CO2KGclimate.tiff", units="in", width=6, height=4, res=300)
CO2KGclimate <- ggplot(data = subset(dat, !is.na(KGMain_climate_group)), aes(x=reorder(KGMain_climate_group, g_CO2_m2_yr, na.rm=T) , y=g_CO2_m2_yr, fill=KGMain_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() +  theme(legend.position="none") + xlab("Koppen Geiger climate zone") + scale_fill_manual(values=c( "#009E73", "#56B4E9", "#9370DB", "#FFA500") )
CO2KGclimate
dev.off()


levels(dat$KGMain_climate_group)

CO2climate <- ggplot(dat, aes(x=Climate_zone, y=g_CO2_m2_yr, fill=Climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2climate

tiff("N2OKGclimate.tiff", units="in", width=6, height=4, res=300)
N2OKGclimate <- ggplot(data = subset(dat, !is.na(KGMain_climate_group)),  aes(x=reorder(KGMain_climate_group, g_N2O_m2_yr, na.rm=T) , y=g_N2O_m2_yr, fill=KGMain_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()  +  theme(legend.position="none") + xlab("Koppen Geiger climate zone") + scale_fill_manual(values = c("Tropical" = "#9370DB", "Temperate" = "#56B4E9",   "Arid" = "#FFA500",  "Continental" = "#009E73")) # + scale_y_continuous(trans='log2')
N2OKGclimate
dev.off()

N2Oclimate <- ggplot(dat, aes(x=Climate_zone , y=g_N2O_m2_yr, fill=Climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Oclimate


#### Effects of climate variables ####
tiff("CO2temp.tiff", units="in", width=6, height=4, res=300)
CO2temp <- ggplot(dat, aes(x=MATemp_C , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp
dev.off()

# Plot scatter plot with line of best fit and equation
CO2temp <- ggplot(dat, aes(x = MATemp_C, y = g_CO2_m2_yr)) + 
  geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),   parse = TRUE, size = 5, color = "black", label.x = "right", label.y = "top") +    theme_minimal()
CO2temp


CO2precip <- ggplot(dat, aes(x=MAPrecip_mm , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2precip

N2Otemp <- ggplot(dat, aes(x=MATemp_C , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Otemp

N2Oprecip <- ggplot(dat, aes(x=MAPrecip_mm , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oprecip

#### GHG and elevation ####
CO2elev <- ggplot(dat, aes(x=Elevation_masl , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2elev

N2Oelev <- ggplot(dat, aes(x=Elevation_masl , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oelev

#### GHG and width/depth ####
CO2width <- ggplot(dat, aes(x=Mean_width_m , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2width

CO2depth <- ggplot(dat, aes(x=Mean_water_depth_m , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2depth

N2Owidth <- ggplot(dat, aes(x=Mean_width_m , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Owidth

N2Odepth <- ggplot(dat, aes(x=Mean_water_depth_m , y=g_N2O_m2_yr)) + geom_point(size = 2) + theme_minimal()
N2Odepth


#### GHG and water chemistry ####
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

#### CO2 vs CH4 ####
tiff("CO2_CH4.tiff", units="in", width=6, height=4, res=300)
CO2_CH4 <- ggplot(dat, aes(x=CH4_diffusive_g_CH4_m2_yr , y=g_CO2_m2_yr)) + geom_point(size = 2) + theme_minimal()
CO2_CH4
dev.off()

# Plot scatter plot with line of best fit and equation
CO2_CH4 <- ggplot(dat, aes(x = CH4_diffusive_g_CH4_m2_yr, y = g_CO2_m2_yr)) + 
  geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),   parse = TRUE, size = 5, color = "black", label.x = "right", label.y = "top") +    theme_minimal()
CO2_CH4


######################################

#### Run statistical tests ####

#To test differences in means between groups: one-way ANOVA (normal) or Kruskal-Wallis (non-normal)
# can use Shapiro Wilks test to test for normality: 

dat %>%
  group_by(KGMain_climate_group) %>%
  filter(!is.na(KGMain_climate_group)) %>%
  filter(KGMain_climate_group != "Arid") %>%
  shapiro_test(g_CO2_m2_yr) #If p > 0.05, then data is normally distributed

#Not normal: soil, nutrient status, land use, hydrological regime, KG main climate

dat %>%
  group_by(Land_useCLC) %>%
  filter(!is.na(Land_useCLC)) %>%
  shapiro_test(g_N2O_m2_yr) #If p > 0.05, then data is normally distributed

#sample sizes to small for nutrient status, land use and climate
# Not normal: soil, hydrological regime, 

#Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality

#Since most of the data does not follow the asumption of normality, we can run a non-paramentric test: the Kruskal-Walis

#### Kruskal wallis CO2 ####
kruskal.test(g_CO2_m2_yr ~ Soil_type, data = subset(dat, !is.na(Soil_type)) ) # no sig diff

kruskal.test(g_CO2_m2_yr ~ Nutrient_status, data = subset(dat, !is.na(Nutrient_status)) ) # no sig diff

kruskal.test(g_CO2_m2_yr ~ Land_useCLC, data = subset(dat, !is.na(Land_useCLC)) ) # p = 0.04

kruskal.test(g_CO2_m2_yr ~ KGMain_climate_group, data = subset(dat, !is.na(KGMain_climate_group)) ) # no sig diff

kruskal.test(g_CO2_m2_yr ~ Hydrological_regime, data = subset(dat, !is.na(Hydrological_regime)) ) #no sig diff

#check for the pairwise differences
pairwise.wilcox.test(dat$g_CO2_m2_yr[!is.na(dat$Land_useCLC)], dat$Land_useCLC[!is.na(dat$Land_useCLC)], p.adjust.method = "BH") # Urban and agriculture are different


#### Kruskal wallis N2O ####
kruskal.test(g_N2O_m2_yr ~ Soil_type, data = subset(dat, !is.na(Soil_type)) ) # no sig diff

kruskal.test(g_N2O_m2_yr ~ Nutrient_status, data = subset(dat, !is.na(Nutrient_status)) ) # p = 0.002

kruskal.test(g_N2O_m2_yr ~ Land_useCLC, data = subset(dat, !is.na(Land_useCLC)) ) # no sig diff

kruskal.test(g_N2O_m2_yr ~ KGMain_climate_group, data = subset(dat, !is.na(KGMain_climate_group)) ) # p = 0.002

kruskal.test(g_N2O_m2_yr ~ Hydrological_regime, data = subset(dat, !is.na(Hydrological_regime)) ) #no sig diff

#check for the pairwise differences
pairwise.wilcox.test(dat$g_N2O_m2_yr[!is.na(dat$Nutrient_status)], dat$Nutrient_status[!is.na(dat$Nutrient_status)], p.adjust.method = "BH") # no sig diff

pairwise.wilcox.test(dat$g_N2O_m2_yr[!is.na(dat$KGMain_climate_group)], dat$KGMain_climate_group[!is.na(dat$KGMain_climate_group)], p.adjust.method = "BH")

#######################################

#### CO2 equivalents ####

#Calculate CO2e of N2O
#the GWP of N2O is 273 over a 100 yr horizon (IPCC, 2021)
dat$N2O_CO2e <- (dat$g_N2O_m2_yr*273)

#Calculate CO2e of CH4
#the GWP of methane is 27 over a 100 yr horizon (IPCC, 2021)
dat$CH4_CO2e <- (dat$CH4_diffusive_g_CH4_m2_yr + dat$CH4_ebullitive_g_CH4_m2_yr)*27

dat$CH4_CO2e <- rowSums(cbind(dat$CH4_diffusive_g_CH4_m2_yr, dat$CH4_ebullitive_g_CH4_m2_yr), na.rm = TRUE) * 27

dat$CH4_CO2e[dat$CH4_CO2e == 0] <- NA


#Plot the proportion of each gas 
mean(dat$N2O_CO2e, na.rm=T)
sd(dat$N2O_CO2e, na.rm=T)

mean(dat$CH4_CO2e, na.rm=T)
sd(dat$CH4_CO2e, na.rm=T)

mean(dat$g_CO2_m2_yr, na.rm=T)
sd(dat$g_CO2_m2_yr, na.rm=T)

sum(!is.na(dat$g_CO2_m2_yr)) # counts


a <- mean(dat$N2O_CO2e, na.rm=T) + mean(dat$CH4_CO2e, na.rm=T) + mean(dat$g_CO2_m2_yr, na.rm=T)
mean(dat$g_CO2_m2_yr, na.rm=T)/a*100

# subset just the studies that measured all 3 gases


subset_CO2eq <- dat[complete.cases(dat$CH4_CO2e, dat$N2O_CO2e, dat$g_CO2_m2_yr), ]
# 16 obs

mean(subset_CO2eq$g_CO2_m2_yr, na.rm=T)
sd(subset_CO2eq$g_CO2_m2_yr, na.rm=T)

mean(subset_CO2eq$CH4_CO2e, na.rm=T)
sd(subset_CO2eq$CH4_CO2e, na.rm=T)

mean(subset_CO2eq$N2O_CO2e, na.rm=T)
sd(subset_CO2eq$N2O_CO2e, na.rm=T)

b <- mean(subset_CO2eq$N2O_CO2e, na.rm=T) + mean(subset_CO2eq$CH4_CO2e, na.rm=T) + mean(subset_CO2eq$g_CO2_m2_yr, na.rm=T)

mean(subset_CO2eq$N2O_CO2e, na.rm=T)/b*100
