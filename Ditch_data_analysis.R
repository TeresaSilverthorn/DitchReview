#### Data analysis for review of CO2 and N2O (and CH4) emissions from ditches and canals reported in the literature ####

# Load in necessary packages: 
library(readxl)
library(dplyr)
library(tidyr)
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
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(vtable)
library(janitor)
library(viridisLite)
library(viridis)

# Set working directory for figures
setwd("C:/Users/teres/Documents/Ditch lit review/DitchReview/Figures/")

#Load in lit review data from Google Sheet (saved on PC):

dat <- readxl::read_excel("C:/Users/teres/Documents/Ditch lit review/DitchReview/Data/Ditch_data_extraction_2024-05-15.xlsx", sheet="Data_extraction", range = "A1:AO113", na = "NA") # Replace character NAs with actual NAs  # set range if you want to exclude the "Not included" studies

dat <- as.data.frame(dat)

str(dat)  #112 obs. of  41 variables
colnames(dat)
dat <- clean_names(dat)  #clean names: no capitals, only _
colnames(dat)

length(unique(dat$authors)) #77 unique studies


# Rename columns

colnames(dat)[colnames(dat) == "p_h"] <- "pH"
colnames(dat)[colnames(dat) == "mean_velocity_m_s_1"] <- "mean_velocity_m_s"
colnames(dat)[colnames(dat) == "mean_discharge_m3_s_1"] <- "mean_discharge_m3_s"
colnames(dat)[colnames(dat) == "g_co2_m_2_yr_1"] <- "g_co2_m_2_yr"
colnames(dat)[colnames(dat) == "ch4_diffusive_g_ch4_m_2_yr_1"] <- "ch4_diff_g_ch4_m_2_yr"
colnames(dat)[colnames(dat) == "ch4_ebullitive_g_ch4_m_2_yr_1"] <- "ch4_ebull_g_ch4_m_2_yr"
colnames(dat)[colnames(dat) == "g_n2o_m_2_yr_1"] <- "g_n2o_m_2_yr"
colnames(dat)[colnames(dat) == "do_mg_l_1"] <- "do_mg_l"
colnames(dat)[colnames(dat) == "ec_us_cm_1"] <- "ec_us_cm"
colnames(dat)[colnames(dat) == "doc_mg_l_1"] <- "doc_mg_l"
colnames(dat)[colnames(dat) == "tp_mg_l_1"] <- "tp_mg_l"
colnames(dat)[colnames(dat) == "tn_mg_l_1"] <- "tn_mg_l"
colnames(dat)[colnames(dat) == "chl_a_mg_l_1"] <- "chl_a_mg_l"


#Make columns factors
dat <- dat %>%
  mutate(soil_type = as.factor(soil_type))  %>%
  mutate(country = as.factor(country)) %>%
  mutate(climate_zone = as.factor(climate_zone))  %>%
  mutate(land_use = as.factor(land_use))  %>%
  mutate(hydrological_regime = as.factor(hydrological_regime)) %>%
  mutate(nutrient_status = as.factor(nutrient_status)) %>%
  mutate(ghg_sampling_method = as.factor(ghg_sampling_method))


#### insert Koppen Geiger climate data ####

# Round coordinates
dat$rndCoord.lon <- RoundCoordinates(dat$longitude)
dat$rndCoord.lat <- RoundCoordinates(dat$latitude)


# add a new column with KG climate zone
dat$kg_climatezone <- LookupCZ(dat)

# The Köppen climate classification divides climates into five main climate groups, with each group being divided based on patterns of seasonal precipitation and temperature, add a column for each main group:

dat <- dat %>%
  mutate(kg_climatezone = as.character(kg_climatezone)) %>%
  mutate(kg_main_climate_group = case_when(
    startsWith(kg_climatezone, "A") ~ "Tropical",
    startsWith(kg_climatezone, "B") ~ "Arid",
    startsWith(kg_climatezone, "C") ~ "Temperate",
    startsWith(kg_climatezone, "D") ~ "Continental",
    startsWith(kg_climatezone, "E") ~ "Polar",
    TRUE ~ NA_character_  # If none of the above conditions match, assign NA
  )) 
  
dat <- dat %>%
  mutate(kg_climatezone = as.factor(kg_climatezone)) %>% 
  mutate(kg_main_climate_group = as.factor(kg_main_climate_group)) 

#### Clean data ####
## velocity : make the non-numeric entries numeric

#determine the lowest 10% percentile of the numeric values

dat$num_mean_velocity_m_s <- as.numeric(dat$mean_velocity_m_s) # subset only the numeric values
quantile(dat$num_mean_velocity_m_s, probs = 0.1, na.rm=T) # 10% 0.035
quantile(dat$num_mean_velocity_m_s, probs = 0.01, na.rm=T) # 0.01294
quantile(dat$num_mean_velocity_m_s, probs = 0.9, na.rm=T) # 90% 0.321

#replace "slow" flow with the 1% percentile value: 0.013
#replace "low" flow with the lowest recorded velocity: 0.01
#replace "low/no" and "sluggish" flow with half of the lowest recorded velocity:  0.005
#replace "standing" with 0


dat <- dat %>%
mutate(mean_velocity_m_s = case_when(
  grepl("standing", mean_velocity_m_s, ignore.case = TRUE) ~ "0.0",
  grepl("sluggish", mean_velocity_m_s, ignore.case = TRUE) ~ "0.005",
  grepl("low / no flow", mean_velocity_m_s, ignore.case = TRUE) ~ "0.005",
  grepl("slow", mean_velocity_m_s, ignore.case = TRUE) ~ "0.013",
  grepl("low", mean_velocity_m_s, ignore.case = TRUE) ~ "0.01",
  TRUE ~ mean_velocity_m_s  # Keep the original value if it doesn't match any condition
))  %>%
    select(-num_mean_velocity_m_s) %>%
  mutate(mean_velocity_m_s = as.numeric(mean_velocity_m_s))


## Soil type
levels(dat$soil_type)

dat <- dat %>%
  mutate(soil_type = case_when(
    grepl("Organic", soil_type, ignore.case = TRUE) ~ "Organic",
    grepl("Mineral", soil_type, ignore.case = TRUE) ~ "Mineral",
    grepl("peat", soil_type, ignore.case = TRUE) ~ "Organic",
    TRUE ~ soil_type  # Keep the original value if it doesn't match any condition
  ))

dat <- dat %>%
  mutate(soil_type = as.factor(soil_type)) 

## Hydrological regime

levels(dat$hydrological_regime)

dat <- dat %>%
  mutate(hydrological_regime = case_when(
    grepl("Perennial", hydrological_regime, ignore.case = TRUE) ~ "Perennial",
    grepl("Intermittent", hydrological_regime, ignore.case = TRUE) ~ "Intermittent",
    TRUE ~ NA_character_  # NA if it doesn't match any condition
  ))

dat <- dat %>%
  mutate(hydrological_regime = as.factor(hydrological_regime))

## Land use
# we could use a variation of the Corine Land Cover categories (made for Europe) : Artificial surfaces, Agricultural areas, Forest and semi-natural areas, Wetlands, Water bodies
levels(dat$land_use)

dat <- dat %>%
  mutate(land_use_clc = case_when(
    grepl("peat", land_use, ignore.case = TRUE) ~ "Wetland",
    grepl("Forest", land_use, ignore.case = TRUE) ~ "Natural_Forest",
    grepl("Natural", land_use, ignore.case = TRUE) ~ "Natural_Forest",
    grepl("Sphagnum", land_use, ignore.case = TRUE) ~ "Wetland",
    grepl("pasture", land_use, ignore.case = TRUE) ~ "Agriculture",
    grepl("Agriculture", land_use, ignore.case = TRUE) ~ "Agriculture",
    TRUE ~ land_use  # Keep the original value if it doesn't match any condition
  ))  %>%
  select(-land_use, everything(), land_use, land_use_clc) #reorder

dat <- dat %>%
  mutate(land_use_clc = as.factor(land_use_clc))

levels(dat$land_use_clc)

## GHG sampling method

levels(dat$ghg_sampling_method)

dat <- dat %>%
  mutate(ghg_sampling_method = case_when(
    grepl("and", ghg_sampling_method, ignore.case = TRUE) ~ "Both",
    grepl("transparent", ghg_sampling_method, ignore.case = TRUE) ~ "Chamber",
    grepl("CO2, CH4", ghg_sampling_method, ignore.case = TRUE) ~ "Both",
    TRUE ~ ghg_sampling_method  # Keep the original value if it doesn't match any condition
  )) 

dat <- dat %>%
  mutate(ghg_sampling_method = as.factor(ghg_sampling_method))

levels(dat$ghg_sampling_method)

#### Add a new variable ratio of stream depth to stream width

dat$depth_width <-  dat$mean_water_depth_m / dat$mean_width_m 


#### Ditch summary statistics ####
#Summary statistic table from vtable:

st(dat, vars = c('elevation_masl','mean_width_m', 'mean_water_depth_m', 'mean_velocity_m_s', 'mean_discharge_m3_s', 'do_mg_l', 'pH', 'ec_us_cm', 'doc_mg_l', 'tn_mg_l', 'nitrate_mg_l',  'tp_mg_l', 'chl_a_mg_l' ))



#### Plots #####

# Publication year frequency plot
hist(dat$publication_year)

tiff("freq_pub_yr.tiff", units="in", width=6, height=4, res=300)

pub_yr <- ggplot(dat, aes(x = publication_year)) +
  geom_histogram(binwidth = 1, fill = "#304F3B", color = "black") +
  scale_x_continuous(breaks = seq(min(dat$publication_year), max(dat$publication_year), by = 2)) +   labs(x = "Publication Year", y = "Frequency") +  theme_minimal() + theme(axis.text.x = element_text(angle = 45,  hjust = 1), axis.ticks.x = element_line(color = "black"), text = element_text(size = 16), panel.grid.major= element_blank())
pub_yr

dev.off()

# Elevation
hist(dat$elevation_masl)

# Mean annual temperature
hist(dat$ma_temp_c)

# Mean annual precipitation
hist(dat$ma_precip_mm)

# Mean width
hist(dat$mean_width_m)

# Mean depth
hist(dat$mean_water_depth_m)



#### Global distribution ####

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


tiff("global_dis.tiff", units="in", width=5, height=6, res=300)

ggplot(data = world) +
  geom_sf(fill = "grey", color=NA) +  theme_void() +
  geom_point(data = filter(distinct(dat, longitude, latitude), !is.na(longitude)), aes(x = longitude, y = latitude), size= 4, colour=	"#304F3B", alpha=0.5, shape=16) +
  xlab("Longitude") + ylab("Latitude") 

dev.off() 



#### GHG data ####
#### GHG summary statistics ####

st(dat, vars = c("g_co2_m_2_yr",  "g_n2o_m_2_yr")) #GHG summary stats


# Convert to CO2-C for comparison with inland waters
x <- mean(dat$g_co2_m_2_yr, na.rm=T)
y <- sd(dat$g_co2_m_2_yr, na.rm=T)

x/44.01*12.01 # convert to CO2-C
y/44.01*12.01 # convert to CO2-C

a <-mean(dat$g_n2o_m_2_yr, na.rm=T)
b <-sd(dat$g_n2o_m_2_yr, na.rm=T)

a/44.01*28.02 # convert to N2O-N
b/44.01*28.02 # convert to N2O-N

summary(dat$g_co2_m_2_yr)
summary(dat$ch4_diff_g_ch4_m_2_yr)
summary(dat$ch4_ebull_g_ch4_m_2_yr)
summary(dat$g_n2o_m_2_yr)

max(dat$g_n2o_m_2_yr, na.rm=T)

#############################################################
#### Correlation plot ####
numeric_dat <- dat[sapply(dat, is.numeric)] # subset numeric data

numeric_dat <- numeric_dat %>%
  select(-rndCoord.lon, -rndCoord.lat, -publication_year, -n_sites, -ma_evap_mm)

colnames(numeric_dat) <- c("lat", "lon", "masl",  "temp", "precip", "width", "depth", "veloc", "discharge", "CO2", "CH4_diff", "CH4_ebull",  "N2O", "DO",  "pH", "EC", "DOC", "TP", "TN", "NO3", "chl-a", "depth_width")

numeric_dat <- numeric_dat %>%
  select(CO2, N2O, CH4_diff, CH4_ebull,  everything()) # reorder to put GHGs in first spots

#numeric_dat <- numeric_dat  %>%
 # select(g_co2_m_2_yr, CH4_diffusive_g_CH4_m2_yr, g_n2o_m_2_yr, Elevation_masl, ma_temp_c, ma_precip_mm, Mean_width_m, Mean_water_depth_m, pH, EC_us_cm, DOC_mg_L, DO_mg_L, TN_mg_L, TP_mg_L) # have to subset otherwise error if including data with too many NAs


#colnames(numeric_dat) <- c("CO2", "CH4", "N2O", "masl", "temp", "precip", "width", "depth", "pH", "EC", "DOC", "DO", "TN", "TP")

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

res2 <- rcorr(as.matrix(numeric_dat), type = c("spearman")) # calculate correlation matrix
res2

flattenCorrMatrix(res2$r, res2$P)

res2$r # Extract the correlation coefficients

res2$P # Extract p-values

corr <- corrplot(res2$r, type = "upper", order = "original", 
                 tl.cex = 1, tl.col = "black", tl.srt = 45, p.mat = res2$P, sig.level = 0.05, insig = "blank")
corr #error, arguments imply differing numer of rows: 336, 310. But it seems fine...
#not sure what the number of row error is about, because the plot looks fine


# Another method, but cor() doesn't calculate p values
cor_dat <- cor(numeric_dat, use = "pairwise.complete.obs", method=c("spearman")) #Mike suggested using spearman rather than pearson correlation

corrplot(cor_dat, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45, insig = "blank")


chart.Correlation(numeric_dat[, c(1:3, 5:13)], histogram=TRUE, pch=10, use = "pairwise.complete.obs")
chart.Correlation(numeric_dat[, c(1:3, 14:19)], histogram=TRUE, pch=10, use = "pairwise.complete.obs")
#there is too much for one plot, so separated the variables into 2 plots


#### Sampling method ####
CO2method <- ggplot(dat, aes(x=ghg_sampling_method , y=g_co2_m_2_yr, fill=ghg_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2method

N2Omethod <- ggplot(dat, aes(x=ghg_sampling_method , y=g_n2o_m_2_yr, fill=ghg_sampling_method) )+   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + scale_y_log10()
N2Omethod

#### Trophic status ####
# Reorder factor levels more logically
levels(dat$nutrient_status)

dat$nutrient_status <- ordered(dat$nutrient_status, levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypertrophic"))


tiff("troph_count.tiff", units="in", width=4, height=4, res=300)

troph_count <- ggplot(dat, aes(x = nutrient_status)) +
geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Trophic state") + theme_minimal() 
troph_count

dev.off()

tiff("CO2trophic.tiff", units="in", width=7, height=4, res=300)

CO2trophic <- ggplot(dat, aes(x=nutrient_status , y=g_co2_m_2_yr, fill=nutrient_status)) + xlab("Nutrient status") +  
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() +theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Nutrient status") + scale_fill_manual(values=c( "#FFD3B5","#AFBCD3", "#4E745E", "#FFACB7" )) +ylab(expression(g~CO[2]~m^-2*~yr^-1)) 
CO2trophic

dev.off()


tiff("N2Otrophic.tiff", units="in", width=7, height=4, res=300)

N2Otrophic <- ggplot(dat, aes(x=nutrient_status , y=g_n2o_m_2_yr , fill=nutrient_status)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()  + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + scale_fill_manual(values=c("#FFD3B5","#AFBCD3", "#4E745E", "#FFACB7")) + xlab("Nutrient status") + theme(legend.position="none") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) #+ scale_y_log10()  #+  scale_y_continuous(trans='log2', labels = scales::number_format(accuracy = 0.01))
N2Otrophic

dev.off()

#### Land use ####
tiff("landuse_count.tiff", units="in", width=4, height=4, res=300)
land_count <- ggplot(data = subset(dat, !is.na(land_use_clc)), aes(x = land_use_clc)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Land use (CLC)") + theme_minimal() 
land_count
dev.off()

tiff("CO2landuse.tiff", units="in", width=6, height=4, res=300)

CO2landuse <- ggplot(data = subset(dat, !is.na(land_use_clc)), aes(x=reorder(land_use_clc, g_co2_m_2_yr, na.rm=T) , y=g_co2_m_2_yr, fill=land_use_clc)) + xlab("Land use") +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() +theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Land use") + scale_fill_manual(values=c( "#FFD3B5", "#4E745E", "#FFACB7" , "#AFBCD3")) +ylab(expression(g~CO[2]~m^-2*~yr^-1)) + scale_x_discrete(labels = c('Urban','Natural/Forest','Agriculture', 'Wetland'))
CO2landuse

dev.off()

tiff("N2Olanduse.tiff", units="in", width=6, height=4, res=300)

N2Olanduse <- ggplot(data = subset(dat, !is.na(land_use_clc)), aes(x=reorder(land_use_clc, g_n2o_m_2_yr, na.rm=T)  , y=g_n2o_m_2_yr, fill=land_use_clc)) + xlab("Land use") +
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + scale_fill_manual(values=c("#FFD3B5", "#4E745E", "#FFACB7" , "#AFBCD3")) + scale_x_discrete(labels = c('Natural/Forest', 'Urban', 'Wetland', 'Agriculture')) + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) #+  scale_y_continuous(trans='log2', labels = scales::number_format(accuracy = 0.01))
N2Olanduse

dev.off()



#### Soil type ####
tiff("soiltype_count.tiff", units="in", width=2.5, height=4, res=300)
soil_count <- ggplot(data = subset(dat, !is.na(soil_type)), aes(x = soil_type)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Soil type") + theme_minimal() 
soil_count
dev.off()

tiff("CO2soil.tiff", units="in", width=4, height=4, res=300)

CO2soil <- ggplot(subset(dat, !is.na(soil_type)), aes(x=soil_type , y=g_co2_m_2_yr, fill=soil_type)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + scale_fill_manual(values=c("#FFACB7", "#4E745E")) +  xlab("Soil type")  +ylab(expression(g~CO[2]~m^-2*~yr^-1))
CO2soil

dev.off()

tiff("N2Osoil.tiff", units="in", width=4, height=4, res=300)

N2Osoil <- ggplot(subset(dat, !is.na(soil_type)),  aes(x=soil_type , y=g_n2o_m_2_yr, fill=soil_type)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + scale_fill_manual(values=c("#FFACB7", "#4E745E")) +  xlab("Soil type") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) #+  scale_y_continuous(trans='log2', labels = scales::number_format(accuracy = 0.01))
N2Osoil

dev.off()

#### Hydrological regime ####
tiff("hydroregime_count.tiff", units="in", width=4, height=4, res=300)
dry_count <- ggplot(dat, aes(x = hydrological_regime)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Hydrological regime") + theme_minimal() 
dry_count
dev.off()

tiff("CO2hydro.tiff", units="in", width=4, height=4, res=300)
CO2hydro <- ggplot(dat, aes(x=hydrological_regime , y=g_co2_m_2_yr, fill=hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none")
CO2hydro
dev.off()

tiff("N2Ohydro.tiff", units="in", width=4, height=4, res=300)
N2Ohydro <- ggplot(dat, aes(x=hydrological_regime , y=g_n2o_m_2_yr, fill=hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() + theme(legend.position="none") #+ scale_y_continuous(trans='log2')
N2Ohydro
dev.off()

#### Climate zone ####
tiff("climate_count.tiff", units="in", width=6, height=4, res=300)
clim_count <- ggplot(data = subset(dat, !is.na(kg_climatezone)), aes(x = kg_climatezone, fill= kg_main_climate_group)) +
  geom_bar() +   labs(y = "Frequency", x = "Climate zones") + theme_minimal() 
clim_count
dev.off()

tiff("climate_count.tiff", units="in", width=6, height=4, res=300)
kg_main_count <- ggplot(dat, aes(x = kg_main_climate_group)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "KG Climate zones") + theme_minimal() 
kg_main_count
dev.off()

tiff("CO2KGclimate.tiff", units="in", width=6, height=4, res=300)

CO2KGclimate <- ggplot(data = subset(dat, !is.na(kg_main_climate_group)), aes(x=reorder(kg_main_climate_group, g_co2_m_2_yr, na.rm=T) , y=g_co2_m_2_yr, fill=kg_main_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() +  theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Köppen Geiger climate zone") +  scale_fill_manual(values = c("Tropical" = "#4E745E", "Temperate" = "#FFACB7",   "Arid" = "#FFD3B5",  "Continental" = "#AFBCD3")) +ylab(expression(g~CO[2]~m^-2*~yr^-1)) 
CO2KGclimate   

dev.off()


levels(dat$kg_main_climate_group)

CO2climate <- ggplot(dat, aes(x=climate_zone, y=g_co2_m_2_yr, fill=climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2climate

tiff("N2OKGclimate.tiff", units="in", width=6, height=4, res=300)

N2OKGclimate <- ggplot(data = subset(dat, !is.na(kg_main_climate_group)),  aes(x=reorder(kg_main_climate_group, g_n2o_m_2_yr, na.rm=T) , y=g_n2o_m_2_yr, fill=kg_main_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()  +  theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Köppen Geiger climate zone") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1))+   scale_fill_manual(values = c("Tropical" = "#4E745E", "Temperate" = "#FFACB7",   "Arid" = "#FFD3B5",  "Continental" = "#AFBCD3")) # + scale_y_continuous(trans='log2', labels = scales::number_format(accuracy = 0.01))
N2OKGclimate

dev.off()

N2Oclimate <- ggplot(dat, aes(x=climate_zone , y=g_n2o_m_2_yr, fill=climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() #+ scale_y_log10()
N2Oclimate


#### Effects of climate variables ####
tiff("CO2temp.tiff", units="in", width=6, height=4, res=300)
CO2temp <- ggplot(dat, aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp
dev.off()

#by climate zone
CO2temp <- ggplot(dat, aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(aes(colour=kg_main_climate_group), size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp

#by trophic status
CO2temp <- ggplot(subset(dat, complete.cases(nutrient_status) ), aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(aes(colour=nutrient_status), size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp

#by soil type
CO2temp <- ggplot(subset(dat, complete.cases(soil_type) ), aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(aes(colour=soil_type), size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp


# Plot scatter plot with line of best fit and equation
CO2temp <- ggplot(dat, aes(x = ma_temp_c, y = g_co2_m_2_yr)) + 
  geom_point(size = 2) +  theme_minimal() #geom_smooth(method = "lm", se = FALSE, color = "blue") +
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),   parse = TRUE, size = 5, color = "black", label.x = "right", label.y = "top") 
CO2temp


CO2precip <- ggplot(dat, aes(x=ma_precip_mm , y=g_co2_m_2_yr)) + geom_point(aes(colour=hydrological_regime), size = 2) + theme_minimal()
CO2precip # by soil_type  is interesting and unexpected

N2Otemp <- ggplot(dat, aes(x=ma_temp_c , y=g_n2o_m_2_yr)) + geom_point( aes(color=kg_main_climate_group), size = 2) + theme_minimal()
N2Otemp

N2Oprecip <- ggplot(dat, aes(x=ma_precip_mm , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oprecip

#### GHG and elevation ####
CO2elev <- ggplot(dat, aes(x=elevation_masl , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2elev

N2Oelev <- ggplot(dat, aes(x=elevation_masl , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oelev

#### GHG and width/depth ####
CO2width <- ggplot(dat, aes(x=mean_width_m , y=g_co2_m_2_yr)) + geom_point( aes(colour=land_use_clc), size = 2) + theme_minimal()
CO2width

CO2depth <- ggplot(dat, aes(x=mean_water_depth_m , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2depth

N2Owidth <- ggplot(dat, aes(x=mean_width_m , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Owidth

N2Odepth <- ggplot(dat, aes(x=mean_water_depth_m , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Odepth

#### GHG and velocity/discharge
CO2veloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_co2_m_2_yr)) + geom_point( aes(color=nutrient_status), size = 2) + theme_minimal()
CO2veloc

CO2discharge <- ggplot(dat, aes(x=mean_discharge_m3_s , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2discharge

N2Oveloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Oveloc

N2Odischarge <- ggplot(dat, aes(x=mean_discharge_m3_s , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Odischarge



#### GHG and water chemistry ####
## CO2
CO2DO <- ggplot(dat, aes(x=do_mg_l , y=g_co2_m_2_yr)) + geom_point( aes(colour=nutrient_status), size = 2) + theme_minimal()
CO2DO

CO2pH <- ggplot(dat, aes(x=pH , y=g_co2_m_2_yr)) + geom_point(aes(colour=land_use_clc),size = 2) + theme_minimal()
CO2pH

CO2EC <- ggplot(dat, aes(x=ec_us_cm , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2EC

CO2DOC <- ggplot(dat, aes(x=doc_mg_l , y=g_co2_m_2_yr)) + geom_point( aes(colour=soil_type), size = 2) + theme_minimal()
CO2DOC

CO2TP <- ggplot(dat, aes(x=tp_mg_l , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2TP

CO2TN <- ggplot(dat, aes(x=tn_mg_l , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal() + xlim(0, 10)
CO2TN


## N2O
N2ODO <- ggplot(dat, aes(x=do_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODO

N2OpH <- ggplot(dat, aes(x=pH , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2OpH

N2OEC <- ggplot(dat, aes(x=ec_us_cm , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2OEC

N2ODOC <- ggplot(dat, aes(x=doc_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODOC

N2OTP <- ggplot(dat, aes(x=tp_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2OTP

N2OTN <- ggplot(dat, aes(x=tn_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal() + xlim(0, 10)
N2OTN

N2ONO3 <- ggplot(dat, aes(x=nitrate_mg_l , y=g_n2o_m_2_yr)) + geom_point( aes(colour=ma_temp_c), size = 2) + theme_minimal() + scale_color_viridis()
N2ONO3

N2Ochla <- ggplot(dat, aes(x=chl_a_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Ochla

#### CO2 vs CH4 ####
tiff("CO2_CH4.tiff", units="in", width=6, height=4, res=300)
CO2_CH4 <- ggplot(dat, aes(x=ch4_diff_g_ch4_m_2_yr , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2_CH4
dev.off()


# Plot scatter plot with line of best fit and equation
CO2_CH4 <- ggplot(dat, aes(x = ch4_diff_g_ch4_m_2_yr, y = g_co2_m_2_yr)) + 
  geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),   parse = TRUE, size = 5, color = "black", label.x = "right", label.y = "top") +    theme_minimal()
CO2_CH4


######################################

#### Run statistical tests ####

#To test differences in means between groups: one-way ANOVA (normal) or Kruskal-Wallis (non-normal)
# can use Shapiro Wilks test to test for normality: 

dat %>%
  group_by(kg_main_climate_group) %>%
  filter(!is.na(kg_main_climate_group)) %>%
 # filter(KGMain_climate_group != "Arid") %>%
  shapiro_test(g_n2o_m_2_yr) #If p > 0.05, then data is normally distributed

#Not normal: soil, nutrient status, land use, hydrological regime, KG main climate

dat %>%
  group_by(nutrient_status) %>%
  filter(!is.na(nutrient_status)) %>%
  shapiro_test(g_co2_m_2_yr) #If p > 0.05, then data is normally distributed

#sample sizes to small for nutrient status, land use 
# Not normal: soil, hydrological regime, 

#Note that, if your sample size is greater than 50, the normal QQ plot is preferred because at larger sample sizes the Shapiro-Wilk test becomes very sensitive even to a minor deviation from normality

#Since most of the data does not follow the assumption of normality, we can run a non-paramentric test: the Kruskal-Walis

#### Kruskal wallis CO2 ####
kruskal.test(g_co2_m_2_yr ~ ghg_sampling_method, data = subset(dat, !is.na(ghg_sampling_method)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ soil_type, data = subset(dat, !is.na(soil_type)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ nutrient_status, data = subset(dat, !is.na(nutrient_status)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ land_use_clc, data = subset(dat, !is.na(land_use_clc)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ kg_main_climate_group, data = subset(dat, !is.na(kg_main_climate_group)) ) # p = 0.057

kruskal.test(g_co2_m_2_yr ~ hydrological_regime, data = subset(dat, !is.na(hydrological_regime)) ) #no sig diff

#check for the pairwise differences
pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$kg_main_climate_group)], dat$kg_main_climate_group[!is.na(dat$kg_main_climate_group)], p.adjust.method = "BH") # no sig diff

pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$land_use_clc)], dat$land_use_clc[!is.na(dat$land_use_clc)], p.adjust.method = "BH") # no sig diff

pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$nutrient_status)], dat$nutrient_status[!is.na(dat$nutrient_status)], p.adjust.method = "BH") # no sig diff


#### Kruskal wallis N2O ####
kruskal.test(g_n2o_m_2_yr ~ ghg_sampling_method, data = subset(dat, !is.na(ghg_sampling_method)) ) #s 0.03

kruskal.test(g_n2o_m_2_yr ~ soil_type, data = subset(dat, !is.na(soil_type)) ) # no sig diff

kruskal.test(g_n2o_m_2_yr ~ nutrient_status, data = subset(dat, !is.na(nutrient_status)) ) # 0.002

kruskal.test(g_n2o_m_2_yr ~ land_use_clc, data = subset(dat, !is.na(land_use_clc)) ) # p = 0.09

kruskal.test(g_n2o_m_2_yr ~ kg_main_climate_group, data = subset(dat, !is.na(kg_main_climate_group)) ) # p = 0.0002

kruskal.test(g_n2o_m_2_yr ~ hydrological_regime, data = subset(dat, !is.na(hydrological_regime)) ) # p = 0.0003

#check for the pairwise differences
pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$ghg_sampling_method)], dat$ghg_sampling_method[!is.na(dat$ghg_sampling_method)], p.adjust.method = "BH") #  no sig diff bn conc and chamber

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$nutrient_status)], dat$nutrient_status[!is.na(dat$nutrient_status)], p.adjust.method = "BH") # eut/hyp-oli and eut/hyp-meso

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$land_use_clc)], dat$land_use_clc[!is.na(dat$land_use_clc)], p.adjust.method = "BH")  # no sig diff

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$kg_main_climate_group)], dat$kg_main_climate_group[!is.na(dat$kg_main_climate_group)], p.adjust.method = "BH") # temperate and continental <0.0001


#######################################

#### CO2 equivalents ####

#Calculate CO2e of N2O
#the GWP of N2O is 273 over a 100 yr horizon (IPCC, 2021)
dat$N2O_CO2e <- (dat$g_n2o_m_2_yr*270)  # GWP is 273

#Calculate CO2e of CH4
#the GWP of methane is 27 over a 100 yr horizon (IPCC, 2021)
dat$CH4_CO2e <- (dat$CH4_diffusive_g_CH4_m2_yr + dat$CH4_ebullitive_g_CH4_m2_yr)*45 #GWP is 27

dat$CH4_CO2e <- rowSums(cbind(dat$CH4_diffusive_g_CH4_m2_yr, dat$CH4_ebullitive_g_CH4_m2_yr), na.rm = TRUE) * 45  # GWP is 27

dat$CH4_CO2e[dat$CH4_CO2e == 0] <- NA


#Proportion of each gas 
mean(dat$N2O_CO2e, na.rm=T)
sd(dat$N2O_CO2e, na.rm=T)

mean(dat$CH4_CO2e, na.rm=T)
sd(dat$CH4_CO2e, na.rm=T)


sum(!is.na(dat$g_co2_m_2_yr)) # counts


a <- mean(dat$N2O_CO2e, na.rm=T) + mean(dat$CH4_CO2e, na.rm=T) + mean(dat$g_co2_m_2_yr, na.rm=T)
mean(dat$g_co2_m_2_yr, na.rm=T)/a*100

# subset just the studies that measured all 3 gases


subset_CO2eq <- dat[complete.cases(dat$CH4_CO2e, dat$N2O_CO2e, dat$g_co2_m_2_yr), ]
# 16 obs

mean(subset_CO2eq$g_co2_m_2_yr, na.rm=T)
sd(subset_CO2eq$g_co2_m_2_yr, na.rm=T)

mean(subset_CO2eq$CH4_CO2e, na.rm=T)
sd(subset_CO2eq$CH4_CO2e, na.rm=T)

mean(subset_CO2eq$N2O_CO2e, na.rm=T)
sd(subset_CO2eq$N2O_CO2e, na.rm=T)

b <- mean(subset_CO2eq$N2O_CO2e, na.rm=T) + mean(subset_CO2eq$CH4_CO2e, na.rm=T) + mean(subset_CO2eq$g_co2_m_2_yr, na.rm=T)

mean(subset_CO2eq$N2O_CO2e, na.rm=T)/b*100

mean(subset_CO2eq$CH4_CO2e, na.rm=T)/b*100

mean(subset_CO2eq$g_co2_m_2_yr, na.rm=T)/b*100

##################################################

#### Quantile regressions ####
library(quantreg)


#try it out for a "wedge" distribution
#plot the relationship

ggplot(dat, aes(mean_velocity_m_s, g_n2o_m_2_yr))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05)) #multiple models from the 5th percentile to 95th percentile

ggplot(dat, aes(do_mg_l, g_co2_m_2_yr))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05))

ggplot(dat, aes(pH, g_co2_m_2_yr))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05))

ggplot(dat, aes(ma_precip_mm, g_co2_m_2_yr))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05))


ggplot(dat, aes(nitrate_mg_l, g_n2o_m_2_yr))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05))

ggplot(dat, aes(tn_mg_l, g_n2o_m_2_yr))+ xlim(0,10) +
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05))


taus<-seq(from = .05, to = .95, by = 0.05) #Taus ranging from 0.05 to 0.95 with a step value of 0.05
quant_all  <- rq(ma_precip_mm~g_co2_m_2_yr, tau = taus, 
                 data = dat)

sumQR<- summary(quant_all)

plot(sumQR) #these do not look like the tutorial...

#can compare the AIC values of these models
aic_df <- data.frame(AIC = AIC(quant_all), model = paste("tau =",taus))
print(subset(aic_df, AIC <= min(aic_df$AIC) + 2)) # pick the model with the lowest AIC score, those within 2 delta AIC are comparable

summary(quant_all) |> plot("g_co2_m_2_yr") # quantile process plot
