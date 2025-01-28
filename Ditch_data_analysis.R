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
library(scales)
library(quantreg)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(tmaptools)
library(cowplot)
library(patchwork)


# Set working directory for figures
setwd("C:/Users/teres/Documents/Ditch lit review/DitchReview/Figures/")

#Load in lit review data from Google Sheet (saved on PC):

dat <- readxl::read_excel("C:/Users/teres/Documents/Ditch lit review/DitchReview/Data/Ditch_data_extraction_2024-07-11.xlsx", sheet="Data_extraction", range = "A1:AO120", na = "NA") # Replace character NAs with actual NAs  # set range if you want to exclude the "Not included" studies

dat <- as.data.frame(dat)

str(dat)  #119 obs. of  41 variables
colnames(dat)
dat <- clean_names(dat)  #clean names: no capitals, only _
colnames(dat)

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
  mutate(ghg_sampling_method = as.factor(ghg_sampling_method)) %>%
  mutate(instream_vegetation = as.factor(instream_vegetation))


length(unique(dat$authors)) #77 unique studies

sum(!is.na(dat$g_co2_m_2_yr)) #99
sum(!is.na(dat$g_n2o_m_2_yr)) #56
sum(!is.na(dat$ch4_diff_g_ch4_m_2_yr)) #94
sum(!is.na(dat$ch4_ebull_g_ch4_m_2_yr)) #20


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
#### Clean velocity : make the non-numeric entries numeric ####

#determine the lowest 10% percentile of the numeric values

dat$num_mean_velocity_m_s <- as.numeric(dat$mean_velocity_m_s) # subset only the numeric values
quantile(dat$num_mean_velocity_m_s, probs = 0.1, na.rm=T) # 10% 0.035
quantile(dat$num_mean_velocity_m_s, probs = 0.01, na.rm=T) # 0.01294
quantile(dat$num_mean_velocity_m_s, probs = 0.9, na.rm=T) # 90% 0.321

#replace "slow" flow with the 1% percentile value: 0.013
#replace "low" flow with the lowest recorded velocity: 0.01
#replace "low/no" and "sluggish" and "minimal/absent" and "low/absent" flow with half of the lowest recorded velocity:  0.005
#replace "standing" with 0



dat <- dat %>%
mutate(mean_velocity_m_s = case_when(
  grepl("standing", mean_velocity_m_s, ignore.case = TRUE) ~ "0.0",
  grepl("sluggish", mean_velocity_m_s, ignore.case = TRUE) ~ "0.005",
  grepl("low / no flow", mean_velocity_m_s, ignore.case = TRUE) ~ "0.005",
  grepl("/absent", mean_velocity_m_s, ignore.case = TRUE) ~ "0.005",
  grepl("slow", mean_velocity_m_s, ignore.case = TRUE) ~ "0.013",
  grepl("low", mean_velocity_m_s, ignore.case = TRUE) ~ "0.01",
  TRUE ~ mean_velocity_m_s  # Keep the original value if it doesn't match any condition
))  %>%
    select(-num_mean_velocity_m_s) %>%
  mutate(mean_velocity_m_s = as.numeric(mean_velocity_m_s))



#### Clean soil type ####

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

#### Clean hydrological regime ####

levels(dat$hydrological_regime)

dat <- dat %>%
  mutate(hydrological_regime = case_when(
    grepl("Perennial", hydrological_regime, ignore.case = TRUE) ~ "Perennial",
    grepl("Intermittent", hydrological_regime, ignore.case = TRUE) ~ "Intermittent",
    TRUE ~ NA_character_  # NA if it doesn't match any condition
  ))

dat <- dat %>%
  mutate(hydrological_regime = as.factor(hydrological_regime))

#### Clean land use ####
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

#### Clean GHG sampling method ####

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


#### Clean vegetation presence / absence ####

levels(dat$instream_vegetation)


dat <- dat %>%
  mutate(instream_vegetation = case_when(
    grepl("Yes", instream_vegetation, ignore.case = TRUE) ~ "Yes",
    grepl("No", instream_vegetation, ignore.case = TRUE) ~ "No",
    TRUE ~ instream_vegetation  # Keep the original value if it doesn't match any condition
  ))  


dat <- dat %>%
  mutate(instream_vegetation = as.factor(instream_vegetation))

levels(dat$instream_vegetation)


#### Add a new variable ratio of stream depth to stream width

dat$depth_width <-  dat$mean_water_depth_m / dat$mean_width_m 

#### Add a new column for nitrate-nitroen NO3-N

dat$nitrateN_mg_l <- dat$nitrate_mg_l/62.0049*14.0067

#### Add a new column for elemental ratios: C:N and N:P

dat$CN <- dat$doc_mg_l/1000/12.011 / dat$tn_mg_l/1000/14.01 #15 obs

dat$CN2 <- dat$doc_mg_l/1000/12.011 / dat$nitrateN_mg_l/1000/14.01  #22 obs

dat$NP <-  dat$nitrateN_mg_l/1000/14.01  / dat$tp_mg_l/1000/30.97 # only 3 obs

###### Function for plotting log transformed negative and positive values from Philip Keller ####

#   scale_y_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000", "10,000"))


sign_breaks <- function(n = 5, base = 10){
  rng <- log(range(n, na.rm = TRUE), base = base)
  min <- floor(rng[1])
  max <- ceiling(rng[2])
  if (max == min) 
    return(base^min)
  by <- floor((max - min)/n) + 1
  base^seq(min, max, by = by)
}


sign_trans <- function(base = exp(1)){
  trans <- function(x) sign(x)*log(abs(x))
  inv <- function(x) base^x
  trans_new("sign_trans", trans, inv, log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

#### Ditch summary statistics ####
#Summary statistic table from vtable:

st(dat, vars = c('elevation_masl','mean_width_m', 'mean_water_depth_m', 'mean_velocity_m_s', 'mean_discharge_m3_s', 'do_mg_l', 'pH', 'ec_us_cm', 'doc_mg_l', 'tn_mg_l', 'nitrateN_mg_l',  'tp_mg_l', 'chl_a_mg_l' ))

#average negative CO2
x <- subset(dat, g_co2_m_2_yr<=0)
count(x$g_co2_m_2_yr)
mean(x$g_co2_m_2_yr)
sd(x$g_co2_m_2_yr)

#average negative N2O
y <- subset(dat, g_n2o_m_2_yr<=0)
count(y$g_n2o_m_2_yr)
mean(y$g_n2o_m_2_yr)
sd(y$g_n2o_m_2_yr)



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
dat$g_co2_m_2_yr


CO2_map <- ggplot(data = world )  +
  geom_sf(fill = "grey", color=NA) +  theme_void() +
  coord_sf(expand = FALSE) + 
   geom_point(data = subset(dat, !is.na(g_co2_m_2_yr)), aes(x = longitude, y = latitude, size= g_co2_m_2_yr), colour="black", fill="#C87542", alpha=0.5, shape=21) + # "distinct" plots only the unique coordinates
  xlab("Longitude") + ylab("Latitude") + 
  scale_size_binned(name =  expression(g~CO[2]~m^-2*~yr^-1) , range = c(0.5, 12),  
                    breaks = c(15, 150, 1500, 15000), 
                    labels = c("-150 to 15", "15 to 150", "150 to 1500", ">15000"),
                    guide = guide_legend(  # Adjust legend properties
    keywidth = unit(1.0, "cm"),  # Set legend key width
    keyheight = unit(0.2, "cm"),  # Set legend key height
    label.theme = element_text(size = 9)) )+ theme(plot.title = element_text(face = "bold", size = 14), legend.margin =  margin(0, 25, 0, 0), legend.position = "right", legend.justification = c(1, 1), axis.title.x=element_blank(), axis.title.y=element_blank()) + 
geom_rect( xmin = -12, ymin = 35,xmax = 48,ymax = 70,fill = NA, colour = "black",  size = 0.6 ) +
  ggtitle("(a)") 
CO2_map


plot_ratio <- tmaptools::get_asp_ratio(world)

tiff("CO2_map", units="in", width=4*plot_ratio, height=4, res=300) 

CO2_map_full <- ggdraw(CO2_map) +
  draw_plot(
    {   
      CO2_map + 
        coord_sf(
          xlim = c(-12, 48),
          ylim = c(35, 70),
          expand = FALSE) +           
        theme(legend.position = "none", plot.title = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.68, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = .001,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.40)
CO2_map_full

dev.off()




N2O_map <- ggplot(data = world) +
  geom_sf(fill = "grey", color=NA) +  theme_void() +
  coord_sf(expand = FALSE) + 
  geom_point(data = subset(dat, !is.na(g_n2o_m_2_yr)), aes(x = longitude, y = latitude, size= g_n2o_m_2_yr), colour="black", fill="#78629B", alpha=0.5, shape=21) + 
      xlab("Longitude") + ylab("Latitude") + scale_size_binned(name =  expression(g~N[2]*`O`~m^-2~yr^-1) , range = c(0.5, 10), breaks = c(2.5, 5.0, 7.5, 10.0), 
                      labels = c("-0.05 to 2.5", "2.5 to 5.0", "5.0 to 7.5", ">10.0"),
    guide = guide_legend(  # Adjust legend properties
    keywidth = unit(1.0, "cm"),  # Set legend key width
    keyheight = unit(0.2, "cm"),  # Set legend key height
    label.theme = element_text(size = 9)) ) + theme(plot.title = element_text(face = "bold", size = 14), legend.position = "right", legend.justification = c(1, 1), axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle("(b)") +   geom_rect( xmin = -10, ymin = 45,xmax = 36,ymax = 70,fill = NA, 
      colour = "black",  size = 0.6 ) 
N2O_map

plot_ratio <- tmaptools::get_asp_ratio(world)

tiff("N2O_map", units="in", width=4*plot_ratio, height=4, res=300)

N2O_map_full <- ggdraw(N2O_map) +
  draw_plot(
    {   
      N2O_map + 
        coord_sf(
          xlim = c(-10, 36),
          ylim = c(45, 70),
          expand = FALSE) +           
        theme(legend.position = "none", plot.title = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.70, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = .001,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.40)
N2O_map_full

dev.off()

#pdf("global_dis.pdf", width=3.5*plot_ratio, height=6)

tiff("global_dist.tiff", units="in", width=3.5*plot_ratio, height=6, res=300)

global_dist <- ggarrange(CO2_map_full, N2O_map_full, 
                        ncol = 1, nrow = 2, align="v",common.legend = F )
global_dist

dev.off() # the (b) plot will not align, so need to do it in post (.edt file in Illustrator)


#### GHG data ####
#### GHG summary statistics ####

st(dat, vars = c("g_co2_m_2_yr",  "g_n2o_m_2_yr")) #GHG summary stats

sd(dat$g_n2o_m_2_yr, na.rm=T)

# mean and 95% CI
t_test_result <- t.test(dat$g_co2_m_2_yr, na.rm = TRUE)
t_test_result$estimate # mean
t_test_result$conf.int #CI

t_test_result2 <- t.test(dat$g_n2o_m_2_yr, na.rm = TRUE)
t_test_result2$estimate # mean
t_test_result2$conf.int #CI

# Convert to t CO2-C ha-1 yr-1 
#       2057 g co2 m-2 y-1
2056.723/44.01*12.01/90.718 #6.186913 t CO2-C ha-1 yr-1
#convert to co2-c
#convert g/m2 to t US tons/ha using  /90.718
#95% CI: 
1533.593/44.01*12.01/90.718 #4.613264
2579.854/44.01*12.01/90.718 #7.760564

#with night time emissions 27% higher Gomez-Gener et al 2021

6.186913*1.27 #7.85738

#### Global ditch emissions ####

## global ditch surface area is 53,804,240,000 m2
## Update taking into account Chris' comments about not needing the include the small estuary value:
## global ditch surface area is 5,353,425 ha (Peacock et al. 2021), or 53,534,250,000 m2

#CO2
#mean
2056.723/44.01*12.01 #convert to CO2-C
561.2643*53534250000*1e-12 # 30.1984 Tg CO2-C #NEW: 30.04686 Tg CO2-C
#lower
1533.593/44.01*12.01 #convert to CO2-C
418.5061*53534250000*1e-12 # 22.5174 Tg CO2-C #NEW: 22.40441 Tg CO2-C
#upper
2579.854/44.01*12.01 #convert to CO2-C
704.0229*53534250000*1e-12 # 37.87942 Tg CO2-C #NEW: 37.68934 Tg CO2-C

#If we add noctural CO2 emissions, which are 27% higher than daytime
561.2643*1.27 #712.8057
712.8057*53534250000*1e-12 #38.15952 Tg CO2-C

#lower: 
418.5061*1.27
531.5027*53534250000*1e-12 #28.4536 Tg CO2-C

#upper
712.8057*1.27
905.2632*53534250000*1e-12 #48.46259 Tg CO2-C


#N2O
#mean
0.8919954/44.013*28.01 #convert to N2O-N
0.5676684*53534250000*1e-12 # 0.03054297 Tg N2O-N #NEW: 0.0303897 Tg N2O-N
#lower
0.4008087/44.013*28.01 #convert to N2O-N
0.2550758*53534250000*1e-12 # 0.01372416 Tg N2O-N #NEW: 0.01365529 Tg N2O-N
#upper
1.3831820/44.013*28.01 #convert to N2O-N
0.880261*53534250000*1e-12 # 0.04736177 Tg N2O-N #NEW: 0.04712411 Tg N2O-N

#### Percentage of global/inland water budgets ####
#Global anthropogenic CO2 (Friedlingstein et al 2023): 11.1 Gt C or 11,100 Tg C
#mean:  30.04686 Tg CO2-C / 11,100 Tg C
30.04686/11100*100 #0.2706924%
#lower: 22.40441 Tg CO2-C / 11,100 Tg C
22.40441/11100*100 #0.2018415
#upper: 37.68934 Tg CO2-C / 11,100 Tg C
37.68934/11100*100 #0.3395436

#Global anthropogenic N2O (Tian et al 2024): 6.5 Tg N year-1
#mean: 0.0303897 Tg N2O-N / 6.5 Tg N year-1
0.0303897/6.5*100 #0.4675338%
#lower:  0.01365529 Tg N2O-N  / 6.5 Tg N year-1
0.01365529/6.5*100 #0.2100814
#upper: 0.04712411 Tg N2O-N / 6.5 Tg N year-1
0.04712411/6.5*100 #0.7249863

#Global inland water CO2 (Drake et al 2018): 3.9 Pg C or 3900 Tg C
#mean: 30.04686 Tg CO2-C /3900 Tg C
30.04686/3900*100 #0.7704323
#lower: 22.40441 Tg CO2-C /3900 Tg C
22.40441/3900*100 #0.5744721
#upper: 37.68934 Tg CO2-C /3900 Tg C
37.68934/3900*100 #0.9663933

#including nocturnal emissions: 
#mean: 38.15952 Tg CO2-C
38.15952/3900*100 #0.9784492 %
#lower: 28.4536 Tg CO2-C
28.4536/3900*100 # 0.7295795 %
#upper: 48.46259 Tg CO2-C
48.46259/3900*100 #1.242631

#Global inland water N2O (Tian et al 2024): 0.5 Tg N year-1 
#mean: 0.0303897 Tg N2O-N / 0.5 Tg N year-1 
0.0303897/0.5*100 #6.07794
#lower:  0.01365529 Tg N2O-N  / 0.5 Tg N year-1 
0.01365529/0.5*100 #2.731058%
#upper: 0.04712411 Tg N2O-N / 0.5 Tg N year-1 
0.04712411/0.5*100 #9.424822%




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


#### Proportion of CH4 from ebulition

dat$prop_ebul <- dat$ch4_ebull_g_ch4_m_2_yr / (dat$ch4_diff_g_ch4_m_2_yr + dat$ch4_ebull_g_ch4_m_2_yr) *100

sum(!is.na(dat$prop_ebul))  #20

mean(dat$prop_ebul, na.rm=T) #79.78185
sd(dat$prop_ebul, na.rm=T)

#############################################################
#### Correlation plot ####
numeric_dat <- dat[sapply(dat, is.numeric)] # subset numeric data

numeric_dat <- numeric_dat %>%
  select(-rndCoord.lon, -rndCoord.lat, -publication_year, -n_sites, -ma_evap_mm, -nitrate_mg_l, -secchi_depth_m)

colnames(numeric_dat)

colnames(numeric_dat) <- c("lat", "lon", "masl",  "temp", "precip", "width", "depth", "veloc", "discharge", "CO2", "CH4_diff", "CH4_ebull",  "N2O", "DO",  "pH", "EC", "DOC", "TP", "TN", "chl-a", "depth_width", "NO3-N", "CN", "CN2", "NP")

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

#Plot all of the possible relationships:

chart.Correlation(numeric_dat[, c(1:3, 5:9)], histogram=TRUE, pch=10, use = "pairwise.complete.obs", method=c("spearman"))

chart.Correlation(numeric_dat[, c(1:3, 10:14)], histogram=TRUE, pch=10, use = "pairwise.complete.obs", method=c("spearman"))

chart.Correlation(numeric_dat[, c(1:3, 15:19)], histogram=TRUE, pch=10, use = "pairwise.complete.obs", method=c("spearman"))
#there is too much for one plot, so separated the variables into 2 plots

#Plot all of the moderate >.4 and strong >.7 Spearman relationships for CO2: DO,  N2O, velocity, pH  and TP 

tiff("spearman_CO2", units="in", width=6, height=6, res=300)
chart.Correlation(numeric_dat[, c(1:2,12,14:15, 18)], histogram=TRUE, pch=10, use = "pairwise.complete.obs", method=c("spearman"))
dev.off()

# for N2O diffusive CH4 emissions,  CO2, velocity , TN , and nitrate

tiff("spearman_N2O", units="in", width=6, height=6, res=300)
chart.Correlation(numeric_dat[, c(2, 1, 3,12, 19, 22)], histogram=TRUE, pch=10, use = "pairwise.complete.obs", method=c("spearman"))
dev.off()

citation("lme4")

#### Sampling method ####
CO2method <- ggplot(subset(dat, ghg_sampling_method!="Both"), aes(x=ghg_sampling_method , y=g_co2_m_2_yr) )+   geom_boxplot(outlier.shape = NA, fill ="#C87542") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() +  theme(axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black"))  + ylab(expression(g~CO[2]~m^-2*~yr^-1)) +   scale_y_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000", "10,000"))  + scale_x_discrete(labels = c(  "Chamber" = "Chamber\n method", "Concentration" = "Concentration\nmethod"))
CO2method

N2Omethod <- ggplot(subset(dat, ghg_sampling_method!="Both"), aes(x=ghg_sampling_method , y=g_n2o_m_2_yr) )+   geom_boxplot(outlier.shape = NA, fill="#78629B") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() + theme(axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) +   xlab("GHG sampling method") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + scale_y_continuous(trans = 'pseudo_log') + scale_x_discrete(labels = c(  "Chamber" = "Chamber\n method", "Concentration" = "Concentration\nmethod"))
N2Omethod

# Mean and standard deviation CO2/N2O by sampling method
aggregate(g_co2_m_2_yr ~ ghg_sampling_method, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n=length(x)))

#### Trophic status ####
# Reorder factor levels more logically
levels(dat$nutrient_status)

dat$nutrient_status <- ordered(dat$nutrient_status, levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypertrophic"))

count(dat$nutrient_status)

tiff("troph_count.tiff", units="in", width=4, height=4, res=300)

troph_count <- ggplot(dat, aes(x = nutrient_status)) +
geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Trophic state") + theme_minimal() 
troph_count

dev.off()

tiff("CO2trophic.tiff", units="in", width=6, height=5, res=300)

CO2trophic <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=nutrient_status , y=g_co2_m_2_yr, fill=nutrient_status)) + xlab("Nutrient status") +  
  geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal() +theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Nutrient status") + scale_fill_manual(values=c( "Hypertrophic"="#A569BD", "Oligotrophic" = "#4780B3", "Eutrophic" = "#4E745E", "Mesotrophic" = "#FF97B5" )) + ylab(expression(g~CO[2]~m^-2*~yr^-1)) 
CO2trophic

dev.off()

# Mean and standard deviation CO2 by trophic status
aggregate(g_co2_m_2_yr ~ nutrient_status, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                              sd = sd(x, na.rm = TRUE), 
                              n = length(x)))


tiff("N2Otrophic.tiff", units="in", width=6, height=5, res=300)

N2Otrophic <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=nutrient_status , y=g_n2o_m_2_yr , fill=nutrient_status)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal()  + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + scale_fill_manual(values=c( "Hypertrophic"="#A569BD", "Oligotrophic" = "#4780B3", "Eutrophic" = "#4E745E", "Mesotrophic" = "#FF97B5" )) + xlab("Nutrient status") + theme(legend.position="none") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) +   scale_y_continuous(trans = 'pseudo_log')
N2Otrophic

dev.off()

# Mean and standard deviation CO2 by trophic status
aggregate(g_n2o_m_2_yr ~ nutrient_status, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))

#### Land use ####
tiff("landuse_count.tiff", units="in", width=4, height=4, res=300)
land_count <- ggplot(data = subset(dat, !is.na(land_use_clc) & unique(authors)), aes(x = land_use_clc)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Land use (CLC)") + theme_minimal() 
land_count
dev.off()

tiff("CO2landuse.tiff", units="in", width=6, height=4, res=300)

CO2landuse <- ggplot(data = subset(dat, !is.na(land_use_clc)), aes(y=land_use_clc , x=g_co2_m_2_yr, fill=land_use_clc)) + xlab("Land use") +
  geom_boxplot(outlier.shape = NA, fill = "#C87542") +  geom_point(position = position_jitter(height=0.25), size = 2.5, alpha=0.5) + theme_minimal() +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 50),  
    plot.title = element_text(hjust = -0.1, size = 12, face = "bold"),      
          plot.title.position = "plot" , axis.title.y = element_blank(), axis.line = element_line(colour = "black"),  panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.ticks.x = element_line(), legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black"))  + ylab("Land use")  + xlab(expression(g~CO[2]~m^-2*~yr^-1))  +  scale_x_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000", "10,000")) +  labs(  title = "(a)")   + scale_y_discrete(labels = c('Agriculture', 'Natural/Forest', 'Urban' , 'Wetland' ))
CO2landuse 

dev.off()

tiff("N2Olanduse.tiff", units="in", width=6, height=4, res=300)

N2Olanduse <- ggplot(data = subset(dat, !is.na(land_use_clc)), aes(y=land_use_clc, x=g_n2o_m_2_yr, fill=land_use_clc)) +  geom_boxplot(outlier.shape = NA, fill="#78629B") +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal() +
 theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 50),  
       plot.title = element_text(hjust = -0.1, size = 12, face="bold"),      
       plot.title.position = "plot" , axis.ticks.x = element_line(), axis.line = element_line(colour = "black"), axis.title.y = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black"))    + xlab(expression(g~N[2]*`O`~m^-2~yr^-1)) + scale_x_continuous(trans = 'pseudo_log') + labs(  title = "(b)")  + scale_y_discrete(labels = c('Agriculture', 'Natural/Forest', 'Urban' , 'Wetland' ))
N2Olanduse  

dev.off()

# Mean and standard deviation CO2/N2O  by land use
aggregate(g_co2_m_2_yr ~ land_use_clc, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))



#### Soil type ####
tiff("soiltype_count.tiff", units="in", width=2.5, height=4, res=300)
soil_count <- ggplot(data = subset(dat, !is.na(soil_type)), aes(x = soil_type)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Soil type") + theme_minimal() 
soil_count
dev.off()


tiff("CO2soil.tiff", units="in", width=4, height=4, res=300)

CO2soil <- ggplot(subset(dat, !is.na(soil_type)), aes(x=soil_type , y=g_co2_m_2_yr, fill=soil_type)) +   geom_boxplot(outlier.shape = NA,  fill = "#C87542") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() + theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 40),  plot.title = element_text(hjust = -0.15, size = 12, face="bold"), plot.title.position = "plot" , axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) +  xlab("Soil type")  +ylab(expression(g~CO[2]~m^-2*~yr^-1)) +  scale_y_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000", "10,000")) +  labs(  title = "(c)") 
CO2soil

dev.off()



tiff("N2Osoil.tiff", units="in", width=4, height=4, res=300)

N2Osoil <- ggplot(subset(dat, !is.na(soil_type)),  aes(x=soil_type , y=g_n2o_m_2_yr, fill=soil_type)) +   geom_boxplot(outlier.shape = NA, fill="#78629B") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() + theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 40),  plot.title = element_text(hjust = -0.15, size = 12, face="bold"), plot.title.position = "plot", axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) +  xlab("Soil type") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + scale_y_continuous(trans = 'pseudo_log') + labs(  title = "(d)") 
N2Osoil

dev.off()

# Mean and standard deviation N2O by soil type
aggregate(g_n2o_m_2_yr ~ soil_type, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),  n = length(x)))



#### DOC by soil type ####

tiff("DOCsoil.tiff", units="in", width=3, height=3, res=300)
DOC_soil <- ggplot(subset(dat, !is.na(soil_type)), aes(x=soil_type , y=doc_mg_l, fill=soil_type)) +   geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() + theme(axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) + scale_fill_manual(values=c("#FFACB7", "#4E745E")) +  xlab("Soil type")  +  ylab(expression(DOC~(mg~L^`-1`)))
DOC_soil
dev.off()

#### Hydrological regime ####
tiff("hydroregime_count.tiff", units="in", width=4, height=4, res=300)
dry_count <- ggplot(dat, aes(x = hydrological_regime)) +
  geom_bar(fill=	"#097969") +   labs(y = "Frequency", x = "Hydrological regime") + theme_minimal() 
dry_count
dev.off()

tiff("CO2hydro.tiff", units="in", width=4, height=4, res=300)

CO2hydro <- ggplot( subset(dat, complete.cases(hydrological_regime)), aes(x=hydrological_regime , y=g_co2_m_2_yr, fill=hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values = c("#FFD700", "#4682B4" )) + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlab(expression("Hydrological regime")) + ylab(expression(g~CO[2]~m^-2*~yr^-1))
CO2hydro

dev.off()

# Assuming 'dat' is your dataframe

# Mean and standard deviation CO2 by hydrological regime
aggregate(g_co2_m_2_yr ~ hydrological_regime, data = dat, 
FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),  n = length(x)))


tiff("N2Ohydro.tiff", units="in", width=4, height=4, res=300)

N2Ohydro <- ggplot(subset(dat, complete.cases(hydrological_regime)), aes(x=hydrological_regime , y=g_n2o_m_2_yr, fill=hydrological_regime)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.position="none") + scale_fill_manual(values = c("#FFD700", "#4682B4" )) + theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlab(expression("Hydrological regime")) + ylab(expression(g~N[2]*`O`~m^-2~yr^-1))
N2Ohydro

dev.off()

# Mean and standard deviation N2O by hydrological regime
aggregate(g_n2o_m_2_yr ~ hydrological_regime, data = dat, 
   FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))


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

CO2KGclimate <- ggplot(data = subset(dat, !is.na(kg_main_climate_group)), aes(x=reorder(kg_main_climate_group, g_co2_m_2_yr, na.rm=T) , y=g_co2_m_2_yr, fill=kg_main_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal() +  theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Köppen Geiger climate zone") +  
  scale_fill_manual(values = c("Tropical" = "#AAD487", "Temperate" = "#FF97B5",  "Arid" = "#FFD766","Continental" = "#4780B3")) + ylab(expression(g~CO[2]~m^-2*~yr^-1)) +
  #scale_y_continuous(trans = 'pseudo_log') #gives a weird y axis
  scale_y_continuous(trans = "sign", breaks = c(-100,-10,1,10,100,1000,10000), labels = c("-100", "-10","1", "10", "100", "1,000", "10,000"))
CO2KGclimate  

# Mean and standard deviation CO2 by climate
aggregate(g_co2_m_2_yr ~ kg_main_climate_group, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))


# scale_y_continuous(trans = 'pseudo_log', breaks = c(-100,-10, 1, 10, 100, 1000, 10000)) 
# trans = "sign" uses the custom function from Philipp Keller

dev.off()

#there are different ways of log transforming in ggplot: you can transform the data, you can transform the scale (before statistics: scale_y_log10() ), and you can transform the coordinate system (after statistics, changes shape of geoms) 
#pseudo_log: This is a custom transformation function that applies a pseudo-logarithmic transformation to the data. It takes the logarithm of the absolute value of the data plus 1 (to avoid log(0)) and retains the sign of the original data.


levels(dat$kg_main_climate_group)

CO2climate <- ggplot(dat, aes(x=climate_zone, y=g_co2_m_2_yr, fill=climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal()
CO2climate

tiff("N2OKGclimate.tiff", units="in", width=6, height=4, res=300)

N2OKGclimate <- ggplot(data = subset(dat, !is.na(kg_main_climate_group)),  aes(x=reorder(kg_main_climate_group, g_n2o_m_2_yr, na.rm=T) , y=g_n2o_m_2_yr, fill=kg_main_climate_group)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2.5, alpha=0.5) + theme_minimal()  +  theme(legend.position="none", axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + xlab("Köppen Geiger climate zone") + ylab(expression(g~N[2]*`O`~m^-2~yr^-1))+   scale_fill_manual(values = c("Tropical" = "#AAD487", "Temperate" = "#FF97B5",  "Arid" = "#FFD766","Continental" = "#4780B3"))  +  
 # scale_y_continuous(trans = "sign") gives an error, introduced infinite values
scale_y_continuous(trans = 'pseudo_log') #, breaks = c(-1, 0, 1, 2, 4, 8)
N2OKGclimate

dev.off()

# Mean and standard deviation N2O by climate
aggregate(g_n2o_m_2_yr ~ kg_main_climate_group, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))


N2Oclimate <- ggplot(dat, aes(x=climate_zone , y=g_n2o_m_2_yr, fill=climate_zone)) + geom_boxplot(outlier.shape = NA) +  geom_point(position = position_jitter(width = 0.15), size = 2) + theme_minimal() #+ scale_y_log10()
N2Oclimate



#### Vegetation presence / absence ####

CO2veg <- ggplot(subset(dat, complete.cases(instream_vegetation)), aes(x=instream_vegetation, y=g_co2_m_2_yr, fill=instream_vegetation)) + geom_boxplot(outlier.shape = NA, fill = "#C87542") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() +  theme(
  plot.margin = margin(t = 10, r = 10, b = 10, l = 40),  
  plot.title = element_text(hjust = -0.15, size = 12, face="bold"),   
  plot.title.position = "plot" ,
    axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) +  scale_x_discrete(labels = c('No veg', 'Veg'))  + ylab(expression(g~CO[2]~m^-2*~yr^-1)) +  scale_y_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000", "10,000")) + labs(  title = "(a)") 
CO2veg

N2Oveg <- ggplot(subset(dat, complete.cases(instream_vegetation)), aes(x=instream_vegetation, y=g_n2o_m_2_yr, fill=instream_vegetation)) + geom_boxplot(outlier.shape = NA, fill="#78629B") +  geom_point(position = position_jitter(width = 0.35), size = 2.5, alpha=0.5) + theme_minimal() +  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 40),  plot.title = element_text(hjust = -0.15, size = 12, face="bold"), plot.title.position = "plot" ,  axis.title.x = element_blank(), axis.line = element_line(colour = "black"),   axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.title = element_text(size = 12), axis.text = element_text(size = 12, color="black")) + scale_x_discrete(labels = c('No veg', 'Veg')) + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + scale_y_continuous(trans = 'pseudo_log')  + labs(  title = "(b)") 
N2Oveg

# Mean and standard deviation N2O by veg presence/absence
aggregate(g_co2_m_2_yr ~ instream_vegetation, data = dat, 
          FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x)))


#### Combine data for supplementary information ####

tiff("soil_type_combined", units="in", width=6, height=6, res=300)

combine <- ggarrange(CO2soil, N2Osoil, 
                     ncol = 1, nrow = 2, align="hv",common.legend = F, labels = c("(a)", "(b)"))
combine

dev.off()


tiff("land_use_combined", units="in", width=6.5, height=6, res=300)

combine <- ggarrange(CO2landuse, N2Olanduse, 
                      ncol = 1, nrow = 2, align="hv",common.legend = F)
combine

dev.off()


tiff("GHG_method", units="in", width=6, height=3, res=300)

combine <- ggarrange(CO2method, N2Omethod,  
                     ncol = 2, nrow = 1, align="hv",common.legend = F, labels = c("(a)", "(b)" ))
combine

dev.off()

tiff("soil_veg", units="in", width=6.5, height=6, res=300)

combine <- ggarrange(CO2veg, N2Oveg, CO2soil, N2Osoil, 
                     ncol = 2, nrow = 2, align="hv",common.legend = F)
combine

dev.off()

#### Effects of climate variables ####
tiff("CO2temp.tiff", units="in", width=6, height=4, res=300)
CO2temp <- ggplot(dat, aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal() +xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = "")))
CO2temp
dev.off()

#by climate zone
tiff("CO2temp.tiff", units="in", width=7, height=4, res=300)

CO2temp <- ggplot(dat, aes(x=ma_temp_c , y=g_co2_m_2_yr)) + geom_point(aes(colour=kg_main_climate_group), size = 2.5, alpha=0.7) + theme_minimal() + xlab(expression(paste("Mean annual temperature (", degree, "C)", sep = ""))) +   scale_colour_manual(values = c("Tropical" = "#4E745E", "Temperate" = "#FFACB7",  "Arid" = "#FFD766","Continental" = "#AFBCD3")) +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), axis.title = element_text(size = 12), legend.text = element_text(size = 12), axis.text = element_text(size = 10, color="black"))  + xlab(expression(paste("MAT (", degree, "C)", sep = ""))) + ylab(expression(g~CO[2]~m^-2*~yr^-1)) 
CO2temp


dev.off()

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

tiff("CO2precip", units="in", width=7, height=4, res=300)
CO2precip <- ggplot(dat, aes(x=ma_precip_mm , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) 
CO2precip # by soil_type  is interesting and unexpected
dev.off()

tiff("N2Otemp", units="in", width=7, height=4, res=300)

N2Otemp <- ggplot(dat, aes(x=ma_temp_c , y=g_n2o_m_2_yr)) + geom_point( aes(color=kg_main_climate_group), size = 2.5, alpha=0.7) + theme_minimal() + theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) +     scale_colour_manual(values = c("Tropical" = "#4E745E", "Temperate" = "#FFACB7",  "Arid" = "#FFD766","Continental" = "#AFBCD3"))  + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + xlab(expression(paste("MAT (", degree, "C)", sep = "")))
N2Otemp

dev.off()

tiff("N2Oprecip", units="in", width=7, height=4, res=300)
N2Oprecip <- ggplot(dat, aes(x=ma_precip_mm , y=g_n2o_m_2_yr)) + geom_point(size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) 
N2Oprecip
dev.off()


#### combine CO2 and N2O  MAT ####

tiff("MAT_combined", units="in", width=6.5, height=3, res=300)

MAT_combined <- ggarrange(CO2temp, N2Otemp,  
                     ncol = 2, nrow = 1, align="hv",common.legend = T, legend="bottom", labels = c("(a)", "(b)"))
MAT_combined

dev.off()


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

#### GHG and velocity/discharge ####
tiff("CO2veloc.tiff", units="in", width=6, height=4, res=300)

CO2veloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5) + theme_minimal() +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlab(expression(Mean~velocity~ (m ~s^-1) )) + ylab(expression(g~CO[2]~m^-2*~yr^-1)) 
CO2veloc

dev.off()

CO2discharge <- ggplot(dat, aes(x=mean_discharge_m3_s , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2discharge

tiff("N2Oveloc.tiff", units="in", width=6, height=4, res=300)

N2Oveloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_n2o_m_2_yr)) + geom_point(size = 2.5, alpha=0.5) + theme_minimal() +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + xlab(expression(Mean~velocity~ (m ~s^-1) ))  
N2Oveloc

dev.off()

N2Odischarge <- ggplot(dat, aes(x=mean_discharge_m3_s , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Odischarge



#### GHG and water chemistry ####
## CO2
tiff("CO2DO", units="in", width=7, height=4, res=300)

CO2DO <- ggplot(dat, aes(x=do_mg_l , y=g_co2_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="black") + theme_minimal() +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlab(expression(DO~(mg~L^{-1}))) + ylab(expression(g~CO[2]~m^-2*~yr^-1))  
CO2DO

dev.off()

tiff("CO2pH", units="in", width=7, height=4, res=300)

CO2pH <- ggplot(dat, aes(x=pH , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + ylab(expression(g~CO[2]~m^-2*~yr^-1))  
CO2pH

dev.off()

CO2EC <- ggplot(dat, aes(x=ec_us_cm , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal()
CO2EC

CO2DOC <- ggplot(dat, aes(x=doc_mg_l , y=g_co2_m_2_yr)) + geom_point( aes(colour=soil_type), size = 2) + theme_minimal()
CO2DOC


tiff("CO2TP", units="in", width=7, height=4, res=300)

CO2TP <- ggplot(subset(dat, complete.cases(tp_mg_l)), aes(x=tp_mg_l , y=g_co2_m_2_yr)) + geom_point(size = 2) + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlab(expression(TP~ (mg ~L^-1))) + ylab(expression(g~CO[2]~m^-2*~yr^-1))
CO2TP

dev.off()

tiff("CO2TN", units="in", width=7, height=4, res=300)

CO2TN <- ggplot(subset(dat, complete.cases(g_co2_m_2_yr)), aes(x=tn_mg_l , y=g_co2_m_2_yr)) + geom_point(aes(colour=land_use_clc), size = 2.5, alpha=0.5) + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +  scale_colour_manual(values=c( "Wetland"="#A569BD", "Urban" = "#4780B3", "Natural_Forest" = "#4E745E", "Agriculture" = "#FF97B5" )) + ylab(expression(g~CO[2]~m^-2*~yr^-1)) + xlab(expression(TN~ (mg ~L^-1))) 
CO2TN

dev.off()

#Is the outlying CO2 value modifying this correlation?

cor_TN_outlier <- dat %>%
  #filter(g_co2_m_2_yr <= 18000) %>%
  filter(complete.cases(g_co2_m_2_yr, tn_mg_l)) %>%
  summarise(correlation = cor.test(g_co2_m_2_yr, tn_mg_l, method = "spearman")$estimate,
            p_value = cor.test(g_co2_m_2_yr, tn_mg_l, method = "spearman")$p.value)
#without the outlying CO2, correlation is 0.43 p = 0.049



## N2O
N2ODO <- ggplot(dat, aes(x=do_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODO

N2OpH <- ggplot(dat, aes(x=pH , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2OpH

N2OEC <- ggplot(dat, aes(x=ec_us_cm , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2OEC

N2ODOC <- ggplot(dat, aes(x=doc_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2ODOC

N2OTP <- ggplot(dat, aes(x=tp_mg_l , y=g_n2o_m_2_yr)) + geom_point(aes(colour=nutrient_status),size = 2.5, alpha=0.5) + theme_minimal()
N2OTP

tiff("N2OTN.tiff", units="in", width=6, height=4, res=300)

N2OTN <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=tn_mg_l , y=g_n2o_m_2_yr)) + geom_point(colour="black", size = 2.5, alpha=0.5) + theme_minimal() + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +   scale_x_log10()  + xlab(expression(TN~ (mg ~L^-1))) 
N2OTN

dev.off()

N2OCN <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=CN , y=g_n2o_m_2_yr)) + geom_point(colour="black", size = 2.5, alpha=0.5) + theme_minimal() + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +   scale_x_log10()  
N2OCN

N2OCN2 <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=CN2 , y=g_n2o_m_2_yr)) + geom_point(colour="black", size = 2.5, alpha=0.5) + theme_minimal() + ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) +  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +   scale_x_log10()  
N2OCN2



tiff("N2ONO3.tiff", units="in", width=6, height=4, res=300)

N2ONO3 <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=nitrateN_mg_l , y=g_n2o_m_2_yr)) + geom_point(colour="black", size = 2.5, alpha=0.5) + theme_minimal() + ylim(-0.05, 6) +  ylab(expression(g~N[2]*`O`~m^-2~yr^-1)) + 
  xlab(expression(NO[3]^"-"~"-N"~ (mg ~L^-1))) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black"))  
N2ONO3

dev.off()

N2Ochla <- ggplot(dat, aes(x=chl_a_mg_l , y=g_n2o_m_2_yr)) + geom_point(size = 2) + theme_minimal()
N2Ochla

#### CO2 vs N2O ####
tiff("CO2_N2O.tiff", units="in", width=6, height=4, res=300)

CO2_N2O <- ggplot(dat, aes(y=g_co2_m_2_yr, x=g_n2o_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="black") + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +  xlab(expression(g~N[2]*`O`~m^-2~yr^-1)) + ylab(expression(g~CO[2]~m^-2*~yr^-1))
#scale_y_continuous(trans = "sign", breaks = c(-10,1,10,100,1000,10000), labels = c("-10","1", "10", "100", "1,000","10,000")) doesn't work for N2O.... 
CO2_N2O


dev.off()

#### CO2 vs CH4 ####
tiff("CO2_CH4.tiff", units="in", width=6, height=4, res=300)
CO2_CH4 <- ggplot(subset(dat, complete.cases(ch4_diff_g_ch4_m_2_yr)), aes(x=ch4_diff_g_ch4_m_2_yr , y=g_co2_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="black") + theme_minimal() + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) + xlim(-1, 1000)
CO2_CH4
dev.off()


#### N2O vs CH4 ####
tiff("N2O_CH4.tiff", units="in", width=6, height=4, res=300)

N2O_CH4 <- ggplot(dat, aes(x=ch4_diff_g_ch4_m_2_yr , y=g_n2o_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="black") + theme_minimal() + theme_minimal() + theme(legend.title=element_blank(), legend.text = element_text(size = 14), axis.title = element_text(size = 16), axis.text = element_text(size = 14, color="black")) +  ylab(expression(g~N[2]*`O`~m^-2~yr^-1))  +xlab(expression(diffusive~CH[4]~flux~(mg~CH[4]*`-C`~m^-2~h^-1))) + scale_x_continuous(trans = "sign", breaks = c(1,10,100,1000,10000), labels = c("1", "10", "100", "1,000","10,000"))
N2O_CH4

dev.off()


# Plot scatter plot with line of best fit and equation
CO2_CH4 <- ggplot(dat, aes(x = ch4_diff_g_ch4_m_2_yr, y = g_co2_m_2_yr)) + 
  geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),   parse = TRUE, size = 5, color = "black", label.x = "right", label.y = "top") +    theme_minimal()
CO2_CH4

#######Combine significant environmental variables ####
# those with a significant Spearman correlation <.4 (moderately correlated) and <.7 (strongly correlated)

CO2DO <- ggplot(dat, aes(x=do_mg_l , y=g_co2_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="#C87542") + theme_minimal() +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) + xlab(expression(DO~(mg~L^`-1`))) + ylab(expression(g~CO[2]~m^`-2`*~yr^`-1`)) + stat_cor(method="spearman", label.sep = "\n", label.x = 7.2, label.y = 16300)
CO2DO

CO2_N2O <- ggplot(dat, aes(y=g_co2_m_2_yr, x=g_n2o_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="#C87542") + theme_minimal() + theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) +  xlab(expression(g~N[2]*`O`~m^`-2`~yr^`-1`)) + ylab(expression(g~CO[2]~m^`-2`*~yr^`-1`)) + stat_cor(method="spearman", label.sep = "\n", label.x = 5.5, label.y = 16000)
CO2_N2O

CO2veloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5, colour="#C87542") + theme_minimal() +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) + xlab(expression(Velocity~ (m ~s^`-1`) )) + ylab(expression(g~CO[2]~m^`-2`*~yr^`-1`)) + stat_cor(method="spearman", label.sep = "\n",  label.x = 0.5, label.y = 16350)
CO2veloc

CO2pH <- ggplot(dat, aes(x=pH , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5, colour="#C87542") + theme_minimal() + theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) + ylab(expression(g~CO[2]~m^`-2`*~yr^`-1`)) + stat_cor(method="spearman", label.sep = "\n",  label.x = 0.5, label.y = 16450)
CO2pH

CO2TP <- ggplot(subset(dat, complete.cases(tp_mg_l)), aes(x=tp_mg_l , y=g_co2_m_2_yr)) + geom_point(size = 2.5, alpha=0.5, colour="#C87542") + theme_minimal() + theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) + xlab(expression(TP~ (mg ~L^`-1`))) + ylab(expression(g~CO[2]~m^`-2`*~yr^`-1`)) + stat_cor(method="spearman", label.sep = "\n",  label.x = 1.2, label.y = 3600)
CO2TP

N2O_CH4 <- ggplot(dat, aes(x=ch4_diff_g_ch4_m_2_yr , y=g_n2o_m_2_yr)) + geom_point( size = 2.5, alpha=0.5, colour="#78629B") + theme_minimal() + theme_minimal() + theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) +  ylab(expression(g~N[2]*`O`~m^`-2`~yr^`-1`))  +xlab(expression(g~CH[4]~m^`-2`~yr^`-1`)) + xlim(-0.05, 800) + stat_cor(method="spearman", label.sep = "\n", label.x = 400, label.y = 8.5)
N2O_CH4 # note one outlying value of CH4 (vinasse) was removed for plotting

N2Oveloc <- ggplot(dat, aes(x=mean_velocity_m_s , y=g_n2o_m_2_yr)) + geom_point(size = 2.5, alpha=0.5, colour="#78629B") + theme_minimal() +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black"))  + ylab(expression(g~N[2]*`O`~m^`-2`~yr^`-1`)) + xlab(expression(Velocity~ (m ~s^`-1`) ))  + stat_cor(method="spearman", label.sep = "\n", label.x = 0.55, label.y = 9)
N2Oveloc

N2OTN <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=tn_mg_l , y=g_n2o_m_2_yr)) + geom_point(colour="#78629B", size = 2.5, alpha=0.5) + theme_minimal() + ylab(expression(g~N[2]*`O`~m^`-2`~yr^`-1`)) +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) +   scale_x_log10()  + xlab(expression(TN~ (mg ~L^`-1`))) + stat_cor(method="spearman", label.sep = "\n", label.x = 1.5, label.y = 9)
N2OTN

N2ONO3 <- ggplot(subset(dat, complete.cases(nutrient_status)), aes(x=nitrateN_mg_l , y=g_n2o_m_2_yr)) + geom_point(colour="#78629B", size = 2.5, alpha=0.5) + theme_minimal() + ylim(-0.05, 6) +  ylab(expression(g~N[2]*`O`~m^`-2`~yr^`-1`)) + xlab(expression(NO[3]^"-"~"-N"~ (mg ~L^`-1`))) +  theme(axis.line = element_line(colour = "black"),  axis.ticks.x = element_line(), axis.ticks.y = element_line(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), legend.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10, color="black")) + stat_cor(method="spearman", label.sep = "\n", label.x = 4, label.y = 5.15)
N2ONO3

tiff("CO2N2O_env_vars", units="in", width=7.7, height=6.2, res=300)

env_vars <- ggarrange(CO2DO, CO2pH,  CO2veloc, CO2TP, CO2_N2O,
                          N2O_CH4, N2Oveloc, N2OTN, N2ONO3,
                                  ncol = 3, nrow = 3, align="hv",common.legend = T,legend="top",  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"))
env_vars

dev.off()



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

#Since most of the data does not follow the assumption of normality, we can run a non-paramentric test: the Kruskal-Walis (for 3 or more groups) or Mann-Whitney test (for 2 groups)

#### Mann-Whitney U test CO2 ####

wilcox.test(g_co2_m_2_yr ~ ghg_sampling_method, data = subset(dat, !is.na(ghg_sampling_method) & ghg_sampling_method != "Both") ) # no sig diff

wilcox.test(g_co2_m_2_yr ~ soil_type, data = subset(dat, !is.na(soil_type)) ) # no sig diff

wilcox.test(g_co2_m_2_yr ~ hydrological_regime, data = subset(dat, !is.na(hydrological_regime)) ) #no sig diff

wilcox.test(g_co2_m_2_yr ~ instream_vegetation, data = subset(dat, !is.na(instream_vegetation)) ) # no sig diff

#### Kruskal wallis CO2 ####

kruskal.test(g_co2_m_2_yr ~ nutrient_status, data = subset(dat, !is.na(nutrient_status)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ land_use_clc, data = subset(dat, !is.na(land_use_clc)) ) # no sig diff

kruskal.test(g_co2_m_2_yr ~ kg_main_climate_group, data = subset(dat, !is.na(kg_main_climate_group)) ) # p = 0.03

#check for the pairwise differences
pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$kg_main_climate_group)], dat$kg_main_climate_group[!is.na(dat$kg_main_climate_group)], p.adjust.method = "BH") # no sig dif

pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$land_use_clc)], dat$land_use_clc[!is.na(dat$land_use_clc)], p.adjust.method = "BH") # no sig diff

pairwise.wilcox.test(dat$g_co2_m_2_yr[!is.na(dat$nutrient_status)], dat$nutrient_status[!is.na(dat$nutrient_status)], p.adjust.method = "BH") # no sig diff

#### Mann-Whitney U test N2O  ####
wilcox.test(g_n2o_m_2_yr ~ ghg_sampling_method, data = subset(dat, !is.na(ghg_sampling_method) & ghg_sampling_method != "Both") ) #s 0.43

wilcox.test(g_n2o_m_2_yr ~ soil_type, data = subset(dat, !is.na(soil_type)) ) # no sig diff

wilcox.test(g_n2o_m_2_yr ~ hydrological_regime, data = subset(dat, !is.na(hydrological_regime)) ) # p = 0.001


wilcox.test(g_n2o_m_2_yr ~ instream_vegetation, data = subset(dat, !is.na(instream_vegetation)) ) # no sig diff

#### Kruskal wallis N2O ####

kruskal.test(g_n2o_m_2_yr ~ nutrient_status, data = subset(dat, !is.na(nutrient_status)) ) # 0.001

kruskal.test(g_n2o_m_2_yr ~ land_use_clc, data = subset(dat, !is.na(land_use_clc)) ) # p = 0.11

kruskal.test(g_n2o_m_2_yr ~ kg_main_climate_group, data = subset(dat, !is.na(kg_main_climate_group)) ) # p = 0.00001



#check for the pairwise differences
pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$ghg_sampling_method)], dat$ghg_sampling_method[!is.na(dat$ghg_sampling_method)], p.adjust.method = "BH") #  no sig diff bn conc and chamber

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$nutrient_status)], dat$nutrient_status[!is.na(dat$nutrient_status)], p.adjust.method = "BH") # eut/hyp-oli and eut/hyp-meso

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$land_use_clc)], dat$land_use_clc[!is.na(dat$land_use_clc)], p.adjust.method = "BH")  # no sig diff

pairwise.wilcox.test(dat$g_n2o_m_2_yr[!is.na(dat$kg_main_climate_group)], dat$kg_main_climate_group[!is.na(dat$kg_main_climate_group)], p.adjust.method = "BH") # temperate and continental <0.0001

kruskal.test(g_n2o_m_2_yr ~ instream_vegetation, data = subset(dat, !is.na(instream_vegetation)) ) # no sig diff

#### Kruskal wallis CH4 ####
kruskal.test(ch4_diff_g_ch4_m_2_yr ~ soil_type, data = subset(dat, !is.na(soil_type)) ) #p-value = 0.1292


#### Mann-Whitney DOC ####

wilcox.test(doc_mg_l ~ soil_type, data = subset(dat, !is.na(soil_type)) ) # p <0.0001

#### Kruskal wallis MAT ####

kruskal.test(ma_temp_c ~ kg_main_climate_group, data = subset(dat, !is.na(kg_main_climate_group)) ) #sig

pairwise.wilcox.test(dat$ma_temp_c[!is.na(dat$kg_main_climate_group)], dat$kg_main_climate_group[!is.na(dat$kg_main_climate_group)], p.adjust.method = "BH")  # sig

#######################################

#### CO2 equivalents ####

#Calculate CO2e of N2O
#the GWP of N2O is 273 over a 100 yr horizon (IPCC, 2021)
#the SGWP of N2O is 270 over a 100 year horizon (Neubauer and Megonigal, 2015), and the cooling potential is 349
#dat$N2O_CO2e <- (dat$g_n2o_m_2_yr*273)  

dat$N2O_CO2e <- ifelse(dat$g_n2o_m_2_yr > 0, 
                       dat$g_n2o_m_2_yr * 273, 
                       dat$g_n2o_m_2_yr * 349)

#Calculate CO2e of CH4
#the GWP of methane is 27 over a 100 yr horizon (IPCC, 2021)
#the SGWP of CH4 is 45 over a 100 year horizon (Neubauer and Megonigal, 2015), cooling potential is 203

total_ch4 <- rowSums(cbind(dat$ch4_diff_g_ch4_m_2_yr, dat$ch4_ebull_g_ch4_m_2_yr), na.rm = TRUE)

# Apply the conditional multiplication
dat$CH4_CO2e <- ifelse(total_ch4 > 0, 
                       total_ch4 * 27, 
                       total_ch4 * 203)


# subset just the studies that measured all 3 gases


subset_CO2eq <- dat[complete.cases(dat$CH4_CO2e, dat$N2O_CO2e, dat$g_co2_m_2_yr), ]
# 36 obs
length(unique(subset_CO2eq$authors)) #22


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

# Total ditch emissions N2O and CO2 emissions in CO2 equivalents

#N2O : 201.4129 g CO2e m-2 yr-1
#ditch surface area: 53,534,250,000 m2

210*53534250000 #1.124219e+13 g or 11.2 Tg


#CO2 2085 g CO2e m-2 yr-1
2085*53534250000 #1.116189e+14 or 112 Tg

# or a total of 2295 CO2e
2295*53534250000 #1.228611e+14 or 122.861099 Tg or 122,861,099 tonnes or 122.86 million tonnes
# considering B.C.'s gross GHG emissions in 2021 were 62.0 million tonnes of carbon dioxide equivalent (MtCO2e).

112.86/62 #= 1.820323


############################################
#### Dry sediment GHG emissions #####


# read in the dry sediment data


# Manually add the dry sediment emissions and run a Kruskall Wallis test of hydrological regime 


dry_dat <- readxl::read_excel("C:/Users/teres/Documents/Ditch lit review/DitchReview/Data/Ditch_data_extraction_2024-07-11.xlsx", sheet="Data_extraction", na = "NA") # 


dry_dat <- as.data.frame(dry_dat)

dry_dat <- clean_names(dry_dat)  #clean names: no capitals, only _
colnames(dry_dat)

# Rename columns

colnames(dry_dat)[colnames(dry_dat) == "p_h"] <- "pH"
colnames(dry_dat)[colnames(dry_dat) == "mean_velocity_m_s_1"] <- "mean_velocity_m_s"
colnames(dry_dat)[colnames(dry_dat) == "mean_discharge_m3_s_1"] <- "mean_discharge_m3_s"
colnames(dry_dat)[colnames(dry_dat) == "g_co2_m_2_yr_1"] <- "g_co2_m_2_yr"
colnames(dry_dat)[colnames(dry_dat) == "ch4_diffusive_g_ch4_m_2_yr_1"] <- "ch4_diff_g_ch4_m_2_yr"
colnames(dry_dat)[colnames(dry_dat) == "ch4_ebullitive_g_ch4_m_2_yr_1"] <- "ch4_ebull_g_ch4_m_2_yr"
colnames(dry_dat)[colnames(dry_dat) == "g_n2o_m_2_yr_1"] <- "g_n2o_m_2_yr"
colnames(dry_dat)[colnames(dry_dat) == "do_mg_l_1"] <- "do_mg_l"
colnames(dry_dat)[colnames(dry_dat) == "ec_us_cm_1"] <- "ec_us_cm"
colnames(dry_dat)[colnames(dry_dat) == "doc_mg_l_1"] <- "doc_mg_l"
colnames(dry_dat)[colnames(dry_dat) == "tp_mg_l_1"] <- "tp_mg_l"
colnames(dry_dat)[colnames(dry_dat) == "tn_mg_l_1"] <- "tn_mg_l"
colnames(dry_dat)[colnames(dry_dat) == "chl_a_mg_l_1"] <- "chl_a_mg_l"


#Make columns factors
dry_dat <- dry_dat %>%
  mutate(soil_type = as.factor(soil_type))  %>%
  mutate(country = as.factor(country)) %>%
  mutate(climate_zone = as.factor(climate_zone))  %>%
  mutate(land_use = as.factor(land_use))  %>%
  mutate(hydrological_regime = as.factor(hydrological_regime)) %>%
  mutate(nutrient_status = as.factor(nutrient_status)) %>%
  mutate(ghg_sampling_method = as.factor(ghg_sampling_method)) %>%
  mutate(instream_vegetation = as.factor(instream_vegetation))


# Subset dry sediment data
dry_dat <- dry_dat[c(139:150), ]

#now merge with the rest of the data

# Find common columns
common_columns <- intersect(names(dry_dat), names(dat))

# Subset both data frames to include only common columns
dry_dat_common <- dry_dat %>% select(all_of(common_columns)) %>% 
  select(-mean_velocity_m_s, -mean_discharge_m3_s)

dat_common <- dat %>% select(all_of(common_columns))%>% 
  select(-mean_velocity_m_s, -mean_discharge_m3_s)

# Combine dry_dat and dat_common using bind_rows
combined_data <- bind_rows(dry_dat_common, dat_common)

#Summary data
aggregate(g_n2o_m_2_yr ~ hydrological_regime, data = combined_data,  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), n = length(x))) 

# test for differences between dry, perennial, and non-perennial flowing

kruskal.test(g_co2_m_2_yr ~ hydrological_regime, data = subset(combined_data, !is.na(hydrological_regime)) ) #not sig

kruskal.test(g_n2o_m_2_yr ~ hydrological_regime, data = subset(combined_data, !is.na(hydrological_regime)) )  #p-value = 0.003203

# pairwise test for N2O

pairwise.wilcox.test(combined_data$g_n2o_m_2_yr[!is.na(combined_data$hydrological_regime)], combined_data$hydrological_regime[!is.na(combined_data$hydrological_regime)], p.adjust.method = "BH")  # sig dif between perennial and intermittent (sample size too small for dry probably)

