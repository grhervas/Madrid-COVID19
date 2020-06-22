#########################################################################
################## COVID-19 IMPACT ON MADRID DISTRICTS ################## 
#########################################################################

# Libraries
library(readxl)
library(rgdal)
library(dplyr)
library(broom)
library(ggplot2)
library(leaflet)
library(tidyr)

# Clear workspace
rm(list = ls())

# Set working directory
if(rstudioapi::isAvailable()){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
  


# ----------------------- LOADING DATA -------------------------------
# Loading the survey data
df <- read_excel("encuesta_covid19_hogar_informante_2020.xlsx")

# Data types
str(df)

# Change IDREGISM to numeric
df$idregism <- as.numeric(df$idregism)

# Number of instances and variables
nrow(df)
ncol(df)

# Loading preprocessed data
dat <- read.csv("Dataset_prepocess.csv" ,header=TRUE, sep=";", na.strings = "..")


# ------------- CREATING NEW VARIABLES OF INTEREST -------------------

# Creating variable of "VITAL SPACE" 
# (= m2 of house / number of cohabitans during lockdown)
df$cohab.density <- df$p5a / df$nm_conf
# Some descriptive statistics
summary(df$cohab.density)
# Missing values
sum(is.na(df$cohab.density))


# Creating variable of "HOUSE HABITABILITY"
# Convert the values "NS/NC" (9) to "No" (2)  in given columns 
convert_NSNC <- function(df, cols, from, to) {
  df[, cols][df[, cols] == from] <- to
  return(df) 
}
df <- convert_NSNC(df, c("p9p1","p9p2","p9p3","p9p4",
                         "p11p1","p11p2","p11p3","p11p4"), 
                   9, 2)

# Habitability score
df$habitability.score <- 
  # House has exterior connection (max. 10)
  10 / df$p6 +
  # Dorms + baths by cohabitants (x10 factor)
  5 * (df$p7a + df$p8a) / df$nm_conf +
  # Lounge?
  15 * (2 - df$p9p1) +
  # Terraza?
  10 * (2 - df$p9p2) +
  # Jardín propio?
  20 * (2 - df$p9p3) + 
  # Jardín común?
  5 * (2 - df$p9p4) +
  # Cocina?
  10 / df$p10 +
  # Calefacción?
  2 * (2 - df$p11p1) +
  # AC?
  2 * (2 - df$p11p2) +
  # PC/tablet?
  10 * (2 - df$p11p3) +
  # Internet?
  15 * (2 - df$p11p4)

# Some descriptive statistics
summary(df$habitability.score)
# Missing values
sum(is.na(df$habitability.score))

# Divided in quartiles (1->lowest, 4->highest)
df$habitability <- ntile(df$habitability.score, 4)


# Creating variable of "MENTAL HEALTH"
df <- convert_NSNC(df, c("pa262","pa28p2"),
                   9, 3)
df <- convert_NSNC(df, c("pa271","pa272","pa273","pa274","pa275",
                         "pa276","pa277","pa278","pa279","pa2710",
                         "pa2711","pa2712"),
                   8, 2)
df <- convert_NSNC(df, c("pa271","pa272","pa273","pa274","pa275",
                         "pa276","pa277","pa278","pa279","pa2710",
                         "pa2711","pa2712"),
                   9, 2)

df$mental.health.score <-
  # Concentración? (1 > 4)
  10 / df$pa271 +
  # Preocupaciones? (1 > 4)
  10 / df$pa272 +
  # Útil? (1 > 4)
  10 / df$pa273 +
  # Decidido? (1 > 4)
  10 / df$pa274 +
  # Agobiado? (1 > 4)
  10 / df$pa275 +
  # Dificultades? (1 > 4)
  10 / df$pa276 +
  # Día a día? (1 > 4)
  10 / df$pa277 +
  # Afrontar? (1 > 4)
  10 / df$pa278 +
  # Deprimido? (1 > 4)
  10 / df$pa279 +
  # Confianza? (1 > 4)
  10 / df$pa2710 +
  # Inútil? (1 > 4)
  10 / df$pa2711 +
  # Feliz? (1 > 4)
  10 / df$pa2712 +
  # Solo? (1 < 4)
  2.5 * df$pa28p2


# Some descriptive statistics
summary(df$mental.health.score)
# Missing values
sum(is.na(df$mental.health.score))

# Divided in quartiles (1->lowest, 5->highest)
df$mental.health <- ntile(df$mental.health.score, 5)


# Creating variable of "MAIN CONCERNS" 
# 1=Económicas; 2=Sanitarias; 3=Políticas; 4=Otros
df$concern <- ifelse(df$cpa31a_1 %in% c(1,2,3,5,6,7,15,16),
                     1, 
                     ifelse(df$cpa31a_1 %in% c(4,8,9,10,14,17,21,22,24,25,26,28),
                            2,
                            ifelse(df$cpa31a_1 %in% c(11,12,13,18,19,20,23,27,96),
                                   3,
                                   4) 
                     )
)


# Some descriptive statistics
summary(df$concern)
# Missing values
sum(is.na(df$concern))


# ------------- KEEPING JUST RELEVANT VARIABLES -------------------

df_cleared <- df[, c("idregis","c3","nm_conf","cohab.density","p4","habitability.score",
                     "habitability","mental.health.score","mental.health","concern")]
names(df_cleared)[c(2,5)] <- c("id.district","house.type")


# ------------ MERGING WITH LABOUR DATA & INCOME -------------------
# We are just keeping the info of the main provider within the home
labour_data <- read.csv("Data_laboral.csv", sep=";", encoding="UTF-8")
names(labour_data)[3:5] <- c("occupation","occ.change","occ.impact") 

labour_data$occ.impact <- ifelse(labour_data$occ.impact == "G1", 1,
                                 ifelse(labour_data$occ.impact == "G2", 2,
                                        ifelse(labour_data$occ.impact == "G3", 3,
                                               ifelse(labour_data$occ.impact == "G4", 4,
                                                      0)
                                        )
                                 )
)


# Median income by district
income_district <- read.csv("renta.csv", sep=";", 
                            encoding="UTF-8", check.names = F)
names(income_district) <- c("id.district","district","avg.income")

# Merging original dataframe with labour data based on IDREGIS
df_merged <- merge(df_cleared, labour_data, by="idregis")
# ... and with Income data (based on ID.DISTRICT)
df_merged <- merge(df_merged, income_district, by="id.district", all.x=T)

# Set missing data correctly
df_merged$cohab.density[df_merged$cohab.density == 999] <- NA

# ------------ AGGREGATE BY DISTRICT -------------------
df_district <- df_merged %>% 
  group_by(id.district) %>% 
  summarise(nm_conf = round(median(nm_conf)),
            cohab.density = mean(cohab.density, na.rm = T),
            house.type = round(median(house.type)),
            habitability.score = mean(habitability.score),
            habitability = round(median(habitability)),
            mental.health.score = mean(mental.health.score),
            mental.health = round(median(mental.health)),
            concern = round(median(concern)),
            age = round(median(age)),
            occupation = round(median(occupation)),
            avg.income = mean(avg.income))

# For the aggregation of occupational change and impact just
# consider groups 1,2 (workers)
# df_district1 <- df_merged %>%
#   filter(occupation %in% c(1,2)) %>%
#   group_by(id.district) %>%
#   summarise(occ.change = round(median(occ.change)),
#             occ.impact = round(median(occ.impact)))

df_district1 <- df_merged %>%
  filter(occ.change %in% c(1,2)) %>%
  group_by(id.district, occ.change) %>%
  summarise(n = n()) %>%
  mutate(occ.change.freq = round(n / sum(n), 2)) %>%
  filter(occ.change == 2) %>%
  select(id.district, occ.change.freq)

df_district2 <- df_merged %>%
  # filter(occ.change == 2) %>%
  filter(occ.change %in% c(1,2)) %>%
  group_by(id.district, occ.impact) %>%
  summarise(n = n()) %>%
  mutate(occ.impact.freq = round(n / sum(n), 2)) %>%
  select(-n) %>%
  spread(occ.impact, occ.impact.freq)

df_district2[is.na(df_district2)] <- 0
names(df_district2)[-1] <- c("occ.impact.group0","occ.impact.group1","occ.impact.group2",
                             "occ.impact.group3","occ.impact.group4")

df_district3 <- merge(df_district1, df_district2, by="id.district")
df_district <- merge(df_district, df_district3, by="id.district")


# Factorize categorical variables
df_district$house.type <- factor(df_district$house.type, levels = 1:5)
df_district$habitability <- factor(df_district$habitability, levels = 1:4)
df_district$mental.health <- factor(df_district$mental.health, levels = 1:4)
df_district$concern <- factor(df_district$concern, levels = 1:3)
df_district$occupation <- factor(df_district$occupation, levels = 1:9)
# df_district$occ.change <- factor(df_district$occ.change, levels = 0:2)
# df_district$occ.impact <- factor(df_district$occ.impact, levels = 0:4)

############################################################################
############################## PLOTTING INFO ###############################
############################################################################

# ---------------- LOADING MADRID SHAPEFILE ----------------------
# Read this shape file with the rgdal library. 
library(rgdal)
my_spdf <- readOGR( 
  dsn = paste0(getwd(),"/DISTRITOS/DISTRITOS.shp"),
  layer = "DISTRITOS",
  verbose=FALSE,
  encoding="UTF-8")
# -- > Now you have a Spdf object (spatial polygon data frame).

my_spdf@data$CODDISTRIT <- 1:21


# ------------------ PLOT IT WITH GGPLOT2 ------------------------
# 'fortify' the data to get a dataframe format required by ggplot2
# library(broom)
# spdf_fortified <- tidy(my_spdf)

# Plot it
# library(ggplot2)
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void() 


# ------------------ CHLOROPETH MAP WITH LEAFLET ------------------------
library(leaflet)
library(RColorBrewer)

# Change UTM coordinates -> Lat/Long
my_spdf <- spTransform(my_spdf, CRS("+proj=longlat +datum=WGS84"))
# Merge data by district to spatial df
my_spdf@data <- merge(my_spdf@data, df_district, 
                      by.x="CODDISTRIT", by.y="id.district")
# Resetting index
rownames(my_spdf@data) <- 0:20
