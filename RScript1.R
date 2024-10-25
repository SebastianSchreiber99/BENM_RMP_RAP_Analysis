## ----libraries------------------------------------

packages_to_load <- c("basemaps", "dplyr", "geodata", "ggmap", "ggplot2", "sf", "terra", "tidyterra", "tidyverse", "tmap", "fasterize", "ctmm", "purrr", "car", "raster", "ggpubr", "fpp2") # create vector of R package names that we know are needed in the rest of the code

#--- Use this R-Chunk to load all your libraries!
#--- Load all packages at once
lapply(packages_to_load, library, character.only = TRUE) # now that all the needed packages are installed on our computer, we pass the list of needed packages to R's load function ('library'). We use lapply to apply the library load function to all packages in our list

NC_LandType <- sf::read_sf("C:/Users/spsch/Documents/R/Dugout/Shapefiles/NC Land Shapefiles/NC_LandType.shp")
CNP_bound <- sf::read_sf("C:/Users/spsch/Documents/R/Dugout/NC Land Shapefiles/CANY_tracts/CANY_tracts.shp")

#Filter by land type
NC_Bench <- filter(NC_LandType, Type == "Upland Benches")
NC_Bottom <- filter(NC_LandType, Type == "Canyon Botttomlands")
NC_Riparian <- filter(NC_LandType, Type == "Riparian")

#Save shapefiles for RAP import
sf::write_sf(sf::st_union(NC_Bench$geometry),
             file.path("C:/Users/spsch/Documents/R/Dugout", "NC_Bench.shp.zip"),
             driver = "ESRI Shapefile")
sf::write_sf(sf::st_union(NC_Bottom$geometry),
             file.path("C:/Users/spsch/Documents/R/Dugout", "NC_Bottom.shp.zip"),
             driver = "ESRI Shapefile")
sf::write_sf(sf::st_union(NC_Riparian$geometry),
             file.path("C:/Users/spsch/Documents/R/Dugout", "NC_Riparian.shp.zip"),
             driver = "ESRI Shapefile")
sf::write_sf(sf::st_union(NC_LandType$geometry),
             file.path("C:/Users/spsch/Documents/R/Dugout", "NC_All.shp.zip"),
             driver = "ESRI Shapefile")
sf_use_s2(FALSE)
sf::write_sf(sf::st_union(CNP_bound$geometry),
             file.path("C:/Users/spsch/Documents/R/Dugout", "CNP_Bound.shp.zip"),
             driver = "ESRI Shapefile")

#Import Polygon Average CSVs for Perennial Biomass
BJ_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/BJ_soil_avg_biomass.csv")
CNP_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/Salt_Creek_avg_biomass.csv")
NC_bench_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_bench_avg_biomass.csv")
NC_bottom_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_bottom_avg_biomass.csv")
NC_riparian_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_riparian_avg_biomass.csv")
NC_all_avg_biomass <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_all_avg_biomass.csv")

#Create dataframes to input trend data
Avg_biomass_trend <- data.frame(matrix(nrow = 6, ncol = 3))
rownames(Avg_biomass_trend) <- c("BJ", "CNP", "NC_bench", "NC_bottom", "NC_riparian", "NC_all")
colnames(Avg_biomass_trend) <- c("Trend 1986-2023", "95% CI Low", "95% CI High")

#Run lm's to estimate average trend
m_BJ <- lm(data = BJ_avg_biomass, formula = PFG ~ year)
m_CNP <- lm(data = CNP_avg_biomass, formula = PFG ~ year)
m_NC_bench <- lm(data = NC_bench_avg_biomass, formula = PFG ~ year)
m_NC_bottom <- lm(data = NC_bottom_avg_biomass, formula = PFG ~ year)
m_NC_riparian <- lm(data = NC_riparian_avg_biomass, formula = PFG ~ year)
m_NC_all <- lm(data = NC_all_avg_biomass, formula = PFG ~ year)

Avg_biomass_lm_list <- list(m_BJ, m_CNP, m_NC_bench, m_NC_bottom, m_NC_riparian, m_NC_all)

#Fill in df
for(i in 1:6){Avg_biomass_trend[i, 1] <- Avg_biomass_lm_list[[i]]$coefficients["year"]}

for(i in 1:6){Avg_biomass_trend[i, 2:3] <-
  confint(Avg_biomass_lm_list[[i]], parm = "year", level = 0.95)}




#Import Polygon Average CSVs for Cover
BJ_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/BJ_soil_avg_cover.csv")
CNP_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/Salt_Creek_avg_cover.csv")
NC_bench_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_bench_avg_cover.csv")
NC_bottom_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_bottom_avg_cover.csv")
NC_riparian_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_riparian_avg_cover.csv")
NC_all_avg_cover <- read_csv("C:/Users/spsch/Documents/R/Dugout/RAP CSVs/NC_all_avg_cover.csv")

#Create dataframes to input trend data
Avg_cover_trend <- data.frame(matrix(nrow = 30, ncol = 6))
colnames(Avg_cover_trend) <- c("Area", "Variable", "Trend 1986-2023", "95% CI Low", "95% CI High", "SEM")

#Run lm's to estimate average trend
m2_BJ <- lm(data = BJ_avg_cover, formula = AFG ~ year)
m2_CNP <- lm(data = CNP_avg_cover, formula = AFG ~ year)
m2_NC_bench <- lm(data = NC_bench_avg_cover, formula = AFG ~ year)
m2_NC_bottom <- lm(data = NC_bottom_avg_cover, formula = AFG ~ year)
m2_NC_riparian <- lm(data = NC_riparian_avg_cover, formula = AFG ~ year)
m2_NC_all <- lm(data = NC_all_avg_cover, formula = AFG ~ year)

m3_BJ <- lm(data = BJ_avg_cover, formula = PFG ~ year)
m3_CNP <- lm(data = CNP_avg_cover, formula = PFG ~ year)
m3_NC_bench <- lm(data = NC_bench_avg_cover, formula = PFG ~ year)
m3_NC_bottom <- lm(data = NC_bottom_avg_cover, formula = PFG ~ year)
m3_NC_riparian <- lm(data = NC_riparian_avg_cover, formula = PFG ~ year)
m3_NC_all <- lm(data = NC_all_avg_cover, formula = PFG ~ year)

m4_BJ <- lm(data = BJ_avg_cover, formula = SHR ~ year)
m4_CNP <- lm(data = CNP_avg_cover, formula = SHR ~ year)
m4_NC_bench <- lm(data = NC_bench_avg_cover, formula = SHR ~ year)
m4_NC_bottom <- lm(data = NC_bottom_avg_cover, formula = SHR ~ year)
m4_NC_riparian <- lm(data = NC_riparian_avg_cover, formula = SHR ~ year)
m4_NC_all <- lm(data = NC_all_avg_cover, formula = SHR ~ year)

m5_BJ <- lm(data = BJ_avg_cover, formula = TRE ~ year)
m5_CNP <- lm(data = CNP_avg_cover, formula = TRE ~ year)
m5_NC_bench <- lm(data = NC_bench_avg_cover, formula = TRE ~ year)
m5_NC_bottom <- lm(data = NC_bottom_avg_cover, formula = TRE ~ year)
m5_NC_riparian <- lm(data = NC_riparian_avg_cover, formula = TRE ~ year)
m5_NC_all <- lm(data = NC_all_avg_cover, formula = TRE ~ year)

m6_BJ <- lm(data = BJ_avg_cover, formula = BGR ~ year)
m6_CNP <- lm(data = CNP_avg_cover, formula = BGR ~ year)
m6_NC_bench <- lm(data = NC_bench_avg_cover, formula = BGR ~ year)
m6_NC_bottom <- lm(data = NC_bottom_avg_cover, formula = BGR ~ year)
m6_NC_riparian <- lm(data = NC_riparian_avg_cover, formula = BGR ~ year)
m6_NC_all <- lm(data = NC_all_avg_cover, formula = BGR ~ year)

Avg_cover_lm_list <- list(m2_BJ, m2_CNP, m2_NC_bench, m2_NC_bottom, m2_NC_riparian, m2_NC_all,
                          m3_BJ, m3_CNP, m3_NC_bench, m3_NC_bottom, m3_NC_riparian, m3_NC_all,
                          m4_BJ, m4_CNP, m4_NC_bench, m4_NC_bottom, m4_NC_riparian, m4_NC_all,
                          m5_BJ, m5_CNP, m5_NC_bench, m5_NC_bottom, m5_NC_riparian, m5_NC_all,
                          m6_BJ, m6_CNP, m6_NC_bench, m6_NC_bottom, m6_NC_riparian, m6_NC_all)

#Fill in df
Avg_cover_trend[, 1] <- rep(c("BJ", "CNP", "NC_bench", "NC_bottom", "NC_riparian", "NC_all"), 5)
Avg_cover_trend[, 2] <- c(rep("AFG", 6), rep("PFG", 6), rep("SHR", 6), rep("TRE", 6), rep("BGR", 6))

for(i in 1:30){Avg_cover_trend[i, 3] <- Avg_cover_lm_list[[i]]$coefficients["year"]}

for(i in 1:30){Avg_cover_trend[i, 4:5] <- confint(Avg_cover_lm_list[[i]], parm = "year", level = 0.95)}

for(i in 1:30){Avg_cover_trend[i, 6] <- summary(Avg_cover_lm_list[[i]])$coefficients[2, 2]}




#NC Stocking rate
Summer_trl <- (200 * .25)
Fall_trl <- ((400 * .5) + (162.5*0.05))
Winter_hfrs_bulls <- (((40*2.5) + ((22.5*1.5)*2.5))/2)

NC_AUM <- (Summer_trl + Fall_trl + Winter_hfrs_bulls)
LB_ac_use <- (NC_AUM * 750 / sum(NC_LandType$Area_ac))



#Save CSVs
write_csv(Avg_biomass_trend, "C:/Users/spsch/Documents/R/Dugout/Results/Summary_Biomass.csv")
write_csv(Avg_cover_trend, "C:/Users/spsch/Documents/R/Dugout/Results/Cover_Summary_Tall.csv")



#Plot Biomass Data
UsePas <- c(rep(LB_ac_use, 19), rep(NA, 19))
UseCur <- c(rep(NA, 18), rep(LB_ac_use, 20))
Biomass_Plot_df <- cbind.data.frame(NC_all_avg_biomass, UseCur, UsePas)

Biomass_Plot <- ggplot(Biomass_Plot_df, aes(x = year)) +
  geom_line(aes(y = HER, col = "Herbacious Prodcution")) +
  geom_line(aes(y = UseCur, col = "Forage Consumption")) +
  geom_line(aes(y = UsePas), linetype = "dashed", col = "red") +
  labs(title = "Production versus Consumption",
       y = "Biomass (lb/ac)") +
  scale_y_continuous(breaks = seq(0, 350, by = 50)) +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(filename = "C:/Users/spsch/Documents/R/Dugout/Results/Biomass Plot.png",
       plot = Biomass_Plot, width = 6, height = 5, dpi = 320)

Production_Plot <- ggplot(NC_all_avg_biomass, aes(x = year)) +
  geom_line(aes(y = PFG, col = "North Cottonwood All Areas")) +
  geom_line(aes(y = BJ_avg_biomass$PFG, col = "Bridger Jack WSA")) +
  geom_line(aes(y = CNP_avg_biomass$PFG, col = "CNP (Salt Creek)")) +
  labs(title = "Production Trends",
       y = "Perennial Herbacious Biomass (lb/ac)") +
  scale_y_continuous(breaks = seq(0, 350, by = 50)) +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(filename = "C:/Users/spsch/Documents/R/Dugout/Results/Production Plot.png",
       plot = Production_Plot, width = 6, height = 5, dpi = 320)




# Plot Cover Trends
Cover_trends_df <- Avg_cover_trend %>%
  mutate(Area = recode_factor(Area, BJ = "Bridger Jack WSA",
                CNP = "CNP (Salt Creek)",
                NC_bench = "NC Upland Benches",
                NC_bottom = "NC Canyon Bottomlands",
                NC_riparian = "NC Riparian",
                NC_all = "NC All Areas")) %>%
  arrange(Area)

Cover_trends_plot <- ggplot(Cover_trends_df, aes(x = Variable, y = `Trend 1986-2023`)) +
  geom_bar(stat= "identity",
           position = position_dodge(),
           aes(fill = Area),
           color = "black") +
  geom_errorbar(data = Avg_cover_trend,
                aes(ymin = `Trend 1986-2023` - `SEM`,
                    ymax = `Trend 1986-2023` + `SEM`),
                stat = "identity",
                position = position_dodge2(padding = .7)) +
  ylim(-0.4, 0.32) +
  labs(title = "Vegetation Cover Trends",
       y = "Change in Percent Cover") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_fill_manual(values = c("darkorange3",
                        "brown",
                        "lightcyan",
                        "cadetblue2",
                        "slategray1",
                        "navyblue"))
                        
ggsave(filename = "C:/Users/spsch/Documents/R/Dugout/Results/Cover Trends Plot.png",
       plot = Cover_trends_plot, width = 6, height = 5, dpi = 320)




#Scrap Code

  #geom_line(aes(y = NC_bench_avg_biomass$PFG, col = "N.C. Upland Benches")) +
  #geom_line(aes(y = NC_bottom_avg_biomass$PFG, col = "N.C. Canyon Bottomlands")) +
  #geom_line(aes(y = NC_riparian_avg_biomass$PFG, col = "N.C. Riparian")) +


