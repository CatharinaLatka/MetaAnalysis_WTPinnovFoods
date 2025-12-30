#################################
# "Graphical abstract"
#author: "Catharina Latka"
#################################


#load packages
library(glue)
library(tidyverse)
library(readxl)
library(gtsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#read data
data_ori<-read_csv(glue("data/processed/data.csv"))


data_ori<-data_ori%>%mutate(product_type=case_when(product_type=="plant_based"~"plant-based",
                                                        product_type=="improved_animal_welfare"~"improved animal welfare",
                                                        product_type=="improved_health"~"improved health",
                                                        product_type=="improved_environmental_footprint"~"improved environment",
                                                        product_type=="lab_gene_engin"~"lab/gene-engineered",
                                                        TRUE~product_type
))

#drop missing years
data_ori<-data_ori%>%filter(!(is.na(first_year)))



cons<-data_ori%>%filter(consumer_type!="average")
length(unique(cons$study_ID))


#filter for averaged
data<-data_ori%>%filter(consumer_type=="average")
length(unique(data$study_ID))


#remove some unnstudy_ID#remove some unnecessary variables
data<-data%>%dplyr::select(-article,-product,-period_data_collection,-dependent_variable,-consumer_group_paper,-unit,-reference_product,
                           -group_share,-WTP_estimate,-WTP_adjusted,-WTP_OR_logit_coefficient,-SE_OR_SD,-price_WTP_reference,-study_ID,
                           -Price_estimate,-SE_price,-SE_WTP)


#create indicator if SE present
data<-data%>%
  mutate(no_SE=if_else(is.na(relative_SE_WTP),"no SE estimate","with SE estimate"))


# Load world map as an sf object
world <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
# Ensure country names match
data$country_low <- tolower(data$country)


# Simple centroids per country for this example
world_sf <- ne_countries(scale = "medium", returnclass = "sf") #here we do not want map units
world_sf<-world_sf%>%filter(!(sovereignt=="France"&geounit!="France"))

centroids <- st_centroid(world_sf)
#FRANCE IS A SPECIAL CASE
points <- st_point_on_surface(world_sf)
points<-points%>%filter(admin=="France")
centroids_coords_FRA <- cbind(points, st_coordinates(points))
centroids_coords_FRA$country_low <- tolower(centroids_coords_FRA$admin)
centroids_coords_FRA<-centroids_coords_FRA%>%dplyr::select(country_low,X,Y)

#GENERAL CASE
centroids$country_low <- tolower(centroids$admin)
centroids_coords <- cbind(centroids, st_coordinates(centroids))

centroids_coords<-centroids_coords%>%dplyr::select(country_low,X,Y)

#REPLACE FRANCE
centroids_coords<-centroids_coords%>%filter(country_low!="france")
centroids_coords<-rbind(centroids_coords,centroids_coords_FRA)




data<-data%>%
  mutate(country_low=if_else(country_low=="usa","united states of america",country_low))%>%
  mutate(country_low=if_else(country_low=="uk","united kingdom",country_low))%>%
  group_by(country_low)%>%
  mutate(mean_rel_diff=mean(relative_difference))%>%
  mutate(count=n())%>%
  ungroup()

data2<-data%>%
  dplyr::select(country_low,sample_size)%>%distinct()%>%
  group_by(country_low)%>%
  mutate(count_studies=n())%>%
  ungroup()

world<-world%>%filter(!(sovereignt=="France"&geounit!="France"))

world$sovereignt <- tolower(world$sovereignt)



# Merge the world map with our data
map_data <- left_join(world, data, by = c("sovereignt" = "country_low"))
map_data <- left_join(map_data, data2)


pie_data <- data %>%
  mutate(country_low=if_else(country_low=="usa","united states of america",country_low))%>%
  mutate(country_low=if_else(country_low=="uk","united kingdom",country_low))%>%
  dplyr::select(country_low,product_type)%>%
  group_by(country_low, product_type) %>%
  mutate(count=1)%>%
  drop_na(country_low)%>%
  summarise(count = sum(count), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = product_type, values_from = count, values_fill = 0)%>%
  filter(country_low!="european countries")

pie_data <- left_join(pie_data, centroids_coords, by = c("country_low" = "country_low"))

pie_data <- pie_data %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric) & !c(X, Y))),
         radius = sqrt(total)) 


category_cols <- c("plant-based","lab/gene-engineered","insects", "improved health", "improved environment","improved animal welfare")
pie_data[category_cols] <- lapply(pie_data[category_cols], as.numeric)



library(scatterpie)
library(ggnewscale)


gr_abst<-ggplot() +
 
  geom_sf(data = map_data, aes(fill = mean_rel_diff), color = "darkgrey") +
  scale_fill_gradient2(low = '#d8b365', high = '#5ab4ac', na.value = "lightgrey",name="Average marginal\nwillingness-to-pay") +
  new_scale_fill() +
  geom_scatterpie(data = pie_data,
                  aes(x = X, y = Y, r = radius),
                  cols = category_cols,
                  color = NA) +theme_bw()+
  scale_fill_viridis_d(name = "Sustainably innovated\nproduct category",direction = -1)+
  theme_bw()+geom_scatterpie_legend(
    pie_data$radius,
    x = -160,
    y = -55,
    n = 3,
    labeller = function(r) round((r )^2))+
  annotate(
    "text",
    x = -160, y = -45,   # adjust to appear above your topmost pie
    label = "Number of studies",
    size = 3.5
  )+
  theme(legend.position = c(0.11, 0.51),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(glue("data/output/figures/5_graphical_abstract.pdf"),gr_abst,height=14,width=20,unit="in")
ggsave(glue("data/output/figures/5_graphical_abstract.png"),gr_abst,height=14,width=20,unit="in")


