#conf <- geojsonio::geojson_read('D:/OneDrive - CGIAR/Analysis/CSO/KEN/clim_conflict_ips_overlays  (ACCLED-1997-2022).geojson')

#root <- '//alliancedfs.alliance.cgiar.org/WS18_Afrca_K_N_ACO/1.Data/Palmira/CSO/data/' 
rm(list=ls(all=TRUE))
library(terra)
root <- 'D:/OneDrive - CGIAR/Analysis/CSO/'

### KENYA
iso <- 'KEN'
conf <- geojsonio::geojson_sf(paste0(root,iso,'/KEN_megapixels.geojson'))

#bdy1 <- geodata::gadm(country=iso, level=1, path=root)
bdy1 <- vect('D:/OneDrive - CGIAR/Data/Admin/Kenya_county_dd.shp')
bdy <- sf::st_as_sf(bdy1)
bdy <- sf::st_cast(bdy)            
temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
i <- temp$intersect_conf_clim
temp$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]"] <-
  "High conflict + Low drought stress" 
temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
  "High conflict + Moderate-Low drought stress"
temp$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
  "High conflict + Moderate-High drought stress"
temp$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
  "High conflict + High drought stress"
others <- conf[conf$conflict_clust_label!="High conflict"  ,]
others$intersect_conf_clim <- 'Low-Moderate Conflict-Climate Co-occurence'

temp <- rbind(temp, others, deparse.level = 1)
i <- temp$intersect_conf_clim
temp$clust[i=="Low-Moderate Conflict-Climate Co-occurence"] <- 5

temp$clust[i=="High conflict + Low drought stress"] <- 4
temp$clust[i=="High conflict + Moderate-Low drought stress"] <- 3
temp$clust[i=="High conflict + Moderate-High drought stress"] <- 2
temp$clust[i=="High conflict + High drought stress"] <- 1
temp$clust <- as.factor(temp$clust)
labs <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress",
          "High conflict + Moderate-Low drought stress", "High conflict + Low drought stress", "Low-Moderate Conflict-Climate Co-occurence")
temp$label[temp$id=='502'] <- '4'
temp$label[temp$id=='435'] <- '3'
temp$label[temp$id=='472'] <- '2'
temp$label[temp$id=='498'] <- '1'
library(tmap)
library(mapview)
tmap_mode("plot")
map <- tm_shape(temp) +
  tm_fill(col= "clust", palette= c("red4", "red2", "orange2", "yellow", "grey85"), title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars="NAME_3") + #"-YlOrRd"
  tm_text("label", col='white', size = 1.1)+
  tm_shape(bdy)+
  tm_text("county", size = 1.0, col='black', remove.overlap = TRUE)+ 
  tm_borders(col = "black")+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(legend.outside=F, 
            legend.text.size = 0.9,
            legend.title.size= 1.2,
            legend.frame=F,
            legend.just = c("left", "top"), 
            legend.position  = c("left", "bottom"),
            legend.width = 0.75,
            inner.margins = c(0.02, 0.02, 0.05, 0.02)
  )
map 

out <- "D:/OneDrive - CGIAR/Analysis/MultiMethodsPaper/"
tmap_save(map,  dpi= 600,  height=8, width=10, units="in",
          filename=paste0(out,iso,"_High_confict_climate_intersection.png"))
write.csv(temp[temp$clust==1,], paste0(out,iso,"_High_conflict_high_climate_Hazad.csv"))
