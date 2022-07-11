# Fri Feb 25 17:00:48 2022 ------------------------------
#script to create random points in classified raster files

setwd("/media/alenc/ADATA SC685/documentos/mestrado/INPE/")

#library----
library(raster)
library(sf)
library(here)
library(writexl)
library(dplyr)

#data----
#raster(here("geometrico/qgis_23069_2/flortotal_23069_2_2010.tif"))-> FT_00167_2015
list.files(path ="/media/alenc/ADATA SC685/documentos/mestrado/INPE/",
           pattern = "flortotal_[0-9]{5}_[0-9]{4}.tif$|flortotal_[0-9]{5}_2_[0-9]{4}.tif$",
           recursive = T,
           full.names = F)-> list_points

lapply(here(list_points), raster) -> raster_list

#for(i in list_points) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

#analysis----
#sampleStratified(x= FT_00167_2015, size= 30, xy=T) -> points_00167_2015
lapply(raster_list, function(x){sampleStratified(x = x, size = 30, xy=T)}) -> teste

#export----
#write.csv(x = points_00167_2015, file = "/home/alenc/Documents/mestrado/cap2_reg/points/points_00167_2015")

for (i in seq_along(teste)){
  write.csv(x = teste[[i]], file = paste0(teste[i]%>%
                                              as.data.frame()%>%
                                              select(4)%>%
                                              names(), ".csv"))
}
