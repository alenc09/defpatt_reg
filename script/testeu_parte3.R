# Fri Feb 04 16:18:13 2022 ------------------------------

#data ####
#read_xlsx("data/planilha_metricas.xlsx", sheet = 2) ->metricas_vegsec

## landscape metrics for each year ####
met_1985<- metricas_vegsec[metricas_vegsec$ano==1985,] 
met_1990<- metricas_vegsec[metricas_vegsec$ano==1990,]
met_1995<- metricas_vegsec[metricas_vegsec$ano==1995,]
met_2000<- metricas_vegsec[metricas_vegsec$ano==2000,]
met_2005<- metricas_vegsec[metricas_vegsec$ano==2005,]
met_2010<- metricas_vegsec[metricas_vegsec$ano==2010,] 
met_2015<- metricas_vegsec[metricas_vegsec$ano==2015,]

l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)
l_anos

#teste u para clumpy
wilcox_list<- function(x){
  x<- wilcox.test(CLUMPY ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_clumpy<- lapply(l_anos, wilcox_list)
names(u_clumpy)<- paste0("CLUMPY ", seq(1985,2015, by=5))
u_clumpy
capture.output(u_clumpy, file = "testeU_clumpy.txt")


###################################################################################################

###teste U para compara??o entre tipos de florestasl
#criar tabelas com m?tricas para cada data
metsecgeo_1985<- tab_geo[tab_geo$tab_ano==1985,] 
metsecgeo_1990<- tab_geo[tab_geo$tab_ano==1990,]
metsecgeo_1995<- tab_geo[tab_geo$tab_ano==1995,]
metsecgeo_2000<- tab_geo[tab_geo$tab_ano==2000,]
metsecgeo_2005<- tab_geo[tab_geo$tab_ano==2005,]
metsecgeo_2010<- tab_geo[tab_geo$tab_ano==2010,] 
metsecgeo_2015<- tab_geo[tab_geo$tab_ano==2015,]
l_metsec<- list(metsecgeo_1985, metsecgeo_1990, metsecgeo_1995, metsecgeo_2000, metsecgeo_2005,
                metsecgeo_2010, metsecgeo_2015)

metsecesp_1985<- tab_esp[tab_esp$tab_ano==1985,] 
metsecesp_1990<- tab_esp[tab_esp$tab_ano==1990,]
metsecesp_1995<- tab_esp[tab_esp$tab_ano==1995,]
metsecesp_2000<- tab_esp[tab_esp$tab_ano==2000,]
metsecesp_2005<- tab_esp[tab_esp$tab_ano==2005,]
metsecesp_2010<- tab_esp[tab_esp$tab_ano==2010,] 
metsecesp_2015<- tab_esp[tab_esp$tab_ano==2015,]
l_metsec<- list(metsecesp_1985, metsecesp_1990, metsecesp_1995, metsecesp_2000, metsecesp_2005,
                metsecesp_2010, metsecesp_2015)
#?rea n?cleo e isolamento - geo
wilcox_list<- function(x){
  x<- wilcox.test(area_core100 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c100<- lapply(l_metsec, wilcox_list)
names(usecgeo_c100)<- paste0("?rea_core100 ", seq(1985,2015, by=5))
usecgeo_c100
capture.output(usecgeo_c100, file = "testeU_secgeoc100.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core300 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c300<- lapply(l_metsec, wilcox_list)
names(usecgeo_c300)<- paste0("?rea_core300 ", seq(1985,2015, by=5))
usecgeo_c300
capture.output(usecgeo_c300, file = "testeU_secgeoc300.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core500 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c500<- lapply(l_metsec, wilcox_list)
names(usecgeo_c500)<- paste0("?rea_core500 ", seq(1985,2015, by=5))
usecgeo_c500
capture.output(usecgeo_c500, file = "C:/Users/Lucas Alencar/OneDrive/Documentos/Mestrado/INPE/R/parte_3/testeU_secgeoc500.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core1000 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c1000<- lapply(l_metsec, wilcox_list)
names(usecgeo_c1000)<- paste0("?rea_core1000 ", seq(1985,2015, by=5))
usecgeo_c1000
capture.output(usecgeo_c1000, file = "testeU_secgeoc1000.txt")

wilcox_list<- function(x){
  x<- wilcox.test(ENN_AM ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_enn<- lapply(l_metsec, wilcox_list)
names(usecgeo_enn)<- paste0("ENN_AM ", seq(1985,2015, by=5))
usecgeo_enn
capture.output(usecgeo_enn, file = "testeU_secgeoenn.txt")


##?rea n?cleo e isolamento - esp
wilcox_list<- function(x){
  x<- wilcox.test(area_core100 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c100<- lapply(l_metsec, wilcox_list)
names(usecesp_c100)<- paste0("?rea_core100 ", seq(1985,2015, by=5))
usecesp_c100
capture.output(usecesp_c100, file = "testeU_secespc100.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core300 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c300<- lapply(l_metsec, wilcox_list)
names(usecesp_c300)<- paste0("?rea_core300 ", seq(1985,2015, by=5))
usecesp_c300
capture.output(usecesp_c300, file = "testeU_secespc300.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core500 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c500<- lapply(l_metsec, wilcox_list)
names(usecesp_c500)<- paste0("?rea_core500 ", seq(1985,2015, by=5))
usecesp_c500
capture.output(usecesp_c500, file = "testeU_secespc500.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core1000 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c1000<- lapply(l_metsec, wilcox_list)
names(usecesp_c1000)<- paste0("?rea_core1000 ", seq(1985,2015, by=5))
usecesp_c1000
capture.output(usecesp_c1000, file = "testeU_secespc1000.txt")

wilcox_list<- function(x){
  x<- wilcox.test(ENN_AM ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_enn<- lapply(l_metsec, wilcox_list)
names(usecesp_enn)<- paste0("ENN_AM ", seq(1985,2015, by=5))
usecesp_enn
capture.output(usecesp_enn, file = "testeU_secespenn.txt")