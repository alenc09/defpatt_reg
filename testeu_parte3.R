setwd("C:/Users/User/OneDrive/Documentos/Mestrado/INPE/R/parte_3")

#importar tabela flortotal e criar tabelas para cada ano
tab_flortotal<- read.csv2("planilha_metricas_total.csv")
str(tab_flortotal)
tab_flortotal$padrao<- factor(tab_flortotal$padrao, levels = c("geo","esp"), ordered = T)
tab_flortotal$ano<- factor(tab_flortotal$ano)

#criar tabelas com métricas para cada data
met_1985<- tab_flortotal[tab_flortotal$ano==1985,] 
met_1990<- tab_flortotal[tab_flortotal$ano==1990,]
met_1995<- tab_flortotal[tab_flortotal$ano==1995,]
met_2000<- tab_flortotal[tab_flortotal$ano==2000,]
met_2005<- tab_flortotal[tab_flortotal$ano==2005,]
met_2010<- tab_flortotal[tab_flortotal$ano==2010,] 
met_2015<- tab_flortotal[tab_flortotal$ano==2015,]


#teste u para quantidade de floresta secundária
l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)
l_anos
wilcox_list<- function(x){
  x<- wilcox.test(areaha_florsec ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_florsec<- lapply(l_anos, wilcox_list)
names(u_florsec)<- paste0("Área floresta secundária ", seq(1985,2015, by=5))
u_florsec
capture.output(u_florsec, file = "testeU_florsec.txt")

#teste u para CAI100
wilcox_list<- function(x){
  x<- wilcox.test(CAI_AM100 ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_cai100<- lapply(l_anos, wilcox_list)
names(u_cai100)<- paste0("CAI_AM100 ", seq(1985,2015, by=5))
u_cai100
capture.output(u_cai100, file = "testeU_cai100.txt")

#teste u para CAI300
wilcox_list<- function(x){
  x<- wilcox.test(CAI_AM300 ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_cai300<- lapply(l_anos, wilcox_list)
names(u_cai300)<- paste0("CAI_AM300 ", seq(1985,2015, by=5))
u_cai300
capture.output(u_cai300, file = "testeU_cai300.txt")

#teste u para CAI500
wilcox_list<- function(x){
  x<- wilcox.test(CAI_AM500 ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_cai500<- lapply(l_anos, wilcox_list)
names(u_cai500)<- paste0("CAI_AM500 ", seq(1985,2015, by=5))
u_cai500
capture.output(u_cai500, file = "testeU_cai500.txt")

#teste u para CAI1000 
wilcox_list<- function(x){
  x<- wilcox.test(CAI_AM1000 ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_cai1000<- lapply(l_anos, wilcox_list)
names(u_cai1000)<- paste0("CAI_AM1000 ", seq(1985,2015, by=5))
u_cai1000
capture.output(u_cai1000, file = "testeU_cai1000.txt")

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

###teste U para comparação entre tipos de florestasl
#criar tabelas com métricas para cada data
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
#área núcleo e isolamento - geo
wilcox_list<- function(x){
  x<- wilcox.test(area_core100 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c100<- lapply(l_metsec, wilcox_list)
names(usecgeo_c100)<- paste0("Área_core100 ", seq(1985,2015, by=5))
usecgeo_c100
capture.output(usecgeo_c100, file = "testeU_secgeoc100.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core300 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c300<- lapply(l_metsec, wilcox_list)
names(usecgeo_c300)<- paste0("Área_core300 ", seq(1985,2015, by=5))
usecgeo_c300
capture.output(usecgeo_c300, file = "testeU_secgeoc300.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core500 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c500<- lapply(l_metsec, wilcox_list)
names(usecgeo_c500)<- paste0("Área_core500 ", seq(1985,2015, by=5))
usecgeo_c500
capture.output(usecgeo_c500, file = "C:/Users/Lucas Alencar/OneDrive/Documentos/Mestrado/INPE/R/parte_3/testeU_secgeoc500.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core1000 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecgeo_c1000<- lapply(l_metsec, wilcox_list)
names(usecgeo_c1000)<- paste0("Área_core1000 ", seq(1985,2015, by=5))
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


##área núcleo e isolamento - esp
wilcox_list<- function(x){
  x<- wilcox.test(area_core100 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c100<- lapply(l_metsec, wilcox_list)
names(usecesp_c100)<- paste0("Área_core100 ", seq(1985,2015, by=5))
usecesp_c100
capture.output(usecesp_c100, file = "testeU_secespc100.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core300 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c300<- lapply(l_metsec, wilcox_list)
names(usecesp_c300)<- paste0("Área_core300 ", seq(1985,2015, by=5))
usecesp_c300
capture.output(usecesp_c300, file = "testeU_secespc300.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core500 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c500<- lapply(l_metsec, wilcox_list)
names(usecesp_c500)<- paste0("Área_core500 ", seq(1985,2015, by=5))
usecesp_c500
capture.output(usecesp_c500, file = "testeU_secespc500.txt")

wilcox_list<- function(x){
  x<- wilcox.test(area_core1000 ~ florfac, data = x, 
                  alternative= "t", exact = T)
}
usecesp_c1000<- lapply(l_metsec, wilcox_list)
names(usecesp_c1000)<- paste0("Área_core1000 ", seq(1985,2015, by=5))
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