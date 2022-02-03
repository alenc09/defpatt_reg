setwd("C:/Users/Lucas/OneDrive/Documentos/Mestrado/INPE/R/parte_3")
install.packages("plyr")
library("dplyr")
library("ggplot2")
library("ggsignif")

#importar tabela flortotal
tab_flortotal<- read.delim("planilha_metricas_total.csv", sep = ";", dec = ",")
str(tab_flortotal)
tab_flortotal$padrao<- factor(tab_flortotal$padrao, levels = c("geo","esp"), ordered = T)
tab_flortotal$ano<- factor(tab_flortotal$ano)

#fazer boxplot para quantidade de floresta secundaria
bp_fsec<- ggplot(tab_flortotal, aes(x = ano, y = areaha_florsec, fill=padrao)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área de vegetação secundária (ha)")+
  theme_classic()+
  scale_fill_manual("Padrão", labels= c("GEO", "ESP"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(17000, 28000, 38000, 36000, 47000, 47000, 51000), 
            xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
            xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
            annotations = c("ns","ns","**", "**", "ns", "ns", "ns"),
            tip_length = 0)
bp_fsec

#boxplot para clumpy
tab_clump<- tab_flortotal[-54,]
bp_clumpy<- ggplot(tab_clump, aes(x = ano, y = CLUMPY, fill=padrao)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name= "CLUMPY")+
  theme_classic()+
  scale_fill_manual("Padrão", labels= c("GEO", "ESP"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(0.91, 0.89, 0.88, 0.87, 0.87, 0.89, 0.88), 
            xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
            xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
            annotations = c("0.09","0.02","0.07", "0.17", "0.12", "0.03", "0.03"),
            tip_length = 0, textsize = 2.5)
bp_clumpy
ggsave("C:/Users/Lucas/OneDrive/Documentos/Mestrado/dissertacao/2art_/bp_clumpy.png", plot=bp_clumpy, width = 174,
       height = 129, units = "mm", dpi = 300, limitsize = F, device = png(res = 300, units = "mm"))


############################################################################################
# boxplots para comparação das florestas secundárias

#montar tabelas com florprima e florsec
tab_florprima<- read.delim("planilha_metricas_prima.csv", dec = ",", sep = ";")
tab_florprima$padrao<- factor(tab_florprima$padrao, levels = c("geo","esp"), ordered = T)
tab_florprima$ano<- factor(tab_florprima$ano)
str(tab_florprima)
str(tab_flortotal)
florfac<- as.factor(rep(c("prima", "sec"), ordered = T, each = 49))
tab_ano<- tab_florprima$ano
rm(tab_geo)
rm(tab_esp)

#tabela geo 
tab_geoprima<- tab_florprima[tab_florprima$padrao=="geo", c(17:20, 23)]
tab_geototal<- tab_flortotal[tab_flortotal$padrao=="geo", c(18:21, 24)]
tab_geo<- bind_rows(tab_geoprima, tab_geototal)
tab_geo<- cbind(tab_ano, florfac, tab_geo)
str(tab_geoprima)
str(tab_geototal)
str(tab_geo)

#área núcleo GEO
bpsec_c100<- ggplot(tab_geo, aes(x = tab_ano, y = area_core100, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 100m)")+
  theme_classic()+
  labs(title = "Geométrico")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsec_c100

bpsec_c300<- ggplot(tab_geo, aes(x = tab_ano, y = area_core300, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 300m)")+
  theme_classic()+
  labs(title = "Geométrico")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsec_c300

bpsec_c500<- ggplot(tab_geo, aes(x = tab_ano, y = area_core500, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 500m)")+
  theme_classic()+
  labs(title = "Geométrico")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsec_c500

bpsec_c1000<- ggplot(tab_geo, aes(x = tab_ano, y = area_core1000, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 1000m)")+
  theme_classic()+
  labs(title = "Geométrico")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsec_c1000

#isolamento GEO
install.packages("ggplot2")
library(ggplot2)
bpsec_enn<- ggplot(tab_geo, aes(x = tab_ano, y = ENN_AM, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "ENN_AM")+
  theme_classic()+
  labs(title = "Geométrico")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsec_enn

#tabela esp
tab_espprima<- tab_florprima[tab_florprima$padrao=="esp", c(17:20, 23)]
tab_esptotal<- tab_flortotal[tab_flortotal$padrao=="esp", c(18:21, 24)]
tab_esp<- bind_rows(tab_espprima, tab_esptotal)
tab_esp<- cbind(tab_ano, florfac, tab_esp)
str(tab_esp)


#área núcleo ESP
bpsecesp_c100<- ggplot(tab_esp, aes(x = tab_ano, y = area_core100, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 100m)")+
  theme_classic()+
  labs(title = "Espinha de peixe")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsecesp_c100

bpsecesp_c300<- ggplot(tab_esp, aes(x = tab_ano, y = area_core300, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 300m)")+
  theme_classic()+
  labs(title = "Espinha de peixe")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsecesp_c300

bpsecesp_c500<- ggplot(tab_esp, aes(x = tab_ano, y = area_core500, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 500m)")+
  theme_classic()+
  labs(title = "Espinha de peixe")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsecesp_c500

bpsecesp_c1000<- ggplot(tab_esp, aes(x = tab_ano, y = area_core1000, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "Área core (borda de 1000m)")+
  theme_classic()+
  labs(title = "Espinha de peixe")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsecesp_c1000

#isoelamento esp
bpsecesp_enn<- ggplot(tab_esp, aes(x = tab_ano, y = ENN_AM, fill=florfac)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "ENN_AM",limits = c(NA, 140))+
  theme_classic()+
  labs(title = "Espinha de peixe")+
  scale_fill_manual("Floresta", labels= c("Primária", "Primária + Secundária"),breaks= c("prima", "sec"),
                    values = c("grey80","grey20"))
bpsecesp_enn

##########################################################################################################
#comparar isolamento com florestas secundárias
gesec_enn<- ggplot(tab_flortotal, aes(x = tab_ano, y = ENN_AM, fill=padrao)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "ENN_AM")+
  theme_classic()+
  scale_fill_manual("Padrão", labels= c("GEO", "ESP"),breaks= c("geo", "esp"),
                    values = c("grey80","grey20"))
gesec_enn

gesec_enn<- ggplot(tab_flortotal, aes(x = tab_ano, y = ENN_MN, fill=padrao)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Ano") + 
  scale_y_continuous(name= "ENN_AM")+
  theme_classic()+
  scale_fill_manual("Padrão", labels= c("GEO", "ESP"),breaks= c("geo", "esp"),
                    values = c("grey80","grey20"))
gesec_enn
