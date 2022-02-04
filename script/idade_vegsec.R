# Fri Feb 04 09:19:18 2022 ------------------------------

#Secondary vegetation age and growth####

#Libraries####
library(ggplot2)
library(ggsignif)
library(readxl)
library(tidyr)
library(dplyr)
library(fitdistrplus)
library(lsmeans)
library(piecewiseSEM)

#importar tabela####
tb_vegsec<- read_xlsx("C:/Users/Lucas/OneDrive/Documentos/Mestrado/INPE/idade_florsec.xlsx")
tb_vegsec$padrao<- factor(tb_vegsec$padrao, levels = c("geo","esp"), ordered = T)
tb_vegsec2<- gather(tb_vegsec[,-9], key = cat_idade, value = area_vegsec, "5_anos": "30_anos", factor_key = T)
str(tb_vegsec2)
tb_desmat<- read_xlsx("planilha_metricas.xlsx")
glimpse(tb_desmat)
tb_desmat<- mutate(tb_desmat, area_desmat = 250000 - areaha_florprima )
desmat_2015<- tb_desmat$area_desmat[tb_desmat$ano == 2015]
tb_vegsec2<- cbind(tb_vegsec2, desmat_2015)
tb_vegsec2<- mutate(tb_vegsec2, vegsec_desmat = (area_vegsec/desmat_2015)*100)

#Boxplot #### 
bp_idade<- ggplot(tb_vegsec2, aes(x = cat_idade, y = area_vegsec, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Secondary vegetation age (years)",
                   labels = c("5", "5-10", "10-15", "15-20", "20-25", "25-30"))+ 
  scale_y_continuous(name= "Area (ha)")+
  theme_classic()+
  theme (text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),
                               breaks= c("geo", "esp"), 
                               values = c("grey80","grey20"))+
  geom_signif(y_position = c(38000, 12000, 8000, 6000, 5000, 5000), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2),
              annotations = c("0.03","0.53", "0.62", "0.62", "0.16", "0.05"),
              tip_length = 0, textsize = 2.5)
bp_idade

ggsave(
  "C:/Users/Lucas/OneDrive/Documentos/Mestrado/dissertacao/2art_/bp_age.png",
  plot = bp_idade,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300,
  limitsize = F,
  device = png(res = 300, units = "mm")
)

#Teste mann-whitney####
##Tabela separada por categoria de idade
area_5<- tb_vegsec2[tb_vegsec2$cat_idade=="5_anos",] 
area_10<- tb_vegsec2[tb_vegsec2$cat_idade=="10_anos",]
area_15<- tb_vegsec2[tb_vegsec2$cat_idade=="15_anos",] 
area_20<- tb_vegsec2[tb_vegsec2$cat_idade=="20_anos",] 
area_25<- tb_vegsec2[tb_vegsec2$cat_idade=="25_anos",] 
area_30<- tb_vegsec2[tb_vegsec2$cat_idade=="30_anos",] 

l_anos<- list(area_5, area_10, area_15, area_20, area_25, area_30)
l_anos
wilcox_list<- function(x){
  x<- wilcox.test(area_vegsec ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_vegsec<- lapply(l_anos, wilcox_list)
names(u_vegsec)<- paste0("?rea vegeta??o secund?ria ", seq(5,30, by=5))
u_vegsec
capture.output(u_vegsec, file = "testeU_vegsec.txt")

#?rea de vegeta??o secund?ria por ?rea desmatada 
vsidade_desmat<- ggplot(tb_vegsec2, aes(x = cat_idade, y = vegsec_desmat, fill=padrao))+ 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Secondary vegetation age",
                   labels = c("5 years", "5-10 years", "10-15 years", "15-20 years", "20-25 years", "25-30 years"))+ 
  scale_y_continuous(name= "Secondary Vegetation per deforestated area (%)")+
  theme_classic()+
  theme (text = element_text(size=12,  family="sans"))+
  scale_fill_manual("Pattern", labels= c("GEO", "FSH"),
                    breaks= c("geo", "esp"), 
                    values = c("grey80","grey20"))
vsidade_desmat

#Teste mann-whitney####
##Tabela separada por categoria de idade
area_5<- tb_vegsec2[tb_vegsec2$cat_idade=="5_anos",] 
area_10<- tb_vegsec2[tb_vegsec2$cat_idade=="10_anos",]
area_15<- tb_vegsec2[tb_vegsec2$cat_idade=="15_anos",] 
area_20<- tb_vegsec2[tb_vegsec2$cat_idade=="20_anos",] 
area_25<- tb_vegsec2[tb_vegsec2$cat_idade=="25_anos",] 
area_30<- tb_vegsec2[tb_vegsec2$cat_idade=="30_anos",] 

l_anos<- list(area_5, area_10, area_15, area_20, area_25, area_30)
l_anos
wilcox_list<- function(x){
  x<- wilcox.test(vegsec_desmat ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_vs.area<- lapply(l_anos, wilcox_list)
names(u_vs.area)<- paste0("?rea vegeta??o secund?ria ", seq(5,30, by=5))
u_vs.area
capture.output(u_vs.area, file = "testeU_vsarea.txt")

###################
#Area de vegeta??o secund?ria ao longo do tempo

#dados
vegsec<- read_xlsx("planilha_metricas.xlsx", sheet = 2)
str(vegsec)
hist(vegsec$areaha_florsec)
vegsec$padrao<- factor(vegsec$padrao)
vs.geo<- vegsec[vegsec$padrao=="geo",]
vs.esp<- vegsec[vegsec$padrao=="esp",]

#modelos
mod.geo<- glm(areaha_florsec ~ ano, data = vs.geo, family = Gamma())
par(mfrow=c(2,2))
plot(mod.geo)
summary(mod.geo)
rsquared(mod.geo)

mod.esp<- glm(areaha_florsec ~ ano, data = vs.esp, family = Gamma())
plot(mod.esp)
summary(mod.esp)
rsquared(mod.esp)

vegsec.padrao<- ggplot(data = vs.geo, aes(x=ano, y=areaha_florsec, shape=padrao))+
  geom_point(data = vs.geo, position = position_dodge(width = 2, preserve = "single"), size = 2)+
  geom_smooth(data = vs.geo, method = "glm", se=F, colour = "black")+
  geom_point(data = vs.esp, aes(x=ano, y=areaha_florsec, shape=padrao), size = 2, colour = "grey40")+
  geom_smooth(data = vs.esp, method = "glm", se=F, colour = "grey40")+
  theme_classic()+
  scale_x_continuous(name = "Year", breaks = seq(1985, 2015, by =5))+
  scale_y_continuous(name= "Secondary Vegetation area (ha)")+
  theme (text = element_text(size=12,  family="sans"))+
  scale_shape_manual("Pattern", labels= c("GEO", "FSH"),
                    breaks= c("geo", "esp"), values = c(16,17))

ggsave("vegsec.padrao.png", plot = vegsec.padrao, width = 20, height = 10, units = "cm", dpi = 300 )


#compara??o entre slopes####
acov.geo.esp<- aov(areaha_florsec ~ ano + padrao + ano*padrao, data = vegsec)
anova(acov.geo.esp)
