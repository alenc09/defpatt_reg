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

#Data####
read_xlsx("data/idade_florsec.xlsx") -> tb_vegsec
factor(tb_vegsec$padrao, levels = c("geo","esp"), ordered = T) -> tb_vegsec$padrao
pivot_longer(data = tb_vegsec, cols = "5_anos":"30_anos", names_to = "cat_idade", values_to = "area_vegsec")%>%
  dplyr::select(paisagem, padrao, cat_idade, area_vegsec) %>%
  glimpse-> vegsec_long

read_xlsx("data/planilha_metricas.xlsx", sheet = 2) %>%
  glimpse-> metricas_vegsec

#tb_desmat<- mutate(tb_desmat, area_desmat = 250000 - areaha_florprima )
#desmat_2015<- tb_desmat$area_desmat[tb_desmat$ano == 2015]
#tb_vegsec2<- cbind(tb_vegsec2, desmat_2015)
#tb_vegsec2<- mutate(tb_vegsec2, vegsec_desmat = (area_vegsec/desmat_2015)*100)

#Boxplot category age####
##data
vegsec_long%>%
  mutate(cat_idade = factor(cat_idade,
                               levels = c("5_anos",
                                          "10_anos",
                                          "15_anos",
                                          "20_anos",
                                          "25_anos",
                                          "30_anos"),
                            ordered = T))%>%
  glimpse -> vegsec_long

ggplot(vegsec_long, aes(x = cat_idade, y = area_vegsec, fill=padrao)) + 
  geom_boxplot(colour= "black") +
  scale_x_discrete(name = "Secondary vegetation age (years)",
                   labels = c("up to 5", "5-10", "10-15", "15-20", "20-25", "25-30"))+ 
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
              tip_length = 0, textsize = 2.5) -> bp_idade

ggsave(filename = "img/bp_age.png", plot = bp_idade, dpi = 300)

#Test - mann-whitney####
##Tabela separada por categoria de idade
area_5<- vegsec_long[vegsec_long$cat_idade=="5_anos",] 
area_10<- vegsec_long[vegsec_long$cat_idade=="10_anos",]
area_15<- vegsec_long[vegsec_long$cat_idade=="15_anos",] 
area_20<- vegsec_long[vegsec_long$cat_idade=="20_anos",] 
area_25<- vegsec_long[vegsec_long$cat_idade=="25_anos",] 
area_30<- vegsec_long[vegsec_long$cat_idade=="30_anos",] 

l_anos<- list(area_5, area_10, area_15, area_20, area_25, area_30)
l_anos
wilcox_list<- function(x){
  x<- wilcox.test(area_vegsec ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_vegsec<- lapply(l_anos, wilcox_list)
names(u_vegsec)<- paste0("Área vegetação secundária ", seq(5,30, by=5))
u_vegsec
capture.output(u_vegsec, file = "testeU_vegsec.txt")

vegsec_long%>%
  group_by(padrao, cat_idade)%>%
  summarise(median = median(area_vegsec),
            range = range(area_vegsec))-> area_cat_age

###################
# secondary vegetation growth ####
##data
hist(metricas_vegsec$areaha_florsec)
metricas_vegsec$padrao<- factor(metricas_vegsec$padrao)
vs.geo<- metricas_vegsec[metricas_vegsec$padrao=="geo",]
vs.esp<- metricas_vegsec[metricas_vegsec$padrao=="esp",]

##models
mod.geo<- glm(areaha_florsec ~ ano, data = vs.geo, family = Gamma())
par(mfrow=c(2,2))
plot(mod.geo)
summary(mod.geo)
rsquared(mod.geo)

mod.esp<- glm(areaha_florsec ~ ano, data = vs.esp, family = Gamma())
plot(mod.esp)
summary(mod.esp)
rsquared(mod.esp)

glm(areaha_florsec ~ ano + padrao + ano*padrao, data = metricas_vegsec, family = inverse.gaussian()) -> mod.gen
plot(mod.gen)
summary(mod.gen)

##figures####
ggplot(data = metricas_vegsec, aes(x=ano, y=areaha_florsec, shape=padrao, group = padrao))+
  geom_point(data = vs.geo, position = position_dodge(width = 2, preserve = "single"), size = 3)+
  geom_point(data = vs.esp, aes(x=ano, y=areaha_florsec), size = 3)+
  geom_smooth(data = vs.geo, method = "glm", se=F, size = 1.5, colour = "black",aes(linetype = "geo"))+
  geom_smooth(data = vs.esp, method = "glm", se=F, size = 1.5,  colour = "black", aes(linetype = "esp"))+
  theme_classic()+
  scale_x_continuous(name = "Year", breaks = seq(1985, 2015, by =5))+
  scale_y_continuous(name= "Secondary Vegetation area (ha)")+
  scale_shape_manual(name = "Deforestation\nPattern",
                     labels= c("GEO", "FSH"),
                     breaks= c("geo", "esp"),
                     values = c(16,17))+
  scale_linetype_manual(name = "",
                        breaks = c("geo" ,"esp"),
                        labels = c("GEO", "FSH"),
                        values = c(1,4))+
  theme(text = element_text(size=12)) -> vegsec.padrao

ggsave("img/vegsec.padrao.png", plot = vegsec.padrao, dpi = 300 )


## slopes comparison####
acov.geo.esp<- aov(areaha_florsec ~ ano*padrao, data = metricas_vegsec)
anova(acov.geo.esp)
