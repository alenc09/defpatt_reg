# Mon Jul 11 10:34:47 2022 ------------------------------
#Script para criar figura da métrica CLUMPY

#library----
library(here)
library(readxl)

#data----
read_xlsx("data/planilha_metricas.xlsx", sheet = 2) -> tab_flortotal
str(tab_flortotal)
factor(tab_flortotal$padrao, levels = c("geo","esp"), ordered = T) -> tab_flortotal$padrao
factor(tab_flortotal$ano) -> tab_flortotal$ano

#analysis----
met_1985<- metricas_vegsec[metricas_vegsec$ano==1985,] 
met_1990<- metricas_vegsec[metricas_vegsec$ano==1990,]
met_1995<- metricas_vegsec[metricas_vegsec$ano==1995,]
met_2000<- metricas_vegsec[metricas_vegsec$ano==2000,]
met_2005<- metricas_vegsec[metricas_vegsec$ano==2005,]
met_2010<- metricas_vegsec[metricas_vegsec$ano==2010,] 
met_2015<- metricas_vegsec[metricas_vegsec$ano==2015,]

l_anos<- list(met_1985, met_1990, met_1995, met_2000, met_2005, met_2010, met_2015)
l_anos

wilcox_list<- function(x){
  x<- wilcox.test(CLUMPY ~ padrao, data = x, 
                  alternative= "t", exact = T)
}
u_clumpy<- lapply(l_anos, wilcox_list)
names(u_clumpy)<- paste0("CLUMPY ", seq(1985,2015, by=5))
u_clumpy
capture.output(u_clumpy, file = "testeU_clumpy.txt")

#figure----
tab_clump<- tab_flortotal[-54,]

ggplot(tab_clump, aes(x = ano, y = CLUMPY, fill=padrao)) + 
  geom_boxplot(colour= "black")+
  scale_x_discrete(name = "Year") + 
  scale_y_continuous(name= "CLUMPY")+
  theme_classic()+
  scale_fill_manual("Pattern", labels= c("GEO", "ESP"),breaks= c("geo", "esp"), values = c("grey80","grey20"))+
  geom_signif(y_position = c(0.91, 0.89, 0.88, 0.87, 0.87, 0.89, 0.88), 
              xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
              xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
              annotations = c("0.09","0.02","0.07", "0.17", "0.12", "0.03", "0.03"),
              tip_length = 0, textsize = 2.5) -> bp_clumpy

ggsave(plot = bp_clumpy, filename = "img/bp_clumpy.jpg", dpi = 600)
