setwd ("C:/Users/Lucas Alencar/OneDrive/Documentos/Mestrado/INPE/R/parte_3")
vegsec <-
  read_excel("C:/Users/Lucas Alencar/OneDrive/Documentos/Mestrado/INPE/area_vegsec.xlsx")

mod_slope <- aov(areaha_florsec ~ ano * padrao, data = vegsec)
summary(mod_slope)
mod_inter <- aov(areaha_florsec ~ ano + padrao, data = vegsec)
summary(mod_inter)
capture.output(mod_slope, file = "mod_slope.txt")
capture.output(mod_inter, file = "mod_inter.txt")

#Libraries####
library(readxl)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(here)

#Data####
read_excel(here("data/ennmn_geo.xlsx"))-> ennmn_geo
ennmn_geo$florfac <- as.factor(ennmn_geo$florfac)
ennmn_geo$ano <- as.factor(ennmn_geo$ano)

geo_ennmn <-
  ggplot(ennmn_geo, aes(x = ano, y = ENN_MN, fill = florfac)) +
  geom_boxplot(colour = "black") +
  ggtitle("a) Geometric Pattern") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "ENN_MN (m)") +
  theme_classic() +
  scale_fill_manual(
    "Pattern",
    labels = c("Old growth", "Old growth + secondary"),
    breaks = c("prima", "sec"),
    values = c("grey80", "grey20")
  ) +
  geom_signif(
    y_position = c(510, 550, 360, 350, 360, 320, 320),
    xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
    xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
    annotations = c("0.01", "0.02", "0.01", "0.01", "0.01", "1.00", "0.31"),
    tip_length = 0,
    textsize = 2.5
  )
geo_ennmn


read_excel(here("data/ennmn_esp.xlsx")) -> ennmn_esp
ennmn_esp$florfac <- as.factor(ennmn_esp$florfac)
ennmn_esp$ano <- as.factor(ennmn_esp$ano)

esp_ennmn <-
  ggplot(ennmn_esp, aes(x = ano, y = enn_mn, fill = florfac)) +
  geom_boxplot(colour = "black") +
  ggtitle("b) Fishbone Pattern") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "ENN_MN (m)") +
  theme_classic() +
  scale_fill_manual(
    "Pattern",
    labels = c("Old growth", "Old growth + secondary"),
    breaks = c("prima", "sec"),
    values = c("grey80", "grey20")
  ) +
  geom_signif(
    y_position = c(150, 160, 170, 200, 210, 230, 230),
    xmin = c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8, 6.8),
    xmax = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2),
    annotations = c("0.07", "0.03", "0.01", "0.12", "0.16", "0.05", "0.03"),
    tip_length = 0,
    textsize = 2.5
  )
esp_ennmn

enn_mn <- ggarrange(
  geo_ennmn,
  esp_ennmn,
  ncol = 2,
  nrow = 1,
  common.legend = T
)

ggsave(
  "img/bp_ennmn.png",
  plot = enn_mn,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300,
  limitsize = F,
  device = png(res = 300, units = "mm")
)


metsecgeo_1985 <- ennmn_geo[ennmn_geo$ano == 1985, ]
metsecgeo_1990 <- ennmn_geo[ennmn_geo$ano == 1990, ]
metsecgeo_1995 <- ennmn_geo[ennmn_geo$ano == 1995, ]
metsecgeo_2000 <- ennmn_geo[ennmn_geo$ano == 2000, ]
metsecgeo_2005 <- ennmn_geo[ennmn_geo$ano == 2005, ]
metsecgeo_2010 <- ennmn_geo[ennmn_geo$ano == 2010, ]
metsecgeo_2015 <- ennmn_geo[ennmn_geo$ano == 2015, ]
l_metsec_geo <-
  list(
    metsecgeo_1985,
    metsecgeo_1990,
    metsecgeo_1995,
    metsecgeo_2000,
    metsecgeo_2005,
    metsecgeo_2010,
    metsecgeo_2015
  )

wilcox_list <- function(x) {
  x <- wilcox.test(
    ENN_MN ~ florfac,
    data = x,
    alternative = "t",
    exact = T
  )
}
u_ennmn_geo <- lapply(l_metsec_geo, wilcox_list)
names(u_ennmn_geo) <- paste0("ENN_MN ", seq(1985, 2015, by = 5))
u_ennmn_geo
capture.output(u_ennmn_geo, file = "testeU_ennmn_geo.txt")


metsecesp_1985 <- ennmn_esp[ennmn_esp$ano == 1985, ]
metsecesp_1990 <- ennmn_esp[ennmn_esp$ano == 1990, ]
metsecesp_1995 <- ennmn_esp[ennmn_esp$ano == 1995, ]
metsecesp_2000 <- ennmn_esp[ennmn_esp$ano == 2000, ]
metsecesp_2005 <- ennmn_esp[ennmn_esp$ano == 2005, ]
metsecesp_2010 <- ennmn_esp[ennmn_esp$ano == 2010, ]
metsecesp_2015 <- ennmn_esp[ennmn_esp$ano == 2015, ]
l_metsec_esp <-
  list(
    metsecesp_1985,
    metsecesp_1990,
    metsecesp_1995,
    metsecesp_2000,
    metsecesp_2005,
    metsecesp_2010,
    metsecesp_2015
  )
wilcox_list <- function(x) {
  x <- wilcox.test(
    enn_mn ~ florfac,
    data = x,
    alternative = "t",
    exact = T
  )
}
u_ennmn_esp <- lapply(l_metsec_esp, wilcox_list)
names(u_ennmn_esp) <- paste0("ENN_MN ", seq(1985, 2015, by = 5))
u_ennmn_esp
capture.output(u_ennmn_esp, file = "testeU_ennmn_esp.txt")
