# Wed May 04 15:55:01 2022 ------------------------------
#Scripts para análise de mudança no isolamento médio das paisagens

#Libraries####
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggpubr)

#Data----
read_excel("D:/lucas_alencar/github/defpatt_reg/data/planilha_metricas.xlsx") -> db_florprima
read_excel("D:/lucas_alencar/github/defpatt_reg/data/planilha_metricas.xlsx", sheet = 2) -> db_flortotal

#data exploration----
str_split_fixed(string = db_florprima$paisagem_ano, pattern = "_", n = 2) -> db_florprima[c("paisagem", "ano")]
str_split_fixed(string = db_flortotal$paisagem_ano, pattern = "_", n = 2) -> db_flortotal[c("paisagem", "ano")]

db_florprima %>% 
  dplyr::select(paisagem, ano, padrao, ENN_MN) %>%  
  rename(ENNMN_prima = ENN_MN) %>% 
  left_join(y=db_flortotal) %>%
  dplyr::select(paisagem, ano, padrao, ENNMN_prima, ENN_MN) %>%
  rename(ENNMN_total = ENN_MN) %>%
  mutate(across (.cols = 1:3, .fns = as.factor)) %>%
  group_by(padrao, ano) %>% 
  summarise(mean_ennmn_prima = mean(ENNMN_prima),
            mean_ennmn_total = mean(ENNMN_total),
            sd_ennmn_prima = sd(ENNMN_prima),
            sd_ennmn_total = sd(ENNMN_total)) %>%
  mutate(enn_dif = 1 - (mean_ennmn_total/mean_ennmn_prima)) %>% 
  glimpse -> tab.enn.dif
  
db_florprima %>% 
  dplyr::select(paisagem, ano, padrao, ENN_MN) %>%  
  rename(ENNMN_prima = ENN_MN) %>% 
  left_join(y=db_flortotal) %>%
  dplyr::select(paisagem, ano, padrao, ENNMN_prima, ENN_MN) %>%
  rename(ENNMN_total = ENN_MN) %>%
  mutate(across (.cols = 1:3, .fns = as.factor)) %>%
  pivot_longer(cols = 4:5, names_to = "florfac", names_prefix = "ENNMN_", values_to = "ENN_MN") %>% 
  glimpse ->tab.enn

#boxplots----
tab.enn %>% 
  filter(padrao == "geo") %>% 
ggplot() +
  geom_boxplot(aes(x = ano, y = ENN_MN, fill = florfac)) +
  ggtitle("a) Geometric Pattern") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "ENN_MN (m)") +
  scale_fill_manual(
    labels = c("Old growth", "Old growth + secondary"),
    breaks = c("prima", "total"),
    values = c("grey80", "grey20"))+
  theme_classic()+
  theme(legend.title = element_blank()) -> geo_ennmn


tab.enn %>% 
  filter(padrao == "esp") %>% 
  ggplot() +
  geom_boxplot(aes(x = ano, y = ENN_MN, fill = florfac)) +
  ggtitle("b) Fishbone Pattern") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "ENN_MN (m)") +
  scale_fill_manual(
    labels = c("Old growth", "Old growth + secondary"),
    breaks = c("prima", "total"),
    values = c("grey80", "grey20"))+
  theme_classic()+
  theme(legend.title = element_blank()) -> esp_ennmn


ggarrange(
  geo_ennmn,
  esp_ennmn,
  ncol = 2,
  nrow = 1,
  common.legend = T
) -> fig.5

ggsave(
  "D:/lucas_alencar/github/defpatt_reg/img/fig5.jpg",
  plot = fig.5, width = 7, dpi = 600)


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
