#Data import
library(readxl)
datos <- read_excel("data_field.xlsx")
RadarAS <- read_excel("Radar_AS.xlsx")
RadarASgreenhouse <- read_excel("Radar_AS_greenhouse.xlsx")
RadarUrea <- read_excel("Radar_U.xlsx")
RadarUreaGreenhouse <- read_excel("Radar_U_greenhouse.xlsx") 
RadarNonfert <- read_excel("Radar_nonfert.xlsx")
RadarNonfertGreen <- read_excel("Radar_nonfert_greenhouse.xlsx")
Oxalic <- read_excel("Oxalic.xlsx")

attach(datos)
attach(RadarAS)
attach(RadarASgreenhouse)
attach(RadarUrea)
attach(RadarUreaGreenhouse)
attach(RadarNonfert)
attach(RadarNonfertGreen)
attach(Oxalic)

#Variable transformation
datos$Hybrid =as.factor(datos$Hybrid)
datos$Treatment =as.factor(datos$Treatment)
datos$Condition =as.factor(datos$Condition)
datos$Condition = factor(datos$Condition, levels =c("Control soil", "ASS soil"))
datos$Treatment = factor(datos$Treatment, levels =c("Non-fert", "U","AS"))

#Loading library
library(ggpubr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(RColorBrewer)
library(viridis)
library(ggsci)
install.packages("fastmap")
devtools::install_version("fastmap", version = "1.2.0", repos = "http://cran.us.r-project.org")
install.packages("backports")

#Plotting Figures with statistics
my_comparisons <- list( c("Non-fert", "AS"), c("Non-fert", "U"), c("U", "AS") )
Figure_1a <- datos
Figure_1a %>%  
  ggplot() +
  aes(x=Treatment, y=soil_pH, col=Condition)+
  geom_boxplot() + labs(x = "Treatments", y = "Soil pH")+  facet_grid(Condition~Hybrid)+scale_color_manual(values = c("dark grey", "black")) + theme_bw()+  scale_y_continuous(limits = c(6, 10))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = 'wilcox.test', ref.group = "Non-fert", size = 2.5)

Figure_2a <- datos
Figure_2a %>%  
  ggplot() +
  aes(x=Treatment, y=DW, col=Condition)+
  geom_boxplot() + labs(x = "Treatments", y = "Shoot dry weight g/plant")+  facet_grid(Condition~Hybrid)+scale_color_manual(values = c("dark grey", "black")) + theme_bw()+  scale_y_continuous(limits = c(0, 100))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = 'wilcox.test', ref.group = "Non-fert", size = 2.5)


Figure_3 <- datos
Figure_3 %>%  
  ggplot() +
  aes(x=Treatment, y=SPAD, col=Condition)+
  geom_boxplot() + labs(x = "Treatments", y = "SPAD index a.u.")+  facet_grid(Condition~Hybrid)+scale_color_manual(values = c("dark grey", "black")) + theme_bw()+  scale_y_continuous(limits = c(0, 60))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = 'wilcox.test', ref.group = "Non-fert", size = 2.5)

#For plotting the radar plots(Figure 4)
library(ggradar)
library(tidyverse)
library(scales)
labels <- c("155","166","SIP", "Control")

lcols <- c("black", "lightgray", "darkgray","#00AFBB")
colors <- c("#00AFBB", "#E7B800", "#FC4E07")

Radar_nonfert$Hybrids <- factor(Radar_nonfert$Hybrids, levels = c("155","166","SIP", "Control"))

ggradar(RadarAS,
        #axis.labels = colnames(Radar_nonfert)[-1],
        values.radar = c("0", "1","2"),
        base.size = 4,
        group.line.width = 1.2,
        group.point.size = 1,
        grid.min = 0,
        grid.mid = 1,
        grid.max = 2,
        axis.label.size = 4,
        grid.label.size = 4,
        legend.position = "bottom",
        plot.title = "AS",
        group.colours = lcols,
        legend.text.size = 10,
        grid.line.width = 0.5
)

#Plotting figure 5 (oxalic acid)

attach(Oxalic)
Oxalic$Hybrid =as.factor(Oxalic$Hybrid)
Oxalic$Treatment =as.factor(Oxalic$Treatment)
Oxalic$Condition =as.factor(Oxalic$Condition)
Oxalic$Condition = factor(Oxalic$Condition, levels =c("Control soil", "ASS soil"))
Oxalic$Treatment = factor(Oxalic$Treatment, levels =c("Non-fert", "U","AS"))

symnum.args <- list(
  cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 2), 
  symbols = c("xxxx", "***", "**", "*", "ns")
)

my_comparisons <- list( c("Non-fert", "AS"), c("Non-fert", "U"), c("U", "AS") )
Figure5 <- Oxalic
Oxalic %>%  
  ggplot() +
  aes(x=Treatment, y=OA_Final, col=Condition)+
  geom_boxplot() + labs(x = "Treatments", y = "Oxalic acid (mg L-1 soil solution)")+  facet_grid(Condition~Hybrid)+scale_color_manual(values = c("dark grey", "black")) + theme_bw()+  scale_y_continuous(limits = NULL)+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = 'wilcox.test', ref.group = "Non-fert", size = 2.5, symnum.args = symnum.args)

