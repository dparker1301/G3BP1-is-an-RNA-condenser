library("ggplot2")
library("dplyr")

sno_No_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/sno_No_PK.csv")
sno_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/sno_PK.csv")
ets_No_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/ets_No_PK.csv")
ets_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/ets_PK.csv")
npm_No_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/npm_No_PK.csv")
npm_PK <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 6 and supp 10/Quantification/npm_PK.csv")

sno_plot <- ggplot() +
  geom_line(data = sno_PK, aes(x=Distance, y=Average), color = "magenta") +
  geom_ribbon(data = sno_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "magenta", alpha = 0.2) +
  geom_line(data = sno_No_PK, aes(x = Distance, y = Average), color = "green") +
  geom_ribbon(data = sno_No_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "green", alpha = 0.2) +
  theme_classic()
sno_plot


ets_plot <- ggplot() +
  geom_line(data = ets_PK, aes(x=Distance, y=Average), color = "magenta") +
  geom_ribbon(data = ets_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "magenta", alpha = 0.2) +
  geom_line(data = ets_No_PK, aes(x = Distance, y = Average), color = "green") +
  geom_ribbon(data = ets_No_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "green", alpha = 0.2) +
  theme_classic()

ets_plot

npm_plot <- ggplot() +
  geom_line(data = npm_PK, aes(x=Distance, y=Average), color = "magenta") +
  geom_ribbon(data = npm_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "magenta", alpha = 0.2) +
  geom_line(data = npm_No_PK, aes(x = Distance, y = Average), color = "green") +
  geom_ribbon(data = npm_No_PK, aes(x = Distance, ymin = Low_conf, ymax = High_conf), fill = "green", alpha = 0.2) +
  theme_classic()
npm_plot
