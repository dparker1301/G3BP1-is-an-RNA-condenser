library("ggplot2")
library("dplyr")


MgTitrate <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Supp 4/MG_quant/240720_Mg_titration.csv")
MgTitrate$Treatment <- as.factor(MgTitrate$Treatment) 
MgTitrate$Average_granule_intensity <- as.numeric(MgTitrate$Average_granule_intensity)
MgTitrate$Normalized <- as.numeric(MgTitrate$Normalized)

Luc <- MgTitrate %>% filter(Channel == "RNA")
Luc <- Luc %>%
  mutate(Treatment = factor(Treatment, levels=c("0_min", "60_min", "24_hr", "No_PK")))

G3BP <- MgTitrate %>% filter(Channel == "Protein")
G3BP <- G3BP %>%
  mutate(Treatment = factor(Treatment, levels=c("0_min", "60_min", "24_hr", "No_PK")))
G3BP$Normalized <- as.numeric(G3BP$Normalized)

One_mM <- Luc %>% filter(Concentration == "1_mM")
two_point_five_mM <- Luc %>% filter(Concentration == "2-5_mM")
five_mM <- Luc %>% filter(Concentration == "5_mM")
ten_mM <- Luc %>% filter(Concentration == "10_mM")

r<-ggplot(One_mM) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) #+
r

p<-ggplot(two_point_five_mM) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) #+
p

q<-ggplot(five_mM) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) #+
q

s<-ggplot(ten_mM) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) #+
s

