library("ggplot2")
library("dplyr")

enzyme_treat <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 1 and supp 1/Quantification/Proteinase RNase and DNase quantification summary.csv")
enzyme_treat$Treatment <- as.factor(enzyme_treat$Treatment) 
enzyme_treat$Average_granule_intensity <- as.numeric(enzyme_treat$Average_granule_intensity)


Protein <- enzyme_treat %>% filter(Channel == "Protein")
Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "Proteinase", "Rnase", "Dnase")))


RNA <- enzyme_treat %>% filter(Channel == "RNA")
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "Proteinase", "Rnase", "Dnase")))


p<-ggplot(Protein) + 
  geom_boxplot(aes(x=Treatment, y=Average_granule_intensity, fill = Treatment), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(x=Treatment, y=Average_granule_intensity, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/25)  +
  scale_fill_manual(values=c("red", "blue", "cyan", "green")) +
  scale_color_manual(values=c("black", "black", "black", "black"))
p

r<-ggplot(RNA) + 
  geom_boxplot(aes(x=Treatment, y=Average_granule_intensity, fill = Treatment), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(x=Treatment, y=Average_granule_intensity, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/25)  +
  scale_fill_manual(values=c("red", "blue", "cyan", "green")) +
  scale_color_manual(values=c("black", "black", "black", "black"))
r


