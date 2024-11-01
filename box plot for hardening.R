library("ggplot2")
library("dplyr")


hardening <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 2/Quantification/240102 final hardening quant.csv")
hardening$Treatment <- as.factor(hardening$Treatment) 
hardening$Average_granule_intensity <- as.numeric(hardening$Average_granule_intensity)
hardening$Normalized <- as.numeric(hardening$Normalized)

Protein <- hardening %>% filter(Channel == "Protein")
Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("No_PK", "0_min", "15_min", "30_min", "1_hr")))

RNA <- hardening %>% filter(Channel == "RNA")
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("No_PK", "0_min", "15_min", "30_min", "1_hr")))


p<-ggplot(Protein) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) +
  scale_fill_manual(values=c("green", alpha("magenta", 0.25), alpha("magenta", 0.5), alpha("magenta", 0.75), alpha("magenta", 1))) +
  scale_color_manual(values=c("black", "black", "black", "black", "black"))
p

r<-ggplot(RNA) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) +
  scale_fill_manual(values=c("green", alpha("magenta", 0.25), alpha("magenta", 0.5), alpha("magenta", 0.75), alpha("magenta", 1))) +
  scale_color_manual(values=c("black", "black", "black", "black", "black"))
r


