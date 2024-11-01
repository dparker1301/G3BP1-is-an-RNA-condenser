library("ggplot2")
library("dplyr")


hardening <- read.csv("/Volumes/dypa2095/Quantification/No_GFP/240702_No_GFP_quant.csv")
hardening$Treatment <- as.factor(hardening$Treatment) 
hardening$Average_granule_intensity <- as.numeric(hardening$Average_granule_intensity)
hardening$Normalized <- as.numeric(hardening$Normalized)

RNA <- hardening
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("No_PK", "0_min", "15_min", "30_min", "1_hr")))

r<-ggplot(RNA) + 
  geom_boxplot(aes(x=Treatment, y=Normalized, fill = Treatment), outlier.shape = NA) +
  geom_jitter(aes(x=Treatment, y=Normalized, color = Treatment), position=position_jitterdodge(0.3), alpha = 0.5) + 
  theme_classic() + 
  theme(aspect.ratio=25/24) +
  scale_fill_manual(values=c("green", alpha("magenta", 0.25), alpha("magenta", 0.5), alpha("magenta", 0.75), alpha("magenta", 1))) +
  scale_color_manual(values=c("black", "black", "black", "black", "black"))
r


