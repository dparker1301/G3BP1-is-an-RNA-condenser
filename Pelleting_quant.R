library("ggplot2")
library("dplyr")


pellet <- read.csv("/Volumes/dypa2095/Quantification/G3BP_pelleting/240611/240725_Pelleting_analysis.csv")
pellet$PK <- as.factor(pellet$PK) 
pellet <- pellet %>%
  mutate(PK = factor(PK, levels=c("RNA", "No", "Yes")))

frac_pel<-ggplot(pellet, aes(x=PK, y=Fraction_pellet, fill = PK)) + 
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=PK, y=Fraction_pellet, color = PK), position=position_jitterdodge(0.3), alpha = 1) + 
  theme_classic() + 
  theme(aspect.ratio=25/25) +
  scale_fill_manual(values=c(alpha("cyan", 0.5), alpha("yellow", 0.5), alpha("magenta", 0.5))) +
  scale_color_manual(values=c("black", "black", "black"))
frac_pel
