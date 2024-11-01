library("ggplot2")
library("dplyr")


pellet <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 1 and supp 1/MEG-3 pelleting/Pelleting_analysis.csv")
pellet$PK <- as.factor(pellet$PK) 
pellet <- pellet %>%
  mutate(PK = factor(PK, levels=c("RNA", "No", "Yes")))


frac_pel<-ggplot(pellet, aes(x=PK, y=Fraction_pellet, fill = PK)) + 
  geom_bar(stat = "summary", fun = "mean") +
  geom_errorbar( aes(x=PK, ymin=Pellet_Average-Pellet_SD, ymax=Pellet_Average+Pellet_SD), width=0.1, colour="black", alpha=0.5, size=0.5) +
  geom_jitter(aes(x=PK, y=Fraction_pellet, color = PK), position=position_jitterdodge(0.3), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("cyan", 0.8), alpha("yellow", 0.8), alpha("magenta", 0.8))) +
  scale_color_manual(values=c("black", "black", "black"))
frac_pel