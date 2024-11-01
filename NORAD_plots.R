library("ggplot2")

NORAD_spots <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 5/Quantification/NORAD_quant.csv")
NORAD_spots$Treatment <- as.factor(NORAD_spots$Treatment) 
  
Total_plot <-  ggplot(NORAD_spots) + 
    geom_boxplot(aes(x=(Proteinase), y=Total, fill = Proteinase), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Proteinase), y=Total, color = Proteinase), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black")) +
    scale_fill_manual(values=c("green", "magenta")) +
    ylim (0,350)

Total_plot

Granule_plot <-  ggplot(NORAD_spots) + 
  geom_boxplot(aes(x=(Proteinase), y=Fraction, fill = Proteinase), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(x=(Proteinase), y=Fraction, color = Proteinase), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
  theme_classic() +
  scale_color_manual(values=c("black", "black")) +
  scale_fill_manual(values=c("green", "magenta")) +
  ylim (0,1)

Granule_plot
