library("ggplot2")
library("dplyr")


xlink_controls <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 3 and supp 4/Quantification/Crosslinking quantification summary.csv")
xlink_controls$Treatment <- as.factor(xlink_controls$Treatment) 
xlink_controls$Normalized_w_control <- as.numeric(xlink_controls$Normalized_w_control)
xlink_controls$Normalized_NPK <- as.numeric(xlink_controls$Normalized_NPK)

Protein <- xlink_controls %>% filter(Channel == "Protein")
Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("No-Cross_control", "Cross_control", "Cross_EDTA", "No-Cross_EDTA")))

RNA <- xlink_controls %>% filter(Channel == "RNA")
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("No-Cross_control", "Cross_control", "Cross_EDTA", "No-Cross_EDTA")))

PK_RNA <- RNA %>% filter(Proteinase == "PK")

no_PK_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Proteinase), y=Normalized_NPK, fill = Treatment), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Proteinase), y=Normalized_NPK, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black", "black", "black"))
}


RNA_plot <-no_PK_plotter(RNA)
RNA_plot + scale_y_continuous(breaks=seq(0, 1.6, 0.2)) + theme(aspect.ratio=25/50)

Protein_plot <- no_PK_plotter(Protein)
Protein_plot + theme(aspect.ratio=25/50)







