library("ggplot2")
library("dplyr")

Protein_denat <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Supp 5/Quantification/protein_denaturant_summary.csv")
Protein_denat$Treatment <- as.factor(Protein_denat$Treatment) 
Protein_denat$Normalized_w_control <- as.numeric(Protein_denat$Normalized_w_control)
Protein_denat$Normalized_NPK <- as.numeric(Protein_denat$Normalized_NPK)

Protein <- Protein_denat %>% filter(Channel == "Protein")
Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "Hexanediol", "Salt")))

RNA <- Protein_denat %>% filter(Channel == "RNA")
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "Hexanediol", "Salt")))

PK_RNA <- RNA %>% filter(Proteinase == "PK")

control_plotter <- function(indata){
   
    ggplot(indata) + 
    geom_boxplot(aes(x=(Treatment), y=Normalized_w_control, fill = Treatment), position=position_dodge(1), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Treatment), y=Normalized_w_control, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  1), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black"))
}

no_PK_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Proteinase), y=Normalized_NPK, fill = Treatment), position=position_dodge(1), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Proteinase), y=Normalized_NPK, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  1), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black"))
}

compare_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Treatment), y=Normalized_NPK, fill = Proteinase), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Treatment), y=Normalized_NPK, color = Proteinase), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
    theme_classic() +
    scale_fill_manual(values = c("green", "magenta")) +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black"))
}

All_denat_RNA <- no_PK_plotter(RNA)
All_denat_RNA + scale_y_continuous(breaks=seq(0, 2, 0.5))
All_denat_RNA + theme(aspect.ratio=25/76)

All_denat_Protein <- no_PK_plotter(Protein)
All_denat_Protein + scale_y_continuous(breaks=seq(0, 3, 0.5))
All_denat_Protein + theme(aspect.ratio=25/76)

Compare_RNA_int <- compare_plotter(RNA)
Compare_RNA_int + theme(aspect.ratio=25/40)
