library("ggplot2")
library("dplyr")

RNA_denat <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 3 and supp 4/Quantification/RNA_denaturants_summary.csv")
RNA_denat$Treatment <- as.factor(RNA_denat$Treatment) 
RNA_denat$Normalized_w_Control <- as.numeric(RNA_denat$Normalized_w_Control)
RNA_denat$Normalized_NPK <- as.numeric(RNA_denat$Normalized_NPK)

RNA_ratio_data <- read.csv("/Volumes/dypa2095/Quantification/RNA_denaturants/240104_RNA_and_Protein_denaturants_for_ratio_comparison_all_control_test.csv")
RNA_ratio_data$Treatment <- as.factor(RNA_ratio_data$Treatment) 
RNA_ratio_data$RNA_Protein_Ratio <- as.numeric(RNA_ratio_data$RNA_Protein_Ratio)
RNA_ratio <- RNA_ratio_data %>% filter(Channel == "Protein")
RNA_ratio <- RNA_ratio %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "25_water", "Water", "95C", "65C", "EDTA", "2M_Urea", "Hexanediol", "Salt")))

Protein <- RNA_denat %>% filter(Channel == "Protein")
Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "25_water", "Water", "95C", "65C", "EDTA", "2M_Urea", "4M_Urea", "Formamide")))

RNA <- RNA_denat %>% filter(Channel == "RNA")
RNA <- RNA %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "25_water", "Water", "95C", "65C", "EDTA", "2M_Urea", "4M_Urea", "Formamide")))

PK_RNA <- RNA %>% filter(Proteinase == "PK")

Fig2Cvars <- c("Control", "25_water", "65C", "EDTA")
Fig2C <- filter(PK_RNA, Treatment %in% Fig2Cvars)

RNA_comp <- filter(RNA, Treatment %in% Fig2Cvars)

control_plotter <- function(indata){
   
    ggplot(indata) + 
    geom_boxplot(aes(x=(Treatment), y=Normalized_w_Control, fill = Treatment), position=position_dodge(1), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Treatment), y=Normalized_w_Control, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  1), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black"))
}

no_PK_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Proteinase), y=Normalized_NPK, fill = Treatment), position=position_dodge(0.9), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Proteinase), y=Normalized_NPK, color = Treatment), size = 1, position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.9), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black", "black", "black"))
}

compare_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Treatment), y=Normalized_NPK, fill = Proteinase), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Treatment), y=Normalized_NPK, color = Proteinase), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
    theme_classic() +
    scale_fill_manual(values = c("green", "magenta")) +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black"))
}

ratio_plotter <- function(indata){
  
  ggplot(indata) + 
    geom_boxplot(aes(x=(Treatment), y=Rep_norm, fill = Treatment), position=position_dodge(0.8), outlier.shape = NA, alpha = 0.8) +
    geom_jitter(aes(x=(Treatment), y=Rep_norm, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  0.8), alpha = 0.5) + 
    theme_classic() +
    scale_color_manual(values=c("black", "black", "black", "black", "black", "black", "black", "black", "black"))
}

Fig2CPlot <- control_plotter(Fig2C)
Fig2CPlot + scale_y_continuous(breaks=seq(0, 1.2, 0.2))
Fig2CPlot + theme(aspect.ratio=25/24)

All_denat_RNA <- no_PK_plotter(RNA)
All_denat_RNA + scale_y_continuous(breaks=seq(0, 2, 0.5))
All_denat_RNA + theme(aspect.ratio=25/76)

All_denat_Protein <- no_PK_plotter(Protein)
All_denat_Protein + scale_y_continuous(breaks=seq(0, 3, 0.5))
All_denat_Protein + theme(aspect.ratio=25/76)

Compare_RNA_int <- compare_plotter(RNA_comp)
Compare_RNA_int + theme(aspect.ratio=25/50)

RNA_ratio_plot <- ratio_plotter(RNA_ratio)
RNA_ratio_plot + ylim(0,4)
