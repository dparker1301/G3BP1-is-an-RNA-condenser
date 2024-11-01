library("ggplot2")
library("dplyr")

gran_dat <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 1 and supp 1/Quantification/G3BP1 titration summary.csv")
gran_dat$G3BP1_conc <- as.factor(gran_dat$G3BP1_conc) 
gran_dat$Treatment <- as.factor(gran_dat$Treatment) 

Protein <- gran_dat %>% filter(Channel == "Protein")
RNA <- gran_dat %>% filter(Channel == "RNA")


Protein <- Protein %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "Proteinase", "Pretreat")))

RNA$G3BP1_conc <- as.factor(RNA$G3BP1_conc) 
RNA$Treatment <- as.factor(RNA$Treatment) 

RNA <- RNA %>%
mutate(Treatment = factor(Treatment, levels=c("Control", "Proteinase", "Pretreat")))

p<-ggplot(Protein) + 
  geom_boxplot(aes(x=G3BP1_conc, y=Normalized, fill = Treatment), position=position_dodge(1), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(x=G3BP1_conc, y=Normalized, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  1), alpha = 0.5) + 
  theme_classic() + 
  scale_fill_manual(values=c("green", "magenta")) +
  scale_color_manual(values=c("black", "black"))
p

r<-ggplot(RNA) + 
  geom_boxplot(aes(x=G3BP1_conc, y=Normalized, fill = Treatment), position=position_dodge(1), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(x=G3BP1_conc, y=Normalized, color = Treatment), position=position_jitterdodge(jitter.width=0.2, dodge.width =  1), alpha = 0.5) + 
  theme_classic() + 
  scale_fill_manual(values=c("green", "magenta")) +
  scale_color_manual(values=c("black", "black"))
r


