library("ggplot2")
library("dplyr")


gel <- read.csv("/Volumes/dypa2095/Quantification/Denaturing_gel_quant/PEG/240810_All_PEG.csv")
gel$Lane <- as.factor(gel$Lane) 
gel <- gel %>%
  mutate(Lane = factor(Lane, levels=c("RNA_no-X", "RNA_X", "PEG_no-X", "PEG_X")))


total<-ggplot(gel, aes(x=Lane, y=Total, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  geom_jitter(aes(x=Lane, y=Total, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
total

single<-ggplot(gel, aes(x=Lane, y=Frac_sing, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_sing, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
single

well<-ggplot(gel, aes(x=Lane, y=Frac_well, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_well, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
well

smear<-ggplot(gel, aes(x=Lane, y=Frac_smear, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_smear, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
smear
