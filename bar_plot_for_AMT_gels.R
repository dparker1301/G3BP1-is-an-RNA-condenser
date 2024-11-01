library("ggplot2")
library("dplyr")


AMT <- read.csv("/Volumes/Parker lab data backup/G3BP1 is an RNA condenser/Figure 4 and supp 8/AMT titration gels/240810_AMT.csv")
AMT$Lane <- as.factor(AMT$Lane) 
AMT <- AMT %>%
  mutate(Lane = factor(Lane, levels=c("RNA", "Condensate", "50", "10", "5", "1", "0.5")))

single<-ggplot(AMT, aes(x=Lane, y=Frac_sing, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_sing, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
single

well<-ggplot(AMT, aes(x=Lane, y=Frac_well, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_well, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
well

smear<-ggplot(AMT, aes(x=Lane, y=Frac_smear, fill = Channel)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.1) +
  geom_jitter(aes(x=Lane, y=Frac_smear, color = Channel), position=position_jitterdodge(0.1), alpha = 1) + 
  theme_classic() +
  scale_fill_manual(values=c(alpha("magenta", 0.8), alpha("green", 0.8))) +
  scale_color_manual(values=c("black", "black"))
smear