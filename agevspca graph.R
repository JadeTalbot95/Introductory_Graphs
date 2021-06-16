#Scatter Graph

library(tidyverse)
library(dplyr)
library(ggplot2)
samples <- sample_mapping_PCD01

agevspsa <- samples %>% select(`Age_at_Diagnosis`, PSA_at_Diagnosis, Gleason_Score)
agevspsa <- agevspsa[complete.cases(agevspsa), ]
agevspsa$PSA_at_Diagnosis <- as.numeric(agevspsa$PSA_at_Diagnosis)
agevspsa$Age_at_Diagnosis <- as.numeric(agevspsa$Age_at_Diagnosis)
agevspsa$Log_PSA <- log2(agevspsa$PSA_at_Diagnosis)

#plot graph agevsPSA
ggplot(agevspsa, aes(x=Age_at_Diagnosis, y=Log_PSA)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="red") +
  ggtitle("Comparison of Age and PSA at diagnosis") +
  xlab("Age at Diagnosis")+ylab("Log2 PSA at Diagnosis") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,)

agevslogPSA <- lm(Age_at_Diagnosis~Log_PSA, agevspsa)

#plot scatter graph PSA vs Gleason score

ggplot(agevspsa, aes(x=Gleason_Score, y=Log_PSA)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="red") +
  ggtitle("Comparison of Gleason Score and PSA at diagnosis") +
  xlab("Gleason Score at Diagnosis")+ylab("Log2 PSA at Diagnosis") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,)

gleasonscorevslogPSA_scatter <- lm(Gleason_Score~Log_PSA, agevspsa)

#plot boxplots PSA vs Gleason score

agevspsa$Gleason_Score <- as.factor(agevspsa$Gleason_Score)

ggplot(agevspsa, aes(x=(Gleason_Score) , y=Log_PSA)) + 
  geom_boxplot(aes(group=(Gleason_Score), color = (Gleason_Score)), lwd=1) + 
  geom_jitter(aes(group=(Gleason_Score), color = (Gleason_Score))) +
  ggtitle("Comparison of Gleason Score and PSA at diagnosis") +
  labs(y="Log2 PSA at Diagnosis", x="Gleason Score at Diagnosis") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,
        legend.position = "none",
        plot.title = element_text(hjust = 0.3, size = 10, face = "bold")) +
  geom_smooth(aes(x = as.numeric(Gleason_Score, y = Log_PSA)), method = "lm", se = FALSE, color="red", formula = y~x) +
  scale_color_manual(values=c("#8890D7", "#6470DE", "#3F4ED8", "#1B2CD5", "#0D1DAD", "#0A1686", "#040832")) +
  scale_x_discrete(name = "Gleason Score at Diagnosis", limits=c("4", "5", "6", "7", "8", "9", "10"))
  
agevspsa$Gleason_Score <- as.numeric(agevspsa$Gleason_Score)
gleasonscorevslogPSA_box <- lm(Gleason_Score~Log_PSA, agevspsa)
