#Scatter Graph

library(tidyverse)
library(dplyr)
library(ggplot2)

sample_mapping_PCD01_ALL <- read.csv('../PCD01_SWATH/PCD01_general/sample_mapping_PCD01_ALL.csv')

remove.list <- paste(paste(MS_Sample_processing_LOG_Repeats$Stoller_Sample_ID,"$", sep = ""),sep = "|", collapse = "|")

samples <- samples[grep(remove.list, samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[grep("Pre_Diagnosis", invert = T, fixed = T, samples$Collection_Type), ]

samples <- samples[grep("Control_pool|Benign_pool_|Disease_pool_|QC", samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[grep("PCD01-B*", samples$'Stoller_sample_ID', invert = T), ]

agevspsa <- samples %>% select(`Age_at_Diagnosis`, PSA_at_Diagnosis, Gleason_Score)

agevspsa$PSA_at_Diagnosis <- as.numeric(agevspsa$PSA_at_Diagnosis)
agevspsa$Age_at_Diagnosis <- as.numeric(agevspsa$Age_at_Diagnosis)
agevspsa$Gleason_Score <- as.numeric(agevspsa$Gleason_Score)

#log2 PSA
agevspsa$Log_PSA <- log2(agevspsa$PSA_at_Diagnosis)

#remove NA's
agevspsaNoNA <- agevspsa
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Age_at_Diagnosis), ]
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Log_PSA), ]

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

ggplot(agevspsaNoNA, aes(x=Age_at_Diagnosis, y=Log_PSA)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="red") +
  ggtitle("Comparison of Age and PSA at diagnosis") +
  xlab("Age at Diagnosis")+ylab("Log2 PSA at Diagnosis") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,)

agevslogPSA <- lm(Age_at_Diagnosis~Log_PSA, agevspsaNoNA)

#plot boxplots PSA vs Gleason score

#remove NA's
agevspsaNoNA <- agevspsa
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Gleason_Score), ]
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Log_PSA), ]

#plot graph
ggplot(agevspsaNoNA, aes(x=Gleason_Score, y=Log_PSA)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="red") +
  ggtitle("Comparison of Gleason Score and PSA at diagnosis") +
  xlab("Gleason Score at Diagnosis")+ylab("Log2 PSA at Diagnosis") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,)

gleasonscorevslogPSA_scatter <- lm(Gleason_Score~Log_PSA, agevspsaNoNA)

#remove NA's
agevspsaNoNA <- agevspsa
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Gleason_Score), ]
agevspsaNoNA <- agevspsaNoNA[complete.cases(agevspsaNoNA$Log_PSA), ]
agevspsaNoNA$Gleason_Score <- as.factor(agevspsaNoNA$Gleason_Score)

ggplot(agevspsaNoNA, aes(x=(Gleason_Score) , y=Log_PSA)) + 
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
  
agevspsaNoNA$Gleason_Score <- as.numeric(agevspsaNoNA$Gleason_Score)
gleasonscorevslogPSA_box <- lm(Gleason_Score~Log_PSA, agevspsaNoNA)

ggplot(agevspsaNoNA, aes( , y=PSA_at_Diagnosis)) +
  geom_boxplot() +
  ggtitle("PSA at Prostate Cancer diagnosis") +
  annotation_logticks(sides = "l") +
  labs(y="PSA concentration (ng/mL)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio=1,
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.3, size = 14, face = "bold")) +
  scale_y_log10(limits = c(1,1000))