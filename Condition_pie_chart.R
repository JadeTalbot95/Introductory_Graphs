#Pie chart
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#load in mapping file

sample_mapping_PCD01_ALL <- read.csv('../PCD01_SWATH/PCD01_general/sample_mapping_PCD01_ALL.csv')

samples <- sample_mapping_PCD01_ALL %>% select('Stoller_sample_ID', Condition, "D.amico.Risk.Score", Collection_Type, 'Age_.at_collection')

remove.list <- paste(paste(MS_Sample_processing_LOG_Repeats$Stoller_Sample_ID,"$", sep = ""),sep = "|", collapse = "|")

samples <- samples[grep(remove.list, samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[grep("Pre_Diagnosis", invert = T, fixed = T, samples$Collection_Type), ]

#samples <- samples[grep("Control_pool|Benign_pool_|Disease_pool_|QC", samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[grep("PCD01-B*", samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[complete.cases(samples$Condition), ]

#Create dataframe of conditions
condition <- samples$Condition

#prints how many of each 
length(grep("Control", condition))
length(grep("PCA", condition))
length(grep("Benign", condition))
#length(grep("Post_Diagnosis", condition))
#sum(is.na(samples$Collection_Type))

condition <- data.frame(
  group = c("Benign", "Control", "Prostate Cancer"),
  value = c(50, 166, 117),
  percentage = c("15% (50)", "50% (166)", "35% (117)")
)

#visualise data
bp <- ggplot(condition, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

#blanktheme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=20, face="bold", hjust = 0.5),
    legend.title = element_blank()
  )

#pie chart
pie <- bp + 
  coord_polar("y", start=0, direction = -1) +
  blank_theme +
  ggtitle("Distribution of Conditions") +
  theme(axis.text.x=element_blank()) +
  scale_fill_manual(values=c("#694BAD", "#3F7EAA", "#F5C12C"), labels=c("Benign", "Control", "Prostate Cancer")) +
  geom_text(aes(label = percentage),position = position_stack(vjust = 0.5), fontface = "bold", color = "white", size=4)

pie

At_Diagnosis = #F95460
  Control = #FEB24C
  NA = #989898
  Post_Diagnosis = #4CCA9D
  Pre_Diagnosis = #3F7EAA
  
  COLOURS LOW = #DD1C77
  INTERMEDIATE = #F2ACCA
  HIGH = #8F003B

  control = #FEB24C