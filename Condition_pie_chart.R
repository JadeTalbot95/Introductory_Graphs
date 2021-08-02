#Pie chart
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#load in mapping file

samples <- read.csv('../PCD01_SWATH/PCD01_general/sample_mapping_PCD01_ALL.csv')

remove.list.swath <- readLines('../PCD01_SWATH/Stoller_library/remove.list.txt')

samples <- samples[grep(remove.list.swath, samples$'Stoller_sample_ID', invert = T), ]

#Create dataframe of conditions
condition <- samples$Condition

#prints how many of each 
length(grep("Benign", condition))
length(grep("Control", condition))
length(grep("PCA", condition))

condition <- data.frame(
  group = c("Benign", "Control", "Prostate Cancer"),
  value = c(56, 168, 121),
  percentage = c("16% (56)", "49% (168)", "35% (121)")
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
    plot.title=element_text(size=10, face="bold", hjust = 0),
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

PROSPECTIVE = #F95460
  RETROSPECTIVE = #4CCA9D
  NA = #989898
  
  COLOURS LOW = #DD1C77
  INTERMEDIATE = #F2ACCA
  HIGH = #8F003B

  control = #FEB24C