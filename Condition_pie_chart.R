#Pie chart
  

#library(tidyverse)
#library(dbplyr)
#library(ggplot2)
#library(scales)

samples <- sample_mapping_PCD01

#Create dataframe
two_conditions <- samples %>% select("D'amico Risk Score", Collection_Type, Condition, Stoller_sample_ID)
#two_conditions <- two_conditions[ grep("PCA", two_conditions$Condition, invert = FALSE) , ]
#two_conditions <- two_conditions[ grep("Post_Diagnosis", two_conditions$Collection_Type, invert = FALSE) , ]

length(grep("Control", two_conditions$Collection_Type))
length(grep("At_Diagnosis", two_conditions$Collection_Type))
length(grep("Post_Diagnosis", two_conditions$Collection_Type))

condition <- data.frame(
  group = c("Control", "At_Diagnois", "Post_Diagnosis", "NA"),
  value = c(164, 114, 27, 38),
  percentage = c("48% (164)", "33% (114)", "8% (27)", "11% (38)")
)

#visualise data
bp <- ggplot(condition, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", color = "black")
bp

#blanktheme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=18, face="bold", hjust = 0.5),
    legend.title = element_blank()
  )

#pie chart
pie <- bp + 
  coord_polar("y", start=0, direction = -1) +
  blank_theme +
  ggtitle("Point of Collection") +
  theme(axis.text.x=element_blank()) +
  scale_fill_manual(values=c("#4CCA9D", "#FEB24C", "#989898", "#F95460")) +
  geom_text(aes(label = percentage),position = position_stack(vjust = 0.5), fontface = "bold", color = "white", size=4)

pie

PROSPECTIVE = #F95460
  RETROSPECTIVE = #4CCA9D
  NA = #989898
  
  COLOURS LOW = #DD1C77
  INTERMEDIATE = #F2ACCA
  HIGH = #8F003B

  control = #FEB24C