#boxplot of PSA values

library(reshape2)
library(ggplot2)
library(scales)

#make data frame
samples <- sample_mapping_PCD01
PSA = samples %>% select(PSA_at_Diagnosis)
PSA <- as.numeric(PSA$PSA_at_Diagnosis)
PSA <- as.data.frame(PSA)
PSA <- PSA %>% drop_na()
colnames(PSA)[1] <- "value"
PSA$variable <- "PCA"


PSAplot <- ggplot(PSA, aes(variable, value)) + 
  geom_boxplot(binaxis = 'y', stackdir = 'center') +
  coord_trans( , y="log10") +
  ggtitle("PSA at Diagnosis") +
  xlab("")+ylab("PSA at Diagnosis (ng/mL)") +
  geom_point() +
  theme(axis.title.x =element_blank(),
        axis.text.x =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

PSAplot <- PSAplot + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                  labels = comma,
                                  #labels = trans_format("log10", math_format(10^.x)),
                                  limits = y_lims(0, 600))