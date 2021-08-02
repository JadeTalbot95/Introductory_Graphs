#box plot of age at collection

#box plot of age at collection

library(ggplot2)

#make data frame

samples <- read.csv('../PCD01_SWATH/PCD01_general/sample_mapping_PCD01_ALL.csv')

remove.list.swath <- readLines('../PCD01_SWATH/Stoller_library/remove.list.txt')

samples <- samples[grep(remove.list.swath, samples$'Stoller_sample_ID', invert = T), ]

samples <- samples[grep("Control_pool|Benign_pool_|Disease_pool_|QC", samples$'Stoller_sample_ID', invert = T), ]

age <- samples %>% select(Condition, Age_.at_collection)
age <- age[complete.cases(age$Condition),]

#change column headings
colnames(age)[1] <- "variable"
colnames(age)[2] <- "value"
age$value <- as.numeric(age$value)

ggplot(age, aes(x=variable , y=value, fill=variable)) + 
  geom_boxplot(aes(fill=variable)) + 
  ggtitle("Comparison of age in each condition") +
  xlab("")+ylab("Age at Collection") +
  theme(axis.title.x =element_blank(),
        axis.text.x =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
		aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
scale_fill_manual(values=c("#694BAD", "#3F7EAA", "#F5C12C"), labels=c("Benign", "Control", "Prostate Cancer")) +
labs(fill = "Condition")