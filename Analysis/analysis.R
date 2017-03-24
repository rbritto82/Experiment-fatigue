#Load data. Use just if the input data was already prepared using prepare_data.R script. Otherwise, comment it.
#tidyData <- tbl_df(read.xlsx("./Data/Tidy_Results_2017-01-17.xlsx", sheetIndex=1, stringsAsFactors = FALSE))

#results and fatigue-----------------------------------------------------------------------------------------
#printing the values grouped by fatigue and selection result
selectionBarplot <- ggplot(tidyData, aes(x = fatigue)) + 
  geom_bar(aes(fill = result), position = "dodge") + 
scale_y_discrete(name = "Correct selection") + 
  scale_x_discrete(name = "Fatigue level") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Data/Selection_Barplot_without_zeros.pdf", selectionBarplot)

tab1 <- table(tidyData$fatigue, tidyData$result)
comparison <- epi.2by2(tab1, method = "cohort.count", conf.level = 0.9)
write.xlsx(tab1, file = paste("./Data/Analysis_", Sys.Date(), ".xlsx", sep = ""), sheetName="Table")

#confidence and fatigue-----------------------------------------------------------------------------------------

confidenceBarplot <- ggplot(tidyData, aes(x = fatigue)) + 
  geom_bar(aes(fill = confidence), position = "dodge") +
  scale_y_discrete(name = "Selection confidence") + 
  scale_x_discrete(name = "Fatigue level") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Data/Confidence_Barplot_without_zeros.pdf", confidenceBarplot)

tab2 <- table(tidyData$confidence, tidyData$fatigue)
wilcox <- wilcox.test(as.numeric(confidence) ~ fatigue, data = tidyData, conf.level = 0.90, alternative = "two.sided")

write.xlsx(tab2, file = paste("./Data/Analysis_", Sys.Date(), ".xlsx", sep = ""), sheetName="Table confidence", append = TRUE)
write.xlsx(wilcox$p.value, file = paste("./Data/Analysis_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcox confindence p-value", append = TRUE)

#per person and fatigue-----------------------------------------------------------------------------------------

perPersonBoxplot <- ggplot(perPersonData, aes(x = fatigue, y =ratio)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Ratio Number wrong Selections / Total Number of Selected Studies") + 
  scale_x_discrete(name = "Fatigue level") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Data/Per_Person_Boxplot.pdf", perPersonBoxplot)

normality <- shapiro.test(perPersonData$ratio)

#tab2 <- table(tidyData$confidence, tidyData$fatigue)
wilcox <- wilcox.test(ratio ~ fatigue, data = perPersonData, conf.level = 0.90, alternative = "two.sided")

write.xlsx(wilcox$p.value, file = paste("./Data/Analysis_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcox Per Person p-value", append = TRUE)
write.xlsx(normality$p.value, file = paste("./Data/Analysis_", Sys.Date(), ".xlsx", sep = ""), sheetName="Normality Per Person p-value", append = TRUE)