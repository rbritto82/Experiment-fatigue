#collecting the index of each variable
indexFatigue <- grep("^Are.you.feeling.mentally.fatigued.now.", names(rawData))
indexConfidence <- grep("^How.confident.are.you.about.your.decision.", names(rawData))
indexDecision <- grep("^What.is.your.decision.about.this.study.", names(rawData))

#remorting the data
decision <- vector()
confidence <- vector()
fatigue <- vector()
for (i in 1:dim(rawData)[1]) {
  temp1 <- vector()
  temp2 <- vector()
  temp3 <- vector()
  for (j in 1:dim(benchmark)[1]) {
    temp1 <- c(temp1, rawData[[indexDecision[j]]][i])
    temp2 <- c(temp2, rawData[[indexConfidence[j]]][i])
    if(j < 11){
      temp3 <- c(temp3, rawData[[2]][i])
    } else if(j >= 11 & j < 21){
      temp3 <- c(temp3, rawData[[indexFatigue[1]]][i])
    } else if(j >= 21 & j < 31){
      temp3 <- c(temp3, rawData[[indexFatigue[2]]][i])
    } else if(j >= 31 & j < 41){
      temp3 <- c(temp3, rawData[[indexFatigue[3]]][i])
    } else if(j >= 41 & j < 51){
      temp3 <- c(temp3, rawData[[indexFatigue[4]]][i])
    } else if(j >= 51 & j < 61){
      temp3 <- c(temp3, rawData[[indexFatigue[5]]][i])
    } else if(j >= 61 & j < 71){
      temp3 <- c(temp3, rawData[[indexFatigue[6]]][i])
    } else if(j >= 71 & j < 81){
      temp3 <- c(temp3, rawData[[indexFatigue[7]]][i])
    } else if(j >= 81 & j < 91){
      temp3 <- c(temp3, rawData[[indexFatigue[8]]][i])
    } else if(j >= 91){
      temp3 <- c(temp3, rawData[[indexFatigue[9]]][i])
    }
  }  
  
  decision <- c(decision, temp1)
  confidence <- c(confidence, temp2)
  fatigue <- c(fatigue, temp3)
}   
tidyData[4] <- decision
tidyData[5] <- confidence
tidyData[6] <- fatigue

   

#naming the new columns of the new data frame
names(tidyData)[4] <- "decision"
names(tidyData)[5] <- "confidence"
names(tidyData)[6] <- "fatigue"

#changing the values of the benchmark variable to facilitate data analysis
tidyData$benchmark <- gsub("No", "Exclusion", tidyData$benchmark)
tidyData$benchmark <- gsub("Yes", "Inclusion", tidyData$benchmark)

#changing the values of the confidence variable to facilitate data analysis
tidyData$confidence <- gsub("Highly confident", "High", tidyData$confidence)
tidyData$confidence <- gsub("Moderately confident", "Medium", tidyData$confidence)
tidyData$confidence <- gsub("Somewhat confident", "Low", tidyData$confidence)

#changing the values of the fatigue variable to facilitate data analysis
tidyData$fatigue <- gsub("I can continue this experiment without any problem. My ability for sustained mental effort is not reduced|I am a bit fatigued, but am still able to make the required mental effort without any break", "Non-fatigued", tidyData$fatigue)
tidyData$fatigue <- gsub("I am fatigued, and need to take a short break before moving on|I am highly fatigued and can not continue with this experiment today", "Fatigued", tidyData$fatigue)

#comparing benchmark with results
tidyData$result <- tidyData$benchmark == tidyData$decision

#transforming the variables in factors
tidyData$confidence <- as.factor(tidyData$confidence)
tidyData$fatigue <- as.factor(tidyData$fatigue)
tidyData$result <- as.factor(tidyData$result)

write.xlsx(tidyData, file = paste("./Data/Tidy_Results_", Sys.Date(), ".xlsx", sep = ""), sheetName="Results")

#number of mistakes and hits per person
perPersonData <- data.frame(name= character(), fatigue = character(), ratio = vector(), stringsAsFactors=FALSE)

for(i in 1:nrow(rawData)){
  fatigueRight <- nrow(tidyData[(tidyData$name == rawData$What.is.your.full.name.[i] & tidyData$fatigue == "Fatigued" & tidyData$result == TRUE),])
  fatigueWrong <- nrow(tidyData[(tidyData$name == rawData$What.is.your.full.name.[i] & tidyData$fatigue == "Fatigued" & tidyData$result == FALSE),])
  nonFatigueRight <- nrow(tidyData[(tidyData$name == rawData$What.is.your.full.name.[i] & tidyData$fatigue == "Non-fatigued" & tidyData$result == TRUE),])
  nonFatigueWrong <- nrow(tidyData[(tidyData$name == rawData$What.is.your.full.name.[i] & tidyData$fatigue == "Non-fatigued" & tidyData$result == FALSE),])
  
  perPersonData <- rbind(perPersonData, data.frame(name = rawData$What.is.your.full.name.[i], fatigue="Fatigued", ratio=fatigueWrong/(fatigueRight+fatigueWrong)))
  perPersonData <- rbind(perPersonData, data.frame(name = rawData$What.is.your.full.name.[i], fatigue="Non-fatigued", ratio=nonFatigueWrong/(nonFatigueRight+nonFatigueWrong)))
}

#perPersonData$ratio[is.na(perPersonData$ratio)] <- 0

write.xlsx(perPersonData, file = paste("./Data/Per_Person_", Sys.Date(), ".xlsx", sep = ""), sheetName="Results")
