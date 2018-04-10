knitr::opts_chunk$set(echo = TRUE)

#Color Format
colFmt = function(x,color){
  outputFormat = opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex')
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(outputFormat == 'html')
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}


#remove scientific notation 
options(scipen=999)

#load package libraries 

library(dplyr)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(gplots)
library(e1071)
library(class)
library(data.table)
library(caret)
library(mosaic)
#symbol %in% is an alias meaning is_in  

#%>% is a pipe lefthand operator, makes code cleaner. here left hand values of code forward into expressions that appear on the right hand side, this is an integral part of the dplyr package and will appear often in all my code 

'%!in%' <- function(x,y)!('%in%'(x,y))

#load in 2014 data set
#here we add the retention charactersitics we are intrested in 

transfer1149<- read.csv(file = file.choose()) %>% filter(CURRENT_ADMIT_TYPE_CODE != ".") %>% filter(CURRENT_ADMIT_TERM_CODE %in% c(1149,1151,1153,1156,1159,1161,1163,1166,1169,1171,1173,1176,1179))

#orientation 2014 data pull 
orient1149 <- read.csv(file = file.choose())

#load in 2015 data set
transfer1159<- read.csv(file = file.choose()) %>% filter(CURRENT_ADMIT_TYPE_CODE != ".") %>% filter(CURRENT_ADMIT_TERM_CODE %in% c(1156,1159,1161,1163,1166,1169,1171,1173,1176,1179))

#orientation 2015 data pull 
orient1159 <- read.csv(file = file.choose())

#load in 2016 data set
transfer1169<- read.csv(file = file.choose()) %>% filter(CURRENT_ADMIT_TYPE_CODE != ".") %>% filter(CURRENT_ADMIT_TERM_CODE %in% c(1166,1169,1171,1173,1176,1179))

#orientation 2016 data pull 
orient1169 <- read.csv(file = file.choose())


```


####Fall 2014 Orientation Sessions and Transfer students Persistence statistical significance 

```{r fall2014, fig.width = 16, echo=TRUE, message=TRUE, warning=TRUE}

#transform raw transfer retention charatersitics dataset into a data.table format in order to make it easier to change input by reference

setDT(transfer1149)[,paste0("ACADEMIC_TERM_YEAR_NAME", 1:2) := tstrsplit(ACADEMIC_TERM_YEAR_NAME, " ")] #split academic year name into just academic year and actual Year. 

#creeate new variable in our data.table to accomodate split 
transfer1149$ACADEMIC_TERM_YEAR_NAME2 <- as.numeric(transfer1149$ACADEMIC_TERM_YEAR_NAME2)  #academic year name only variable added 
transfer1149$ActualYear <- ifelse(grepl("Spring|Winter|Summer", transfer1149$ACADEMIC_TERM_YEAR_NAME), 
                                  transfer1149$ACADEMIC_TERM_YEAR_NAME2 - 1, transfer1149$ACADEMIC_TERM_YEAR_NAME2) #year numeric only variable added 

#there are duplicate records for each student ID to indicate their progress after they enrolled at the university 
#now we will attempt to combine duplicate record to figure out who dropped or retained 

#here you see how %>% makes the code easier to read

#Find Maximum Quarters Enrolled
maxterm <- transfer1149 %>%
  group_by(STUDENT_ID) %>%
  filter(TOTAL_QUARTERS_ENROLLED == max(TOTAL_QUARTERS_ENROLLED)) %>%
  mutate(MaxQuarters = TOTAL_QUARTERS_ENROLLED) %>%
  dplyr::select(STUDENT_ID, MaxQuarters)

#Find Terms Enrolled in First Year
minyr <- transfer1149 %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ActualYear)) %>%
  mutate(MinYear = ActualYear) %>%
  dplyr::select(STUDENT_ID, MinYear)

minyr2 <- merge(transfer1149, minyr, by="STUDENT_ID", all.x=TRUE)

termsinminyear <- minyr2 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 != "Summer") %>%
  group_by(STUDENT_ID) %>%
  filter(ActualYear == MinYear) %>%
  tally()
setnames(termsinminyear, "n", "FYTermsRetained")

#Did Student Come Back for Second Fall?
WhichFallsRetained <- transfer1149 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 == "Fall") %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ACADEMIC_TERM_CODE)) %>%
  mutate(FirstFall = ACADEMIC_TERM_CODE) %>%
  mutate(SecondFall = FirstFall +10)%>%
  dplyr::select(STUDENT_ID, SecondFall)

hadsecondfall <- merge(transfer1149, WhichFallsRetained, by="STUDENT_ID", all.x=TRUE)
hadsecondfall <- hadsecondfall %>%
  group_by(STUDENT_ID) %>%
  mutate(SecondFallHappened = ifelse(SecondFall == ACADEMIC_TERM_CODE, "Yes", "No")) %>%
  filter(SecondFallHappened == "Yes") %>%
  dplyr::select(STUDENT_ID, SecondFallHappened)

#Now Merge All These, Find Student Status
findingretained <- merge(maxterm, termsinminyear, by="STUDENT_ID", all.x=TRUE)  
findingretained <- merge(findingretained, hadsecondfall, by="STUDENT_ID", all.x=TRUE)
findingretained <- findingretained %>%
  mutate(StudentStatus = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Stop-Out", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Stop-Out", "Retained"))))  %>%
  mutate(StudentStatusGen = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop/Trans/Stop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Drop/Trans/Stop", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Retained", "Retained"))))

#here we join the retained variables to the original dataset

persitencetransfer1149 <- merge(transfer1149, findingretained, by="STUDENT_ID", all.x=TRUE)


#Now, take only the first term for the rest of this study to ignore duplicates 

persitencetransfer1149 <- persitencetransfer1149  %>%
  filter(TOTAL_QUARTERS_ENROLLED == 1) %>% filter(ACADEMIC_TERM_CODE == "1149")

#data integretiy check - open this document to see the retained info properly joined to original 
write.csv(persitencetransfer1149, "retainedJoinRet1149.csv")

#here we add in orientation info 
persitencetransfer1149orient  <- merge(persitencetransfer1149, orient1149, by=c("STUDENT_ID", "CURRENT_ADMIT_TERM_CODE", "CURRENT_ADMIT_TYPE_CODE"), all.x=TRUE)


#data integretiy check - open this document to see that new orientation info properly joined to retainedRet
write.csv(persitencetransfer1149orient, "orientJoinRet1149.csv")


##Fall 2014 Transfer Cohort chiSquare analysis 


#data integretiy check - open this document to see that table is correct

write.csv(persitencetransfer1149orient, "prepForChiSquareAnaysis1149.csv")

#prep knit table 
persitencetransfer1149orient$ORIENTATION_GROUP_DESCRIPTION <- factor(persitencetransfer1149orient$ORIENTATION_GROUP_DESCRIPTION, levels=c("Orientation Session 1", "Orientation Session 2", "Orientation Session 3", "Orientation Session 4", "Orientation Session 5","Orientation Session 6","Late Orientation Session 1", "Late Orientation Session 2", "Late Orientation Session 3", "Late Orientation Session 4", "None Attended ",
                                                                                                                                          "Des Moines Orientation", "Lynnwood Orientation", "Online Orientation Session", "Orientation Session-Quarterly", "Pierce Orientation", "Transfer Orientation Session", "Transfer Orientation Session 2", "Wenatchee Orientation"))

#Fall 2014 Transfer Cohort chiSquare analysis 
#here we calculate statistic
#here we make data readable 

OverallPercent <- persitencetransfer1149orient %>% group_by(StudentStatusGen) %>% tally() %>% mutate(sum = sum(n), percent=n/sum) %>% dplyr::select(StudentStatusGen, percent)
Pret <- as.numeric(as.character(OverallPercent[2,2]))
Pdrop <- as.numeric(as.character(OverallPercent[1,2]))

otally <- persitencetransfer1149orient %>% 
  filter(ActualYear %in% 2014) %>%
  group_by(ORIENTATION_GROUP_DESCRIPTION, StudentStatusGen) %>% 
  tally() %>% 
  dcast(ORIENTATION_GROUP_DESCRIPTION~StudentStatusGen, value.var="n") %>%
  filter(ORIENTATION_GROUP_DESCRIPTION %in% c("Orientation Session 1", "Orientation Session 2", "Orientation Session 3", "Orientation Session 4", "Orientation Session 5","Orientation Session 6","Late Orientation Session 1", "Late Orientation Session 2", "Late Orientation Session 3", "Late Orientation Session 4", "None Attended ",
                                              "Des Moines Orientation", "Lynnwood Orientation", "Online Orientation Session", "Orientation Session-Quarterly", "Pierce Orientation", "Transfer Orientation Session", "Transfer Orientation Session 2", "Wenatchee Orientation"))
otally[is.na(otally)] <-0
otally <- otally %>%
  mutate(Total = Retained + `Drop/Trans/Stop`) %>%
  mutate(PercentageRetained = paste(as.integer(100*Retained/Total), "%")) %>%
  mutate(ExpectedDrop = Pdrop*Total) %>%
  mutate(ExpectedRetained = Pret*Total) %>%
  mutate(ChiVar = ((Retained-ExpectedRetained)^2/ExpectedRetained) + ((`Drop/Trans/Stop`-ExpectedDrop)^2/ExpectedDrop)) %>%
  mutate(pvalue = round(1-(pchisq(ChiVar,1)),3)) %>%
  mutate(SignificantDifference = ifelse(pvalue<.001, "***", ifelse(pvalue<.01, "**", ifelse(pvalue<.05, "*", "")))) %>%
  dplyr::select(ORIENTATION_GROUP_DESCRIPTION, `Retained`, `Drop/Trans/Stop`,  `Total`, `PercentageRetained`, SignificantDifference)

colnames(otally) <- c("Orientation Code", "Retained", "Not Retained", "Total", "Percent Retained", "Statistical Significance")

knitr::kable(otally)


#creating crosstabs for categorical tables 
#check table integrity 
library("gplots")
# 1. convert the data as a table
#dt <- as.table(as.matrix(orient1149))
# 2. Graph
orient1149.tab <- table(orient1149$DTA_FLAG,orient1149$ORIENTATION_GROUP_CODE)
balloonplot(t(orient1149.tab), main ="Relative magnitude of Sessions", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


#library("graphics")
#mosaicplot(orient1149.tab, shade = TRUE, las=2,
#main = "Insight into Orientation Sessions")


# install.packages("vcd")
library("vcd")

?assoc
# plot just a subset of the table
assoc(head(orient1149.tab), shade = TRUE, split_vertical = FALSE, las=2)
chisq.test(orient1149.tab)

```









####Fall 2015 Orientation Sessions and Transfer students Persistence statistical significance 
```{r fall2015,fig.width = 16, echo=TRUE, message=TRUE, warning=TRUE}

#transform raw transfer retention charatersitics dataset into a data.table format in order to make it easier to change input by reference

setDT(transfer1159)[,paste0("ACADEMIC_TERM_YEAR_NAME", 1:2) := tstrsplit(ACADEMIC_TERM_YEAR_NAME, " ")] #split academic year name into just academic year and actual Year. 

#creeate new variable in our data.table to accomodate split 
transfer1159$ACADEMIC_TERM_YEAR_NAME2 <- as.numeric(transfer1159$ACADEMIC_TERM_YEAR_NAME2)  #academic year name only variable added 
transfer1159$ActualYear <- ifelse(grepl("Spring|Winter|Summer", transfer1159$ACADEMIC_TERM_YEAR_NAME), 
                                  transfer1159$ACADEMIC_TERM_YEAR_NAME2 - 1, transfer1159$ACADEMIC_TERM_YEAR_NAME2) #year numeric only variable added 

#there are duplicate records for each student ID to indicate their progress after they enrolled at the university 
#now we will attempt to combine duplicate record to figure out who dropped or retained 

#here you see how %>% makes the code easier to read

#Find Maximum Quarters Enrolled
maxterm1159 <- transfer1159 %>%
  group_by(STUDENT_ID) %>%
  filter(TOTAL_QUARTERS_ENROLLED == max(TOTAL_QUARTERS_ENROLLED)) %>%
  mutate(MaxQuarters = TOTAL_QUARTERS_ENROLLED) %>%
  dplyr::select(STUDENT_ID, MaxQuarters)

#Find Terms Enrolled in First Year
minyr1159 <- transfer1159 %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ActualYear)) %>%
  mutate(MinYear = ActualYear) %>%
  dplyr::select(STUDENT_ID, MinYear)

minyr21159 <- merge(transfer1159, minyr1159, by="STUDENT_ID", all.x=TRUE)


termsinminyear1159 <- minyr21159 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 != "Summer") %>%
  group_by(STUDENT_ID) %>%
  filter(ActualYear == MinYear) %>%
  tally()
setnames(termsinminyear1159, "n", "FYTermsRetained")

#Did Student Come Back for Second Fall?

WhichFallsRetained1159 <- transfer1159 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 == "Fall") %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ACADEMIC_TERM_CODE)) %>%
  mutate(FirstFall = ACADEMIC_TERM_CODE) %>%
  mutate(SecondFall = FirstFall +10)%>%
  dplyr::select(STUDENT_ID, SecondFall)

hadsecondfall1159 <- merge(transfer1159, WhichFallsRetained1159, by="STUDENT_ID", all.x=TRUE)
hadsecondfall1159 <- hadsecondfall1159 %>%
  group_by(STUDENT_ID) %>%
  mutate(SecondFallHappened = ifelse(SecondFall == ACADEMIC_TERM_CODE, "Yes", "No")) %>%
  filter(SecondFallHappened == "Yes") %>%
  dplyr::select(STUDENT_ID, SecondFallHappened)

#Now Merge All These, Find Student Status

findingretained1159 <- merge(maxterm1159, termsinminyear1159, by="STUDENT_ID", all.x=TRUE)  
findingretained1159 <- merge(findingretained1159, hadsecondfall1159, by="STUDENT_ID", all.x=TRUE)
findingretained1159 <- findingretained1159 %>%
  mutate(StudentStatus = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Stop-Out", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Stop-Out", "Retained"))))  %>%
  mutate(StudentStatusGen = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop/Trans/Stop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Drop/Trans/Stop", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Retained", "Retained"))))

#here we join the retained variables to the original dataset

persitencetransfer1159 <- merge(transfer1159, findingretained1159, by="STUDENT_ID", all.x=TRUE)


#Now, take only the first term for the rest of this study to ignore duplicates 

persitencetransfer1159 <- persitencetransfer1159  %>%
  filter(TOTAL_QUARTERS_ENROLLED == 1) %>% filter(ACADEMIC_TERM_CODE == "1159")

#data integretiy check - open this document to see the retained info properly joined to original 
write.csv(persitencetransfer1159, "retainedJoinRet1159.csv")

#here we add in orientation info 
persitencetransfer1159orient  <- merge(persitencetransfer1159, orient1159, by=c("STUDENT_ID", "CURRENT_ADMIT_TERM_CODE", "CURRENT_ADMIT_TYPE_CODE"), all.x=TRUE)


#data integretiy check - open this document to see that new orientation info properly joined to retainedRet
write.csv(persitencetransfer1159orient, "orientJoinRet1159.csv")


#Fall 2015 Transfer Cohort chiSquare analysis 


#data integretiy check - open this document to see that table is correct

#write.csv(persitencetransfer1159orient, "prepForChiSquareAnaysis1159.csv")


OverallPercent1159 <- persitencetransfer1159orient %>% group_by(StudentStatusGen) %>% tally() %>% mutate(sum = sum(n), percent=n/sum) %>% dplyr::select(StudentStatusGen, percent)
Pret1159 <- as.numeric(as.character(OverallPercent[2,2]))
Pdrop1159<- as.numeric(as.character(OverallPercent[1,2]))

otally1159 <- persitencetransfer1159orient %>% 
  filter(ActualYear %in% 2015) %>%
  group_by(ORIENTATION_GROUP_DESCRIPTION, StudentStatusGen) %>% 
  tally() %>% 
  dcast(ORIENTATION_GROUP_DESCRIPTION~StudentStatusGen, value.var="n")


otally1159[is.na(otally1159)] <-0
otally1159 <- otally1159 %>%
  mutate(Total = Retained + `Drop/Trans/Stop`) %>%
  mutate(PercentageRetained = paste(as.integer(100*Retained/Total), "%")) %>%
  mutate(ExpectedDrop = Pdrop*Total) %>%
  mutate(ExpectedRetained = Pret*Total) %>%
  mutate(ChiVar = ((Retained-ExpectedRetained)^2/ExpectedRetained) + ((`Drop/Trans/Stop`-ExpectedDrop)^2/ExpectedDrop)) %>%
  mutate(pvalue = round(1-(pchisq(ChiVar,1)),3)) %>%
  mutate(SignificantDifference = ifelse(pvalue<.001, "***", ifelse(pvalue<.01, "**", ifelse(pvalue<.05, "*", "")))) %>%
  dplyr::select(ORIENTATION_GROUP_DESCRIPTION, `Retained`, `Drop/Trans/Stop`,  `Total`, `PercentageRetained`, SignificantDifference)

colnames(otally1159) <- c("Orientation Code", "Retained", "Not Retained", "Total", "Percent Retained", "Statistical Significance")

knitr::kable(otally1159)



#creating crosstabs for categorical tables 
#check table integrity 
library("gplots")
# 1. convert the data as a table
#dt <- as.table(as.matrix(orient1149))
# 2. Graph
orient1159.tab <- table(orient1159$DTA_FLAG,orient1159$ORIENTATION_GROUP_CODE)
balloonplot(t(orient1159.tab), main ="Relative magnitude of Sessions", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


#library("graphics")
#mosaicplot(orient1159.tab, shade = TRUE, las=2,
# main = "Insight into Orientation Sessions")


# install.packages("vcd")
library("vcd")

?assoc
# plot just a subset of the table
assoc(head(orient1159.tab), shade = TRUE, split_vertical = FALSE, las=2)
chisq.test(orient1159.tab)



```









####Fall 2016 Orientation Sessions and Transfer students Persistence statistical significance 








```{r fall2016, fig.width = 15, echo=TRUE, message=TRUE, warning=TRUE}

#transform raw transfer retention charatersitics dataset into a data.table format in order to make it easier to change input by reference

setDT(transfer1169)[,paste0("ACADEMIC_TERM_YEAR_NAME", 1:2) := tstrsplit(ACADEMIC_TERM_YEAR_NAME, " ")] #split academic year name into just academic year and actual Year. 

#creeate new variable in our data.table to accomodate split 

transfer1169$ACADEMIC_TERM_YEAR_NAME2 <- as.numeric(transfer1169$ACADEMIC_TERM_YEAR_NAME2)  

#academic year name only variable added 


transfer1169$ActualYear <- ifelse(grepl("Spring|Winter|Summer", transfer1169$ACADEMIC_TERM_YEAR_NAME), 
                                  transfer1169$ACADEMIC_TERM_YEAR_NAME2 - 1, transfer1169$ACADEMIC_TERM_YEAR_NAME2) #year numeric only variable added 

#there are duplicate records for each student ID to indicate their progress after they enrolled at the university 
#now we will attempt to combine duplicate record to figure out who dropped or retained 

#here you see how %>% makes the code easier to read

#Find Maximum Quarters Enrolled
maxterm1169 <- transfer1169 %>%
  group_by(STUDENT_ID) %>%
  filter(TOTAL_QUARTERS_ENROLLED == max(TOTAL_QUARTERS_ENROLLED)) %>%
  mutate(MaxQuarters = TOTAL_QUARTERS_ENROLLED) %>%
  dplyr::select(STUDENT_ID, MaxQuarters)

#Find Terms Enrolled in First Year
minyr1169 <- transfer1169 %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ActualYear)) %>%
  mutate(MinYear = ActualYear) %>%
  dplyr::select(STUDENT_ID, MinYear)

minyr21169 <- merge(transfer1169, minyr1169, by="STUDENT_ID", all.x=TRUE)

termsinminyear1169 <- minyr21169 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 != "Summer") %>%
  group_by(STUDENT_ID) %>%
  filter(ActualYear == MinYear) %>%
  tally()
setnames(termsinminyear1169, "n", "FYTermsRetained")

#Did Student Come Back for Second Fall?
WhichFallsRetained1169 <- transfer1169 %>%
  filter(ACADEMIC_TERM_YEAR_NAME1 == "Fall") %>%
  group_by(STUDENT_ID) %>%
  slice(which.min(ACADEMIC_TERM_CODE)) %>%
  mutate(FirstFall = ACADEMIC_TERM_CODE) %>%
  mutate(SecondFall = FirstFall +10)%>%
  dplyr::select(STUDENT_ID, SecondFall)

hadsecondfall1169 <- merge(transfer1169, WhichFallsRetained1169, by="STUDENT_ID", all.x=TRUE)
hadsecondfall1169 <- hadsecondfall1169 %>%
  group_by(STUDENT_ID) %>%
  mutate(SecondFallHappened = ifelse(SecondFall == ACADEMIC_TERM_CODE, "Yes", "No")) %>%
  filter(SecondFallHappened == "Yes") %>%
  dplyr::select(STUDENT_ID, SecondFallHappened)

#Now Merge All These, Find Student Status
findingretained1169 <- merge(maxterm1169, termsinminyear1169, by="STUDENT_ID", all.x=TRUE)  
findingretained1169 <- merge(findingretained1169, hadsecondfall1169, by="STUDENT_ID", all.x=TRUE)
findingretained1169 <- findingretained1169 %>%
  mutate(StudentStatus = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Stop-Out", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Stop-Out", "Retained"))))  %>%
  mutate(StudentStatusGen = ifelse(is.na(SecondFallHappened) & MaxQuarters <=3, "Drop/Trans/Stop", ifelse(is.na(SecondFallHappened) & MaxQuarters >3, "Drop/Trans/Stop", ifelse(SecondFallHappened == "Yes" & FYTermsRetained <3, "Retained", "Retained"))))

#here we join the retained variables to the original dataset

persitencetransfer1169 <- merge(transfer1169, findingretained1169, by="STUDENT_ID", all.x=TRUE)


#Now, take only the first term for the rest of this study to ignore duplicates 

persitencetransfer1169 <- persitencetransfer1169  %>%
  filter(TOTAL_QUARTERS_ENROLLED == 1) %>% filter(ACADEMIC_TERM_CODE == "1169")

#data integretiy check - open this document to see the retained info properly joined to original 
write.csv(persitencetransfer1169, "retainedJoinRet1169.csv")

#here we add in orientation info 
persitencetransfer1169orient  <- merge(persitencetransfer1169, orient1169, by=c("STUDENT_ID", "CURRENT_ADMIT_TERM_CODE", "CURRENT_ADMIT_TYPE_CODE"), all.x=TRUE)


#data integretiy check - open this document to see that new orientation info properly joined to retainedRet
write.csv(persitencetransfer1169orient, "orientJoinRet1169.csv")


#Fall 2015 Transfer Cohort chiSquare analysis 
#here we calculate statistic
#here we make data readable 

OverallPercent1169 <- persitencetransfer1169orient %>% group_by(StudentStatusGen) %>% tally() %>% mutate(sum = sum(n), percent=n/sum) %>% dplyr::select(StudentStatusGen, percent)
Pret1169 <- as.numeric(as.character(OverallPercent[2,2]))
Pdrop1169<- as.numeric(as.character(OverallPercent[1,2]))

#here we aggregate 
otally1169 <- persitencetransfer1169orient %>% 
  filter(ActualYear %in% 2016) %>%
  group_by(ORIENTATION_GROUP_DESCRIPTION, StudentStatusGen) %>% 
  tally() %>% 
  dcast(ORIENTATION_GROUP_DESCRIPTION~StudentStatusGen, value.var="n")


#integrity check - 

otally1169[is.na(otally1169)] <-0

#then continue with aggreagtion 

otally1169[is.na(otally1169)] <-0
otally1169 <- otally1169 %>%
  mutate(Total = Retained + `Drop/Trans/Stop`) %>%
  mutate(PercentageRetained = paste(as.integer(100*Retained/Total), "%")) %>%
  mutate(ExpectedDrop = Pdrop*Total) %>%
  mutate(ExpectedRetained = Pret*Total) %>%
  mutate(ChiVar = ((Retained-ExpectedRetained)^2/ExpectedRetained) + ((`Drop/Trans/Stop`-ExpectedDrop)^2/ExpectedDrop)) %>%
  mutate(pvalue = round(1-(pchisq(ChiVar,1)),3)) %>%
  mutate(SignificantDifference = ifelse(pvalue<.001, "***", ifelse(pvalue<.01, "**", ifelse(pvalue<.05, "*", "")))) %>%
  dplyr::select(ORIENTATION_GROUP_DESCRIPTION, `Retained`, `Drop/Trans/Stop`,  `Total`, `PercentageRetained`, SignificantDifference)

colnames(otally1169) <- c("Orientation Code", "Retained", "Not Retained", "Total", "Percent Retained", "Statistical Significance")

knitr::kable(otally1169)


#creating crosstabs for categorical tables 
#check table integrity 
library("gplots")
# 1. convert the data as a table
#dt <- as.table(as.matrix(orient1149))
# 2. Graph
orient1169.tab <- table(orient1169$DTA_FLAG,orient1169$ORIENTATION_GROUP_CODE)
balloonplot(t(orient1169.tab), main ="Relative magnitude of Sessions", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


#library("graphics")
#mosaicplot(orient1169.tab, shade = TRUE, las=2,
#         main = "Insight into Orientation Sessions")


# install.packages("vcd")
library("vcd")

?assoc
# plot just a subset of the table
assoc(head(orient1169.tab), shade = TRUE, split_vertical = FALSE, las=2)
chisq.test(orient1169.tab)



```






