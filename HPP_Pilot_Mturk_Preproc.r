#### HPP_Pilot_Mturk_Preproc####
#
### Purpose ###
# This script is for pre-processing the pilot data from MTurk as reported in IJzerman et al.(2018), Human Penguin Project (HPP).
# Overview of HPP: https://osf.io/2rm5b/
#
#
# Code author: Chuan-Peng Hu, PhD, 
# Affliated to: Neuroimaging Center (NIC), Johannes Gutenberg University Medical Center, 55131 Mainz, Germany;
# Email: hcp4715@gmail.com
# 
# Author   Date (d/m/y) Notes/Changes
# ========   =========   ========
# C-P. Hu    09/08/18    split the preprocess into two file: preproc, and score calculating, this one is preproc.
#
#
### input data ####
#
# Oringinal data: sav file: 'Core_Temperature_Study__Improved_MTurk total.sav' 
#
# Revised data: 'HPP_MTurk_cleaned_valid.csv' (with codebook 'Codebook_HPP_prolific_academic_0619.xlsx')
#       We thanks Jixin Yin for check the data and prepare the code book.
#
# Also, the weight, height, avghumid, and mintemp data were from the data reported in previous version of the manuscript:pilotpenguins.sav
# 
### output file and Variables ####
#
# output file: 'Data_Raw_HPP_Pilot_Mturk_Share.csv'
# 
# Data related to the following variables are kept (reported in the article):
# Age            -- the birth year.
# anxiety        -- subscale of attachment, Fraley et al., 2000,  using sum score
# attachhome     -- attachment to home; Harris et al., 1996       using average score
# attachphone    -- attachment to the phone                       using sum score
# avghumid       -- average humidity of the day                   
# avgtemp        -- average temperature
# avoidance      -- subscale of attachment, Fraley et al., 2000   using sum score
# gluctot        -- daily sugary drink consumption, Henriksen et al., 2014
# health         -- health condition 
# height         -- height (in meter)
# Medication     -- medication condition
# mintemp        -- minimum temperature of the day
# networksize    -- social network; Cohen et al., 1997
# nostalgia      -- (Routledge et al., 2008)                       using sum score
# selfcontrol    -- self-control, Tangney et al., 2004             using sum score
# Sex 
# site           -- the plocation of participants
# smoking        -- 
# socialdiversity--
# socialembedded -- social network; Cohen et al., 1997
# stress         -- Perceived stress (Cohen & Wills, 1985)         using sum score
# weight         -- wightkg

### Preparing ####
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English

rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep = TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# packages
pkgNeeded <- (c("randomForest","plyr","foreign", "party", 'tree','lattice',
                'stargazer',"summarytools","psych","car",'memisc'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';


#### Preprocessing #####
# load the spss file for the country information, the '.por' file is transferred from '.sav' file
data_spss <- data.frame(as.data.set(spss.portable.file("HPP_pilot_MTurk_cleaned_deidentified.por")))

# Read data
DataRaw <- read.csv("HPP_MTurk_cleaned.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

## read the data from previous validated
repoData_MT_s <- read.csv("Mturk_From_osf_rm_diff.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
repoData_MT_s_reord <- repoData_MT_s[with(repoData_MT_s, order(age, anxiety,avoidance)), ] # order based on "age", "anxiety", and "avoidance"

# recode the temperature:
# exclude participants
# criteria: 
# c1: average temperation is greater than 34.99
# c2: not drink or eat somethin cold or warm in 10 minutes before (eatdrink = 1)
# c3: no exercise in 60 mintues before the survey (exercise = 2)

# first: filter eatdrinking
valid.data_Eat <- subset(DataRaw, eatdrink != 1)   # eat or drink  (0 participants)
valid.data_NA <- subset(DataRaw, is.na(eatdrink))  # eat or drink data is NA (0 participants)
valid.data_NoEat <- subset(DataRaw, eatdrink == 1) # No eat of drink

# Second: filter exercise
valid.data_exercise <- subset(valid.data_NoEat, exercise != 2) # did exercise within one hour (1 participants)
valid.data_exercise_NA <- subset(valid.data_NoEat, is.na(exercise))
valid.data_NoExercise <- subset(valid.data_NoEat, exercise == 2) # did exercise within one hour

# Third: filter average temperature
valid.data_Tmp <- subset(valid.data_NoExercise, avgtemp < 34.99)  # participant that not excluded by the other two criteria (0 participant)

valid.data <- subset(DataRaw,avgtemp > 34.99 & eatdrink == 1 & exercise == 2) # average temperature higher than 34.99 is valid

# write.csv(valid.data,'Data_raw_HPP_Pilot_Mturk_No_share.csv',row.names = F)

#### detecting the differences
valid.data$birthyear <- as.integer(paste("19",as.character(round(valid.data$birthyear,2)),sep = ''))

valid.data$age <- valid.data$birthyear # calcuate the age for each participant

DataRaw$birthyear <- as.integer(paste("19",as.character(round(DataRaw$birthyear,2)),sep = ''))
colnames(valid.data)[colnames(valid.data) == 'sex'] <- 'Sex'

#DataRaw$age <- 2015 - DataRaw$birthyear # calcuate the age for each participant

# detect differences between pilot-osf data and my data
## calculated the anxiety and attachhome score for re-ordering
ECRanxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
                      "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyKeys  <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
ECRanxietyKeys2 <- c("ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "-ECR9", "ECR10", "-ECR11",
                     "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyScore <- psych::scoreItems(ECRanxietyKeys2,valid.data[,ECRanxietyNames], totals = T, min = 1, max = 7) 
valid.data$anxiety <-  ECRanxietyScore$scores   # sum score
#
#ECRanxietyScore2 <- psych::scoreItems(ECRanxietyKeys2,DataRaw[,ECRanxietyNames], totals = T, min = 1, max = 7) 
#DataRaw$anxiety <-  ECRanxietyScore2$scores   # sum score
#
### score and alpha for ECR avoidance ####
ECRavoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                        "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
ECRavoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
ECRavoidanceKeys2 <- c("ECR19","-ECR20","ECR21","-ECR22", "ECR23","ECR24","ECR25","-ECR26","-ECR27",
                       "-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33", "-ECR34","-ECR35","-ECR36")
ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,valid.data[,ECRavoidanceNames], totals = T, min = 1, max = 7)
valid.data$avoidance <- ECRavoidanceScore$scores # sum score

ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,valid.data[,ECRavoidanceNames], totals = T, min = 1, max = 7)
valid.data$avoidance <- ECRavoidanceScore$scores # sum score
#
## re-order the data
valid.data_reord <- valid.data[with(valid.data, order(age, anxiety,avoidance)), ] # order based on "age", "anxiety", and "avoidance"
#write.csv(valid.data_reord[,c("age", "anxiety","avoidance")],'Mturk_pilot_clean.csv',row.names = F)
#
#DataRaw_reord <- DataRaw[with(DataRaw, order(age, anxiety,avoidance)), ] # order based on "age", "anxiety", and "avoidance"
#write.csv(DataRaw_reord[,c('V1',"age", "anxiety","avoidance")],'Mturk_pilot_all.csv',row.names = F)
#
## read the data from previous validated
#repoData <- read.csv("pilotpenguins_hans.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
#repoData_MT <- subset(repoData,Site == 1)
#repoData_MT_reord <- repoData_MT[with(repoData_MT, order(age, anxiety,avoidance)), ] # order based on "age", "anxiety", and "avoidance"
#write.csv(repoData_MT_reord,'Mturk_reported_osf.csv',row.names = F)

## save the useful variable for later open data
SNINames <- paste("SNI",1:32,sep = '') # colnames for social network indices
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , 
                   "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13","stress14")
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9")
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                  "onlineid9", "onlineid10", "onlineide11")
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )

didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")

OtherNames <- c('birthyear','Sex','avgtemp','health')

selectNames <- c(OtherNames,SNINames,scontrolNames,stressNames,phoneNames,onlineNames,ECRNames,homeNames,nostagliaNames,didfNames,eotNames)

valid.data_share <- valid.data_reord[,selectNames]

# newNames <- c('glucoseplosone','Site','avghumid','mintemp')
## gluctot and artgluctot (already calculated in multi-site dataset)

valid.data_share$glucoseplosone <- rowSums(valid.data_reord[,c("Q89_6_1_TEXT",'Q89_7_1_TEXT','Q89_12_1_TEXT')],na.rm = T)
#Datasum$artgluctot <- rowSums(valid.data[,c("Q89_8_1_TEXT",'Q89_9_1_TEXT','Q89_13_1_TEXT')],na.rm = T)
valid.data_share$Site       <- "Mturk"
valid.data_share$birthyear  <- valid.data_reord$birthyear
#valid.data_share$avgtemp    <- valid.data_reord$avgtemp_r
valid.data_share$Medication <- valid.data_reord$meds
valid.data_share$Smoking    <- valid.data_reord$smoke

# from osf reported data
valid.data_share$avghumid <- repoData_MT_s_reord$avghumid
valid.data_share$mintemp  <-  repoData_MT_s_reord$mintemp
#valid.data_share$heightm  <- repoData_MT_s_reord$heightm
#valid.data_share$weightkg <- repoData_MT_s_reord$weightkg

# write the sharable data
write.csv(valid.data_share,'Data_Raw_HPP_Pilot_MT_Share.csv',row.names = F)
