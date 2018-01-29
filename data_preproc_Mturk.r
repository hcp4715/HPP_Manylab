#### Code_pre-proc_MTurk_HPP####
#
### Purpose ###
# Pre-processing the data from pilot data from m-Turk as reported in IJzerman et al.(2018), Human Penguin Project (HPP).
# Overview of HPP: https://osf.io/2rm5b/ 
# 
#
# Code author: Chuan-Peng Hu, PhD, 
# Affliated to: Neuroimaging Center (NIC), Johannes Gutenberg University Medical Center, 55131 Mainz, Germany;
# Email: hcp4715@gmail.com
# 
# Author      Date       Notes/Changes
# ========   =========   ========
# C-P. Hu    23/12/17    add comparison between results here and reported in article
# C-P. Hu    26/01/18    add more specific criteria for excluding data
#
#
### input data ####
#
# Oringinal data: sav file: 'Core_Temperature_Study__Improved_MTurk total.sav' 
#
# Revised data: 'HPP_MTurk_cleaned.csv' (with codebook 'Codebook_HPP_mTurk_0613.xlsx')
#
# 
### output file and Variables ####
#
# output file: 'summary_pilot_MTurk.csv'
# 
# including following variables:
# Age
# Sex 
# stress         -- Perceived stress (Cohen & Wills, 1985)
# nostalgia      -- (Routledge et al., 2008)
# attachhome     -- attachment to home; Harris et al., 1996
# selfcontrol    -- self-control, Tangney et al., 2004
# avoidance      -- subscale of attachment, Fraley et al., 2000
# anxiety        -- subscale of attachment, Fraley et al., 2000
# EOT            -- alexithymia subscale; Kooiman et al., 2002
# DIDF           -- alexithymia subscale; Kooiman et al., 2002
# networksize    -- social network; Cohen et al., 1997
# socialembedded -- social network; Cohen et al., 1997
# CSI            -- complex social integration, social network; Cohen et al., 1997
# gluctot        -- daily sugary drink consumption, Henriksen et al., 2014
# artgluctot     -- diet drinks consumption, Henriksen et al., 2014 
# height         -- height
# weight         -- wightkg
# mintemp        -- minimum temperature of the day
# avghumidity    -- average humidity of the day
#
### final Note ####
#
# This script is largely based on spss syntax file 'Syntax to Calculate Scales and Reliabilities.sps'
#
#### compare results in article and here ####
#
# Items         In Article      Output of this script
# ============  ===========     ========================
# valid data    140(excluded 3) 141 (1 exclusion)
# selfcontrol                   0.8784647
# stress                        0.8985846
# attachphone                   0.8993025
# onlineid                      0.9109445
# ECR-total                     0.953831
# ECR-anxiety                   0.9286249
# ECR-avoidance                 0.9578632
# nostalgia                     0.9433518
# Alex-didf                     0.9139629
# Alex-eot                      0.5731632
# attachhome                    0.8815955
#
### Preparing ####
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set language to Egnlish

rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions

# require packages, if not installed, install the packages first.
pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

# packages needed
pkgNeeded <- (c("randomForest","plyr","foreign", "party", 'tree','lattice',
                'stargazer',"summarytools","psych","car",'memisc'))

lapply(pkgNeeded,pkgTest)   # require needed packages
rm('pkgNeeded') # remove the variable 'pkgNeeded';

#### Begin the pre-processing ####
# load the spss file for the country information, the '.por' file is transferred from '.sav' file
data_spss <- data.frame(as.data.set(spss.portable.file("HPP_pilot_MTurk_cleaned_deidentified.por")))

# Read data
DataRaw <- read.csv("HPP_MTurk_cleaned.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# recode the temperature:
DataRaw$Temperature_t1 <- if (any(DataRaw$Q8 == 2)) (((DataRaw$Q7-32)*5)/9) else DataRaw$Q7
DataRaw$Temperature_t2 <- if (any(DataRaw$Q66 == 2)) (((DataRaw$Q65-32)*5)/9) else DataRaw$Q65
DataRaw$country_region <- data_spss$country_  # get the country information
rm(data_spss)  # remove the spss data

# exclude participants
# criteria: 
# c1: average temperation is greater than 34.99
# c2: not drink or eat somethin cold or warm in 10 minutes before (eatdrink = 1)
# c3: no exercise in 60 mintues before the survey (exercise = 2)

# first: filter eatdrinking
valid.data_Eat <- subset(DataRaw, eatdrink != 1)   # eat or drink  (3 participants)
valid.data_NA <- subset(DataRaw, is.na(eatdrink))  # eat or drink data is NA (2 participants)
valid.data_NoEat <- subset(DataRaw, eatdrink == 1) # No eat of drink

# Second: filter exercise
valid.data_exercise <- subset(valid.data_NoEat, exercise != 2) # did exercise within one hour (2 participants)
valid.data_exercise_NA <- subset(valid.data_NoEat, is.na(exercise))
valid.data_NoExercise <- subset(valid.data_NoEat, exercise == 2) # did exercise within one hour

# Third: filter average temperature
valid.data_Tmp <- subset(valid.data_NoExercise, avgtemp < 34.99)  # participant that not excluded by the other two criteria (1 participant)

valid.data <- subset(DataRaw,avgtemp > 34.99 & eatdrink == 1 & exercise == 2) # average temperature higher than 34.99 is valid
write.csv(valid.data,'HPP_MTurk_cleaned_valid.csv',row.names = F)

#### Start to calculatin scores #### 

# create the dataframe for summary data that can be used for later use
Datasum <- valid.data[,c('age','sex')]               # age, sex
colnames(Datasum)[colnames(Datasum) == 'sex'] <- 'Sex'
Datasum$avgtemp <- (valid.data$Temperature_t1 + valid.data$Temperature_t2)/2  # average temperature

#### calculate social network index ####
## soical diveristy 
# for social diversity, we re-code the types of relationship into 1 or 0
# so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were car::recoded
SNINames <- c("SNI1","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
              "SNI21","SNI28","SNI29","SNI30","SNI31","SNI32")
snDivNames <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
                "SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32") 
SNIData <- valid.data[,SNINames]

# recode Q10
SNIData$SNI1_r <- car::recode(SNIData$SNI1,"1= 1; else = 0")

# re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
socDivData_r <- apply(SNIData[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 
socDivData_r <- data.frame(socDivData_r)
# add suffix to the colnames
colnames(socDivData_r) <- paste(colnames(socDivData_r),"div",  sep = "_")
socDivData_r$SNIwork <- socDivData_r$SNI17_div + socDivData_r$SNI18_div   # combine the social network for work
socDivData_r$SNIwork_r <- car::recode(socDivData_r$SNIwork,"0 = 0;1:10 = 1")
SNIData <- cbind(SNIData, socDivData_r)  # combine by columne of re-coded data

# extra groups, 0 --> 0; more than 0 --> 1

extrDivData <- valid.data[,extrDivName]
# re-code other groups: 0/NA -> 0; else -> 1
extrDivData_r <- apply(extrDivData,2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; else = 1"); x}) 
extrDivData_r <- data.frame(extrDivData_r)
# sum the other groups
extrDivData_r$extrDiv <- rowSums(extrDivData_r)
# re-code other groups again
extrDivData_r$extrDiv_r <- car::recode(extrDivData_r$extrDiv,'0 = 0; else = 1')
SNIData$extrDiv_r <- extrDivData_r$extrDiv_r

# add social diversity with other groups
snDivNames_r <- c("SNI1_r","SNI3_div","SNI5_div","SNI7_div","SNI9_div","SNI11_div","SNI13_div","SNI15_div","SNIwork_r",
                  "SNI19_div","SNI21_div","extrDiv_r")
SNIData$SNdiversity <- rowSums(SNIData[,snDivNames_r])

# Social Network size
snSizeNames <- c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21")

# NOTE: In our experience, individuals sometimes interpret the SNI item inquiring about the number of "other group" 
# members with whom they interact at least once every 2 weeks more broadly than we intended, with some respondents 
# reporting up to 100 or more fellow group-members. To ensure that social network size scores are not artificially inflated by 
# individuals reporting large group memberships, we recommend recoding the variable so that all values over 6 are given a 
# score of 7, thus keeping it consistent with all other quantitative SNI items.
extrSizeData_r <- apply(extrDivData,2,function(x) {x <- car::recode(x,"0 = 0; NA = 0;1 = 1; 2= 2; 3= 3; 4= 4;5= 5; 6 = 6; else = 7"); x}) 
extrSizeData_r <- data.frame(extrSizeData_r)
# add suffix to the colnames
colnames(extrSizeData_r) <- paste(colnames(extrSizeData_r),"sz",  sep = "_")

SNSizeData <- cbind(SNIData,extrSizeData_r)
SNSizeNames_r <- c("SNI1_r","SNI3", "SNI5", "SNI7", "SNI9" , "SNI11", "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21",
                   "SNI28_sz","SNI29_sz","SNI30_sz","SNI31_sz","SNI32_sz")
SNSizeData$snSize <- rowSums(SNSizeData[,SNSizeNames_r],na.rm=TRUE)

## number of embedded networks
## family: SNI1_r, SNI3,SNI5,SNI7,SNI9 (total >4);
## friends: SNI11 (>4);
## Church: SNI13 (>4);
## Students/school: SNI 15 (>4)
## Work: SNI17 + SNI 18 >4
## neighbor: SNI19 >4
## volunteer SNI21 >4
## other groups: totoal > 4
SNSizeData$familyNW <- rowSums(SNSizeData[,c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9")])
SNSizeData$familyNW_r <- car::recode(SNSizeData$familyNW,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$friendNW_r <- car::recode(SNSizeData$SNI11,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$churchNW_r <- car::recode(SNSizeData$SNI13,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$studyNW_r <- car::recode(SNSizeData$SNI15,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$workNW <- SNSizeData$SNI17 + SNSizeData$SNI18 
SNSizeData$workNW_r <- car::recode(SNSizeData$workNW,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$neighbor_r <- car::recode(SNSizeData$SNI19,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$volun_r <- car::recode(SNSizeData$SNI21,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$extra <- rowSums(SNSizeData[,c("SNI28","SNI29","SNI30","SNI31","SNI32")])
SNSizeData$extra_r <- car::recode(SNSizeData$extra,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$socEmbd <- rowSums(SNSizeData[,c("familyNW_r","friendNW_r","churchNW_r","studyNW_r","workNW_r",
                                            "neighbor_r","volun_r","extra_r")])

## calculate the complex social integration
Datasum$socialdiversity <- SNIData$SNdiversity # complex social integration
Datasum$networksize <- SNSizeData$snSize 
Datasum$socialembedded <- SNSizeData$socEmbd 

#### below is the calculating of scale score and aphla coefficient for each scale ####

#### score and alpha for self control scale ####
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , 
                   "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
# scontrolKeys2 <- list(c(1,-1,-1,-1,-1,1,-1,1,-1,-1,1,-1,-1)) #  this is the original scale with reverse coding
scontrolKeys2 <- list(c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                   "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" ))
# scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # in case if the score in this dataset is already reversed

scontrolAlpha <- psych::alpha(valid.data[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)    # std. alpha: 0.8784647

SelfControlScore <- psych::scoreItems(scontrolKeys2,valid.data[,scontrolNames], min = 1, max = 5)
print(SelfControlScore$alpha)

# average score, using the most simple way
# Datasum$selfcontrol <- (valid.data$scontrol1 + (6 - valid.data$scontrol2) + (6 - valid.data$scontrol3) + (6 - valid.data$scontrol4)
#                         + (6 - valid.data$scontrol5) + valid.data$scontrol6 + (6 - valid.data$scontrol7) + valid.data$scontrol8
#                        + (6 - valid.data$scontrol9) + (6 - valid.data$scontrol10) + valid.data$scontrol11 + (6 - valid.data$scontrol12)
#                        + (6 - valid.data$scontrol13))/length(scontrolNames) 
Datasum$selfcontrol <- SelfControlScore$scores

## score and alpha for perceive stress ####
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13", "stress14")
stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
stressKeys2 <- list(c("stress1" , "stress2" ,"stress3","-stress4", "-stress5", "-stress6", "-stress7", "stress8",
                 "-stress9", "-stress10","stress11", "stress12", "-stress13", "stress14"))
# stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # in case the score is already re-coded

stressAlpha <- psych::alpha(valid.data[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # std. alpha: 0.8985846

stressScore <- psych::scoreItems(stressKeys2,valid.data[,stressNames],min = 1, max = 5)
# Datasum$stress <- (valid.data$stress1 + valid.data$stress2 + valid.data$stress3 + (6 - valid.data$stress4)
#                    + (6 - valid.data$stress5) + (6 - valid.data$stress6) + (6 - valid.data$stress7) + valid.data$stress8
#                    + (6 - valid.data$stress9) + (6 - valid.data$stress10) + valid.data$stress11 + valid.data$stress12
#                    + (6 - valid.data$stress13)+ valid.data$stress14)/length(stressNames) # average score
Datasum$stress <- stressScore$scores

## score and alpha for attach phone ####
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )
phoneAlpha <- psych::alpha(valid.data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.899
Datasum$attachphone <- rowSums(valid.data[,phoneNames],na.rm = T)/length(phoneNames) # average score

## score and alpha for online ####
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")
onlineAlpha <- psych::alpha(valid.data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.91
Datasum$online <- rowSums(valid.data[,onlineNames],na.rm = T)/length(onlineNames) # average score

## score and alpha for ECR ####
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,-11,12,13,14,15,16,17,18,
             19,-20,21,-22,23,24,25,-26,27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
# make the key list for scoreItems
ECRKeys2 <- list(c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "-ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","-ECR20","ECR21","-ECR22",
               "ECR23","ECR24","ECR25","-ECR26","ECR27","-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33",
               "-ECR34","-ECR35","-ECR36"))

ECRAlpha <- psych::alpha(valid.data[,ECRNames], 
                            keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.953831
ECRScore <- psych::scoreItems(ECRKeys2,valid.data[,ECRNames], min = 1, max = 7)
Datasum$ECR <- ECRScore$scores # average score

## score and alpha for ECR Anxiety
ECRanxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
ECRanxietyKeys2 <- list(c("ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "-ECR11",
                      "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18"))
ECRanxietyAlpha <- psych::alpha(valid.data[,ECRanxietyNames], 
                         keys=ECRanxietyKeys)  # calculate the alpha coefficient 
print(ECRanxietyAlpha$total)  # std. alpha 0.928, instead of 0.92
ECRanxietyScore <- psych::scoreItems(ECRanxietyKeys2,valid.data[,ECRanxietyNames], min = 1, max = 7)
Datasum$ECRanxeity <- ECRanxietyScore$scores # average score

## score and alpha for ECR avoidance
ECRavoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                     "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
ECRavoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
ECRavoidanceKeys2 <- list(c("ECR19","-ECR20","ECR21","-ECR22", "ECR23","ECR24","ECR25","-ECR26","ECR27",
                   "-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33", "-ECR34","-ECR35","-ECR36"))

ECRavoidanceAlpha <- psych::alpha(valid.data[,ECRavoidanceNames], 
                             keys=ECRavoidanceKeys)  # calculate the alpha coefficient 
print(ECRavoidanceAlpha$total)  # std. alpha 0.958, instead of 0.916
ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,valid.data[,ECRavoidanceNames], min = 1, max = 7)
Datasum$ECRavoidance <- ECRavoidanceScore$scores # average score

## score and alpha for nostaglia ####
# Note the SNS1 is reverse coding
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostagliaKeys2 <- list(c( "-SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" ))
# nostagliaKeys <- c(1,2,3,4,5,6,7) # in case the score is already re-coded
nostagliaAlpha <- psych::alpha(valid.data[,nostagliaNames], keys=nostagliaKeys)  # calculate the alpha coefficient 
print(nostagliaAlpha$total)  # std. alpha 0.943, instead of 0.92

nostagliaScore <- psych::scoreItems(nostagliaKeys2,valid.data[,nostagliaNames],min = 1, max = 7) ## 
Datasum$nostaglia <- nostagliaScore$scores

## score and alpha coefficient for ALEX ####
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys2 <- list(c("ALEX1","ALEX2","ALEX3","-ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11"))
#didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11) # in case the score is already re-coded

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys2 <- list(c("-ALEX12","ALEX13","-ALEX14","ALEX15" ,"-ALEX16"))
# eotKeys <- c(1,2,3,4,5) # in case the score is already re-coded

#Datasum$didf <- rowSums(valid.data[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.data[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF: std.aplha: 0.9139629
didfScore <- psych::scoreItems(didfKeys2,valid.data[,didfNames], min = 1, max = 5)
Datasum$didf <- didfScore$scores

#Datasum$eot <- rowSums(valid.data[,eotNames],na.rm = T)/length(eotNames) # average score
eotfAlpha <-  psych::alpha(valid.data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotfAlpha$total)  # print the alpha for eot:std. alpha: 0.573
eotScore <- psych::scoreItems(eotKeys2,valid.data[,eotNames], min = 1, max = 5)
Datasum$eot <- eotScore$scores

## score and alpha for attachemnt to home ####
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.data[,homeNames], 
                               keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.88, instead of 0.901
Datasum$attachhome <- rowSums(valid.data[,homeNames],na.rm = T)/length(homeNames)
#homeItem <- psych::scoreItems(homeKeys,valid.data[,homeNames],min = 1, max = 5) ## 

## gluctot and artgluctot ####
# already calculated in multi-site dataset
Datasum$glucoseplosone <- rowSums(valid.data[,c("Q89_6_1_TEXT",'Q89_7_1_TEXT','Q89_12_1_TEXT')],na.rm = T)
Datasum$artgluctot <- rowSums(valid.data[,c("Q89_8_1_TEXT",'Q89_9_1_TEXT','Q89_13_1_TEXT')],na.rm = T)
Datasum$Site <- "Mturk"
Datasum$heightm <- valid.data$heightm
Datasum$weightkg <- valid.data$weightkg
Datasum$health <- valid.data$health
Datasum$avghumid <- NA
Datasum$Medication <- valid.data$meds
Datasum$Smoking <- valid.data$smoke
Datasum$mintemp <- NA
#Datasum$gluctot <- rowSums(valid.data[,c("Q89_6_1_TEXT",'Q89_7_1_TEXT','Q89_12_1_TEXT')],na.rm = T)
#Datasum$artgluctot <- rowSums(valid.data[,c("Q89_8_1_TEXT",'Q89_9_1_TEXT','Q89_13_1_TEXT')],na.rm = T)
DatasumSort <- subset(Datasum[ , order(names(Datasum))])

# write to sum data

write.csv(DatasumSort,'summary_pilot_MTurk.csv',row.names = F)

##### end ####

