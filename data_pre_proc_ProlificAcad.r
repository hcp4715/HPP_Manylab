#### Code_pre-proc_ProlificAcad_HPP####
#
### Purpose ###
# Pre-processing the data from pilot data from prolific Academia as reported in IJzerman et al.(2018), Human Penguin Project (HPP).
# Overview of HPP: https://osf.io/2rm5b/ 
# 
#
# Code author: Chuan-Peng Hu, PhD, 
# Affliated to: Neuroimaging Center (NIC), Johannes Gutenberg University Medical Center, 55131 Mainz, Germany;
# Email: hcp4715@gmail.com
# 
# Author      Date       Notes/Changes
# ========   =========   ========
# C-P. Hu    17/01/17    add more notations
# C-P. Hu    26/01/18    add more specific criteria for excluding data
#
#
### input data ####
#
# Oringinal data: sav file: 'prolific academic corrected dataset december 2015.sav' 
#
# Revised data: 'prolific_academic_corrected_201512_rev_yjx2_3.csv' (with codebook 'Codebook_HPP_prolific_academic_0619.xlsx')
#       We thanks Jixin Yin for check the data and prepare the code book.
# 
### output file and Variables ####
#
# output file: 'summaryProflificAcd.csv'
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
# valid data    148(excluded 48) 100 (exclude 8, 92 valid)
# selfcontrol                   0.8734
# stress                        0.8971
# attachphone                   0.8698
# onlineid                      0.8936
# ECR-total                     0.95389
# ECR-anxiety                   0.93678
# ECR-avoidance                 0.9451
# nostalgia                     0.9499748
# Alex-didf                     0.9081569
# Alex-eot                      0.560
# attachhome                    0.9067
#
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

# this belowing code was not used.
## to read spss file with duplicated labels, fixed this error from: https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
#Int2Factor <- function(x){
#        if(!is.null(attr(x, "value.labels"))){
#                vlab <- attr(x, "value.labels")
#                if(sum(duplicated(vlab)) > 0)
#                        cat("Duplicated levels:", vlab, "\n")
#                else if(sum(duplicated(names(vlab))) > 0)
#                        cat("Duplicated labels:",
#                            names(vlab)[duplicated(names(vlab))], "\n")
#                else
#                        x <- factor(x, levels = as.numeric(vlab),
#                                    labels = names(vlab))
#        }
#        x
#}

#mul_data_raw <- read.spss("HPP_pilot_MTurk_cleaned_deidentified.sav", use.value.labels = FALSE)
#mul_data_raw <- lapply(mul_data_raw, Int2Factor)
#mul_data_raw <- as.data.frame(mul_data_raw, stringsAsFactors = FALSE)

#attach(mul_data_raw)

#data_spss <- data.frame(as.data.set(spss.portable.file("HPP_pilot_MTurk_cleaned_deidentified.por")))

# I used the following code to read data
DataRaw <- read.csv("prolific_academic_corrected_201512_rev_yjx2_3.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# define the cleaned data:
nameSumData <- c("age","anxiety", "attachhome", "attachphone","avghumid", "avgtemp", "avoidance", "glucoseplosone",
                        "health", "heightm", "Medication", "mintemp","networksize","nostalgia", "selfcontrol", "Sex",
                        "Site","Smoking", "socialdiversity", "socialembedded", "stress", "weightkg")

# recode the temperature:
# there was one participants filled 32 for Q7 and 2 for Q8, resulted 0 for t1; however, the results of Q66 was 1, 
# and again the answer for Q65 was 32. so here I change the answer for Q8 as 1.
DataRaw$Q8[DataRaw$Q7 == 32 & DataRaw$Q8 == 2] <- 1

# DataRaw$Temperature_t1_r <- if (any( DataRaw$Q8 == 2)) (((DataRaw$Q7-32)*5)/9) else DataRaw$Q7
DataRaw$Temperature_t1_r <- DataRaw$Q7

# transfer the temperature at T1 to the same scale
for (ii in 1:length(DataRaw$Q8)){
        if (DataRaw$Q8[ii] ==2){
                DataRaw$Temperature_t1_r[ii] <- ((DataRaw$Q7[ii]-32)*5)/9
        }
        else DataRaw$Temperature_t1_r[ii] <- DataRaw$Q7[ii]
}

# DataRaw$Temperature_t2_r <- if (any(DataRaw$Q66 == 2)) (((DataRaw$Q65-32)*5)/9) else DataRaw$Q65
DataRaw$Temperature_t2_r <- DataRaw$Q65
DataRaw$Q66r <- DataRaw$Q66
DataRaw$Q66r[is.na(DataRaw$Q66r)] <- 0

# transfer the temperature at T2 to the same scale
for (ii in 1:length(DataRaw$Q66r)){
        if (DataRaw$Q66r[ii] ==2){
                DataRaw$Temperature_t2_r[ii] <- ((DataRaw$Q65[ii]-32)*5)/9
        }
        else DataRaw$Temperature_t2_r[ii] <- DataRaw$Q65[ii]
}


DataRaw$avgtemp_r <- rowSums(DataRaw[,c('Temperature_t1_r','Temperature_t2_r')],na.rm = T)/2
# correct the value for participatn with NA for Q65

DataRaw$avgtemp_r[is.na(DataRaw$Q65)] <- DataRaw$Temperature_t1_r[is.na(DataRaw$Q65)]  


# unify the birth year
DataRaw$birthyear <- as.integer(paste("19",as.character(round(DataRaw$Q87,2)),sep = ''))

# exclude participants
# criteria: 
# c1: average temperation is greater than 34.99
# c2: not drink or eat somethin cold or warm in 10 minutes before (eatdrink = 1)
# c3: no exercise in 60 mintues before the survey (exercise = 2)
valid.data <- subset(DataRaw,avgtemp_r > 34.99 & eatdrink == 1 & exercise == 2) # average temperature higher than 34.99 is valid

# criteria: T1 is greater than 34.99
valid.data1 <- subset(DataRaw,Temperature_t1_r > 34.99)
# criteria: T2 is greater than 34.99
valid.data2 <- subset(DataRaw,Temperature_t2_r > 34.99)
# criteria: T1 & T2 is greater than 34.99
valid.data3 <- subset(DataRaw,Temperature_t2_r > 34.99 & Temperature_t1_r > 34.99 )
# criteria: T1 or T2 or average is greater than 34.99
valid.data4 <- subset(DataRaw,Temperature_t2_r > 34.99 | Temperature_t1_r > 34.99 | avgtemp_r > 34.99)

valid.data$age <- 2014 - valid.data$birthyear # calcuate the age for each participant

## dataframe for summary data
Datasum <- valid.data[,c('age','Sex')]               # age, sex
# Datasum$num <- seq(1:nrow(Datasum))                  # add an index
# Datasum <- Datasum[,c('num','age','sex')]            # re-order the columns
#Datasum$Temperature_t1 <- valid.data$Temperature_t1  # temperature at time point 1
#Datasum$Temperature_t2 <- valid.data$Temperature_t2  # temperature at time point 2
Datasum$avgtemp <- valid.data$avgtemp_r  # average temperature

#### calculate social network index ####
## calculate the soical diveristy
# for social diversity, we re-code the types of relationship into 1 or 0
# so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were recoded by car::recoded
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
# Datasum$diversity <- SNIData$SNdiversity

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

## score and alpha for self control scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , 
                   "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
scontrolKeys2 <- list(c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                        "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" ))

# scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # in case if the score in this dataset is already reversed
scontrolAlpha <- psych::alpha(valid.data[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  # 0.8734  problematic

SelfControlScore <- psych::scoreItems(scontrolKeys2,valid.data[,scontrolNames], min = 1, max = 5)
print(SelfControlScore$alpha)

Datasum$selfcontrol <- SelfControlScore$scores # self control score

## score and alpha for perceive stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13", "stress14")
stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
stressKeys2 <- list(c("stress1" , "stress2" ,"stress3","-stress4", "-stress5", "-stress6", "-stress7", "stress8",
                      "-stress9", "-stress10","stress11", "stress12", "-stress13", "stress14"))

stressAlpha <- psych::alpha(valid.data[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.8971
stressScore <- psych::scoreItems(stressKeys2,valid.data[,stressNames],min = 1, max = 5)

Datasum$stress <-stressScore$scores

## score and alpha for attach phone ####
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )
phoneAlpha <- psych::alpha(valid.data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8698
Datasum$attachphone <- rowSums(valid.data[,phoneNames],na.rm = T)/length(phoneNames) # average score


## score and alpha for online ####
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")
onlineAlpha <- psych::alpha(valid.data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8936
#Datasum$online <- rowSums(valid.data[,onlineNames],na.rm = T)/length(onlineNames) # average score

## score and alpha for ECR ####
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
             19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
# make the key list for scoreItems
ECRKeys2 <- list(c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "-ECR11",
                    "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","-ECR20","ECR21","-ECR22",
                    "ECR23","ECR24","ECR25","-ECR26","ECR27","-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33",
                    "-ECR34","-ECR35","-ECR36"))
ECRAlpha <- psych::alpha(valid.data[,ECRNames], 
                         keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.95389
ECRScore <- psych::scoreItems(ECRKeys2,valid.data[,ECRNames], min = 1, max = 7)
Datasum$ECR <- ECRScore$scores # average score

## score and alpha for ECR Anxiety
ECRanxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyKeys  <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
ECRanxietyKeys2 <- list(c("ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "-ECR11",
                          "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18"))
ECRanxietyAlpha <- psych::alpha(valid.data[,ECRanxietyNames], 
                                keys=ECRanxietyKeys)  # calculate the alpha coefficient 
print(ECRanxietyAlpha$total)  # std. alpha 0.93678

ECRanxietyScore <- psych::scoreItems(ECRanxietyKeys2,valid.data[,ECRanxietyNames], min = 1, max = 7)
Datasum$ECRanxeity <- ECRanxietyScore$scores # average score

## score and alpha for ECR avoidance ####
ECRavoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                        "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
ECRavoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
ECRavoidanceKeys2 <- list(c("ECR19","-ECR20","ECR21","-ECR22", "ECR23","ECR24","ECR25","-ECR26","ECR27",
                            "-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33", "-ECR34","-ECR35","-ECR36"))

ECRavoidanceAlpha <- psych::alpha(valid.data[,ECRavoidanceNames], 
                                  keys=ECRavoidanceKeys)  # calculate the alpha coefficient 
print(ECRavoidanceAlpha$total)  # std. alpha 0.9451, 
ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,valid.data[,ECRavoidanceNames], min = 1, max = 7)
Datasum$ECRavoidance <- ECRavoidanceScore$scores # average score

## score and alpha for nostaglia
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostagliaKeys2 <- list(c( "-SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" ))
# nostagliaKeys <- c(1,2,3,4,5,6,7) # in case the score is already re-coded
nostagliaAlpha <- psych::alpha(valid.data[,nostagliaNames], keys=nostagliaKeys)  # calculate the alpha coefficient 
print(nostagliaAlpha$total)  # std. alpha 0.9499748

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
print(didfAlpha$total)  # print the alpha for DIDF: std.aplha: 0.9081569
didfScore <- psych::scoreItems(didfKeys2,valid.data[,didfNames], min = 1, max = 5)
Datasum$didf <- didfScore$scores

#Datasum$eot <- rowSums(valid.data[,eotNames],na.rm = T)/length(eotNames) # average score
eotfAlpha <-  psych::alpha(valid.data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotfAlpha$total)  # print the alpha for eot:std. alpha: 0.560
eotScore <- psych::scoreItems(eotKeys2,valid.data[,eotNames], min = 1, max = 5)
Datasum$eot <- eotScore$scores

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.data[,homeNames], 
                          keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9067
Datasum$attachhome <- rowSums(valid.data[,homeNames],na.rm = T)/length(homeNames)
#homeItem <- psych::scoreItems(homeKeys,valid.data[,homeNames],min = 1, max = 5) ## 


## gluctot and artgluctot (already calculated in multi-site dataset)
Datasum$glucoseplosone <- rowSums(valid.data[,c("Q89_6_1_TEXT",'Q89_7_1_TEXT','Q89_12_1_TEXT')],na.rm = T)
#Datasum$artgluctot <- rowSums(valid.data[,c("Q89_8_1_TEXT",'Q89_9_1_TEXT','Q89_13_1_TEXT')],na.rm = T)
Datasum$Site <- "ProlificAcademic"
Datasum$heightm <- valid.data$heightm
Datasum$weightkg <- valid.data$weightkg
Datasum$health <- valid.data$health
Datasum$avghumid <- NA
Datasum$Medication <- valid.data$meds
Datasum$mintemp <- NA
Datasum$Smoking <- valid.data$smoke
DatasumSort <- subset(Datasum[ , order(names(Datasum))])

# write to sum data

write.csv(DatasumSort,'summaryProflificAcd.csv',row.names = F)

##### end ####

