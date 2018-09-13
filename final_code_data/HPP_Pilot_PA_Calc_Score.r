#### Code_pre-proc_ProlificAcad_HPP####
#
### Purpose ###
# Calculating the scores for each scale for the data from prolific Academia, as reported in IJzerman et al.(2018), Human Penguin Project (HPP).
# Overview of HPP: https://osf.io/2rm5b/ 
# 
#
# Code author: Chuan-Peng Hu, PhD, 
# Affliated to: Neuroimaging Center (NIC), Johannes Gutenberg University Medical Center, 55131 Mainz, Germany;
# Email: hcp4715@gmail.com
# 
# Author   Date(d/m/y)   Notes/Changes
# ========   =========   ========
# C-P. Hu    17/01/17    add more notations
# C-P. Hu    26/01/18    add more specific criteria for excluding data
# C-P. Hu    07/08/18    Compared with data reported in the article
# C-P. Hu    09/08/18    Split the preproc file into two files: preproc & calculating score, this one is for calculating score
# C-P. Hu    14/08/18    Prepare data and code for open data
#
#
### input data ####
#
# Oringinal data: sav file: 'prolific academic corrected dataset december 2015.sav' 
#
# Cleaned data: 'Data_Raw_HPP_Pilot_PA_Share.csv' (with codebook 'Codebook_HPP_PA_Share.xlsx')
#       We thanks Jixin Yin for check the data and prepare the original code book.
# 
### output file and Variables ####
#
# output file: 'Data_Sum_HPP_Pilot_PA_Share.csv'
# 
# including following variables (reported in the article):
# Age            -- using 2015 minus the birth year.
# Sex 
# smoking        -- 
# weight         -- wightkg
# site           -- the plocation of participants
# gluctot        -- daily sugary drink consumption, Henriksen et al., 2014
# artgluctot     -- diet drinks consumption, Henriksen et al., 2014 
# health         -- health condition 
# height         -- height (in meter)
# Medication     -- medication condition
# mintemp        -- minimum temperature of the day

# anxiety        -- subscale of attachment, Fraley et al., 2000,  
# attachhome     -- attachment to home; Harris et al., 1996       
# attachphone    -- attachment to the phone                       
# avghumid       -- average humidity of the day                   
# avgtemp        -- average temperature
# avoidance      -- subscale of attachment, Fraley et al., 2000   
# stress         -- Perceived stress (Cohen & Wills, 1985)         
# nostalgia      -- (Routledge et al., 2008)                       
# selfcontrol    -- self-control, Tangney et al., 2004             

### three indeces for soical network:
# networksize    -- social network; Cohen et al., 1997
# socialdiversity-- complex social integration, social network; Cohen et al., 1997
# socialembedded -- social network; Cohen et al., 1997


### scales that not reported in IJzerman et al.(2018):
# EOT            -- alexithymia subscale; Kooiman et al., 2002
# DIDF           -- alexithymia subscale; Kooiman et al., 2002

### final Note ####
#
# This script is largely based on spss syntax file 'Syntax to Calculate Scales and Reliabilities.sps'
#
#### compare results in article and here ####

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
pkgNeeded <- (c("tidyverse","psych","car",'MBESS'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';


#### Load data #####
valid.data <- read.csv("Data_Raw_HPP_Pilot_PA_Share.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

#### start to calculate score #########
# define the output file colnames:
namePilotPA <- c("age", "anxiety", "attachhome", "attachphone", "avghumid", "avgtemp", "avoidance", "glucoseplosone",
                 "health", "heightm", "Medication", "mintemp", "networksize", "nostalgia", "selfcontrol", "Sex",
                 "Site", "Smoking", "socialdiversity", "socialembedded", "stress","weightkg")

# create an empty data frame with colnames
pilotPA <- setNames(data.frame(matrix(ncol = length(namePilotPA), nrow = nrow(valid.data))), namePilotPA)

# copy the variables that don't need calcuation
copyName <- c('age','glucoseplosone','Site','Sex','avghumid','mintemp','avgtemp','heightm','weightkg','health','Medication','Smoking')
pilotPA[,copyName] <- valid.data[,copyName]

#### calculate social network indices ####
# Three indices for social network:
#  'networksize', 'socialdiversity', 'socialembedded',
#
# the corresponding between Questions number in SPSS and colnames in sharable data
# Q10 - SNI1 (marital status)
# Q12 - SNI3 (children see or talk)
# Q14 - SNI5 (parents see or talk)
# Q16 - SNI7 (partner's parents see or talk)
# Q18 - SNI9 (relatives see or talk)
# Q20 - SNI11 (friends see or talk)
# Q22 - SNI13 (religious grou see or talk)
# Q24 - SNI15 (students or teachers, see or talk)
# Q26 - SNI17 (ppl supervised by you, see or talk)
# Q27 - SNI18 (co-worker or supervisoer, see or talk), SNI 17, 18 need to combined when calculating diversity
# Q28 - SNI19 (neigbors, see or talk)
# Q30 - SNI21 (volunteer worker' see or talk)
# Q33_2_1_1_text - SNI28 (group 1 members, see or talk)
# Q33_2_1_2_text - SNI29 (group 2 members, see or talk)
# Q33_2_1_3_text - SNI30 (group 3 members, see or talk)
# Q33_2_1_4_text - SNI31 (group 4 members, see or talk)
# Q33_2_1_5_text - SNI32 (group 5 members, see or talk)
#
## calculate the social diveristy
# social diversity sum up different relationship type, therefore, each relationship was binarized.
# for social diversity, we re-code the types of relationship into 1 or 0
# so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were recoded by function car::recoded
snDivNames  <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
                "SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32")    # colnames of the extra groups

# get data for diversity
snDivData <- setNames(data.frame(matrix(ncol = length(snDivNames), nrow = nrow(valid.data))), snDivNames)

# recode Q10 (spouse): 1-> 1; else ->0
snDivData$SNI1_r <- car::recode(valid.data$SNI1,"1= 1; else = 0")

# re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
snDivData[,snDivNames] <- apply(valid.data[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 
colnames(snDivData[,snDivNames]) <- paste(snDivNames,"div",  sep = "_")    # add suffix to the colnames

# combine the diversity of work (SNI17, SNI18)
snDivData$SNIwork   <- snDivData$SNI17 + snDivData$SNI18    
snDivData$SNIwork_r <- car::recode(snDivData$SNIwork,"0 = 0;1:10 = 1")

# re-code extra groups, 0/NA --> 0; more than 0 --> 1
extrDivData <- valid.data[,extrDivName]  # Get extra data

# sum the other groups
extrDivData$sum <- rowSums(extrDivData)
snDivData$extrDiv_r <- car::recode(extrDivData$sum,"0 = 0; NA = 0; else = 1")  # recode

# add social diversity with other groups
snDivNames_r <- c("SNI1_r","SNI3","SNI5","SNI7","SNI9","SNI11","SNI13","SNI15","SNIwork_r",
                  "SNI19","SNI21","extrDiv_r")

# get the social diveristy score
snDivData$SNdiversity <- rowSums(snDivData[,snDivNames_r])
pilotPA$socialdiversity <- snDivData$SNdiversity  # assign it to the output file

# Social Network size
# This index is the number of people in social network
#
# Q10 - SNI1 (marital status): 1 or 0
# Q12 ~ Q30  - SNI3 ~ SNI21 (odd numbers): as indicated
# for volunteer or other social group,0, 1-6, or 7
# Q33_2_1_1_text - SNI28 (group 1 members, see or talk)
# Q33_2_1_2_text - SNI29 (group 2 members, see or talk)
# Q33_2_1_3_text - SNI30 (group 3 members, see or talk)
# Q33_2_1_4_text - SNI31 (group 4 members, see or talk)
# Q33_2_1_5_text - SNI32 (group 5 members, see or talk)
# NOTE: In our experience, individuals sometimes interpret the SNI item inquiring about the number of "other group" 
# members with whom they interact at least once every 2 weeks more broadly than we intended, with some respondents 
# reporting up to 100 or more fellow group-members. To ensure that social network size scores are not artificially inflated by 
# individuals reporting large group memberships, we recommend recoding the variable so that all values over 6 are given a 
# score of 7, thus keeping it consistent with all other quantitative SNI items.
#
# get the social network data that do not need to recode
# Data        <- valid.data[,SNINames]

# the colnames for the columns that needed to be recoded for calculating network size
snSizeNames <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21")
snSizeData <- valid.data[,snSizeNames]  # get the data
snSizeData[is.na(snSizeData)] <- 0      # missing data equal to zero

# recode data
snSizeData$SNI1_r <- car::recode(valid.data$SNI1,"1= 1; else = 0")

snSizeData[,c("SNI28","SNI29","SNI30","SNI31",'SNI32')] <- apply(valid.data[,c("SNI28","SNI29","SNI30","SNI31",'SNI32')],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0;1 = 1; 2= 2; 3= 3; 4= 4;5= 5; 6 = 6; else = 7"); x}) 

# cobmine data
snSizeNames_r <- c("SNI1_r","SNI3", "SNI5", "SNI7", "SNI9" , "SNI11", "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21",
                             "SNI28","SNI29","SNI30","SNI31","SNI32")

snSizeData$snSize     <- rowSums(snSizeData[,snSizeNames_r],na.rm=TRUE) # calculate the network size score
pilotPA$networksize <- snSizeData$snSize # calculate the network size score

## number of embedded networks
## family: SNI1_r, SNI3,SNI5,SNI7,SNI9 (total > 4);
## friends: SNI11 (>4);
## Church: SNI13 (>4);
## Students/school: SNI 15 (>4)
## Work: SNI17 + SNI 18 >4
## neighbor: SNI19 >4
## volunteer SNI21 >4
## other groups: totoal > 4
# recode based on the above rules:
snEmbedNames <- c('family') # generate a dataframe for embed index.
snEmbedData <- setNames(data.frame(matrix(ncol = length(snEmbedNames), nrow = nrow(valid.data))), snEmbedNames)

# calculated the embeded index for each network
snEmbedData$family   <- rowSums(snSizeData[,c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9")])
snEmbedData$family_r <- car::recode(snEmbedData$family,"1:4 = 0; 0 = 0; else = 1")
snEmbedData$friends_r <- car::recode(snSizeData$SNI11,"0:4 = 0; else = 1")
snEmbedData$Church_r <- car::recode(snSizeData$SNI13,"0:4 = 0; else = 1")
snEmbedData$StuSchool_r  <- car::recode(snSizeData$SNI15,"0:4 = 0; else = 1")
snEmbedData$work     <- snSizeData$SNI17 + snSizeData$SNI18 
snEmbedData$work_r   <- car::recode(snEmbedData$work,"0:4 = 0; else = 1")
snEmbedData$neighbor_r <- car::recode(snSizeData$SNI19,"0:4 = 0; else = 1")
snEmbedData$volun_r    <- car::recode(snEmbedData$SNI21,"0:4 = 0;else = 1")
snEmbedData$extra      <- rowSums(snSizeData[,c("SNI28","SNI29","SNI30","SNI31","SNI32")])
snEmbedData$extra_r    <- car::recode(snEmbedData$extra,"0:4 = 0; else = 1")

# calculate the social embedded score
snEmbedData$socEmbd <- rowSums(snEmbedData[,c("family_r","friends_r","Church_r","StuSchool_r","work_r","neighbor_r","volun_r","extra_r")])
pilotPA$socialembedded <- snEmbedData$socEmbd  # assign the value to output file

# remove the temporary varialbes the no longer needed
rm(snDivData,snSizeData,snEmbedData,snDivNames,snDivNames_r,snEmbedNames,snSizeNames,snSizeNames_r,extrDivData)
#### below is the calculating of scale score and aphla coefficient for each scale ####

## score of selfcontrol ####
# always pay attention to this scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , 
                   "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
# scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # in case if the score in this dataset is already reversed
scontrolKeys2 <- c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                        "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" )
scontrolAlpha <- psych::alpha(valid.data[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  # 0.879 

# McDonald's omega
scontrolOmega <- psych::omega(valid.data[,scontrolNames])
print(scontrolOmega$omega_h) # 0.6127

SelfControlScore <- psych::scoreItems(scontrolKeys2,valid.data[,scontrolNames], totals = T, min = 1, max = 5)
pilotPA$selfcontrol <- SelfControlScore$scores # self control score

## score and alpha for perceive stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13","stress14")
stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,-12,13,14) # original key for reverse coding
stressKeys2 <- c("stress1" , "stress2" ,"stress3","-stress4", "-stress5", "-stress6", "-stress7", 
                      "stress8","-stress9", "-stress10","stress11", "-stress12", "stress13",'stress14')

stressAlpha <- psych::alpha(valid.data[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.9107

# McDonald's omega
stressOmega <- psych::omega(valid.data[,stressNames]) # warnings a loading great than 1 was detected
print(stressOmega$omega_h) # 0.68

stressScore <- psych::scoreItems(stressKeys2,valid.data[,stressNames],totals = T, min = 1, max = 5)
pilotPA$stress <-stressScore$scores

## score and alpha for attach phone ####
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )
phoneAlpha <- psych::alpha(valid.data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8698
phoneAlpha2 <- MBESS::ci.reliability(valid.data[,phoneNames],type = 'alpha', conf.level = .95,interval.type = 'bca', B = 1000)

# McDonald's omega
phoneOmega <- psych::omega(valid.data[,phoneNames])     # warning: an ultra-Heywood case 1 was detected
#phoneOmega2 <- ci.reliability(valid.data[,phoneNames],type = 'omega', conf.level = .95,interval.type = 'bca', B = 1000)
print(phoneOmega$omega_h) # 0.68939

# Datasum$attachphone <- rowSums(valid.data[,phoneNames],na.rm = T) # sum score
pilotPA$attachphone <- rowSums(valid.data[,phoneNames],na.rm = T) # sum score

## score and alpha for online ####
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")
onlineAlpha <- psych::alpha(valid.data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8936

# also McDonald's omega
onlineOmega <- psych::omega(valid.data[,onlineNames]) 
print(onlineOmega$omega_h) # 0.6933

pilotPA$onlineid <- rowSums(valid.data[,onlineNames],na.rm = T)/length(onlineNames)

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
print(ECRAlpha$total)  # std. alpha 0.95382
ECROmega <- psych::omega(valid.data[,ECRNames]) # warnings
print(ECROmega$omega_h) # 0.672

## score and alpha for ECR Anxiety
ECRanxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyKeys  <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
ECRanxietyKeys2 <- c("ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "-ECR9", "ECR10", "-ECR11",
                          "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyAlpha <- psych::alpha(valid.data[,ECRanxietyNames], 
                                keys=ECRanxietyKeys)  # calculate the alpha coefficient 
print(ECRanxietyAlpha$total)  # std. alpha 0.9371
# also McDonald's omega
ECRanxietyOmega <- psych::omega(valid.data[,ECRanxietyNames]) 
print(ECRanxietyOmega$omega_h) # 0.6846

ECRanxietyScore <- psych::scoreItems(ECRanxietyKeys2,valid.data[,ECRanxietyNames], totals = T, min = 1, max = 7) 
#Datasum$ECRanxeity <- ECRanxietyScore$scores # average score
pilotPA$anxiety <-  ECRanxietyScore$scores   # sum score

## score and alpha for ECR avoidance ####
ECRavoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                        "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
ECRavoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
ECRavoidanceKeys2 <- c("ECR19","-ECR20","ECR21","-ECR22", "ECR23","ECR24","ECR25","-ECR26","-ECR27",
                            "-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33", "-ECR34","-ECR35","-ECR36")

ECRavoidanceAlpha <- psych::alpha(valid.data[,ECRavoidanceNames], 
                                  keys=ECRavoidanceKeys)  # calculate the alpha coefficient 
print(ECRavoidanceAlpha$total)  # std. alpha 0.9459, 

# also McDonald's omega
ECRavoidanceOmega <- psych::omega(valid.data[,ECRavoidanceNames]) 
print(ECRavoidanceOmega$omega_h) # 0.784

ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,valid.data[,ECRavoidanceNames], totals = T, min = 1, max = 7)
pilotPA$avoidance <- ECRavoidanceScore$scores # sum score

## score and alpha for nostalgia, with Q83 (SNS1)
nostalgiaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7")
nostalgiaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostalgiaKeys2 <- c( "-SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7")
# nostalgiaKeys <- c(1,2,3,4,5,6,7) # in case the score is already re-coded
nostalgiaAlpha <- psych::alpha(valid.data[,nostalgiaNames], keys=nostalgiaKeys)  # calculate the alpha coefficient 
print(nostalgiaAlpha$total)  # 0.9152, std. alpha 0.9527

# McDonald's omega
nostalgiaOmega <- psych::omega(valid.data[,nostalgiaNames]) 
print(nostalgiaOmega$omega_h) # 0.8776

nostalgiaScore <- psych::scoreItems(nostalgiaKeys2,valid.data[,nostalgiaNames], totals = T, min = 1, max = 7) ## 
pilotPA$nostalgia <- nostalgiaScore$scores

# score and alpha for nostalgia, without Q83(SNS1)
nostalgiaNames_2 <- c( "SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaKeys_2 <- c(1,2,3,4,5,6) # reverse coded as negative
nostalgiaKeys2_2 <- c("SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaAlpha2 <- psych::alpha(valid.data[,nostalgiaNames_2], keys=nostalgiaKeys_2)  # calculate the alpha coefficient 
print(nostalgiaAlpha2$total)  # 0.9079, std. alpha 0.95425

# McDonald's omega
nostalgiaOmega2 <- psych::omega(valid.data[,nostalgiaNames_2])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(nostalgiaOmega2$omega_h) # 0.871

nostalgiaScore2 <- psych::scoreItems(nostalgiaKeys2_2,valid.data[,nostalgiaNames_2], totals = T, min = 1, max = 7) ## 
pilotPA$nostalgia2 <- nostalgiaScore2$scores

# remove temporary variables to accelerate the processing
rm(nostalgiaNames,nostalgiaKeys,nostalgiaKeys2,nostalgiaAlpha)

## score and alpha coefficient for ALEX ####
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys2 <- c("ALEX1","ALEX2","ALEX3","-ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
#didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11) # in case the score is already re-coded

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys2 <- c("-ALEX12","ALEX13","-ALEX14","ALEX15" ,"-ALEX16")
# eotKeys <- c(1,2,3,4,5) # in case the score is already re-coded

#Datasum$didf <- rowSums(valid.data[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.data[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF: std.aplha: 0.9081569
didfScore <- psych::scoreItems(didfKeys2,valid.data[,didfNames], min = 1, max = 5)
pilotPA$didf <- didfScore$scores

# McDonald's omega
didfOmega <- psych::omega(valid.data[,didfNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(didfOmega$omega_h) # 0.8447


#Datasum$eot <- rowSums(valid.data[,eotNames],na.rm = T)/length(eotNames) # average score
eotAlpha <-  psych::alpha(valid.data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotAlpha$total)  # print the alpha for eot:std. alpha: 0.560
eotScore <- psych::scoreItems(eotKeys2,valid.data[,eotNames], min = 1, max = 5)
pilotPA$eot <- eotScore$scores

# McDonald's omega
eotOmega <- psych::omega(valid.data[,eotNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(eotOmega$omega_h) # 0.5074


## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.data[,homeNames], 
                          keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9061

# McDonald's omega
homeOmega <- psych::omega(valid.data[,homeNames])  # warnings: An ultra-Heywook case;
print(homeOmega$omega_h) # 0.688

pilotPA$attachhome <- rowSums(valid.data[,homeNames],na.rm = T)/length(homeNames)

# re-order the columns
pilotPA_reord <- pilotPA[ , order(names(pilotPA))]

write.csv(pilotPA_reord,'Data_Sum_HPP_Pilot_PA_Share.csv',row.names = F)

##### end ####