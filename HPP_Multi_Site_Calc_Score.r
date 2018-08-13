#### Code_pre-proc_HPP_multi-site data####
#
### Purpose ###
# Pre-processing the data from pilot data from multi-site dataset as reported in IJzerman et al.(2018), Human Penguin Project (HPP).
# Overview of HPP: https://osf.io/2rm5b/ 
# 
#
# Code author: Chuan-Peng Hu, PhD, 
# Affliated to: Neuroimaging Center (NIC), Johannes Gutenberg University Medical Center, 55131 Mainz, Germany;
# Email: hcp4715@gmail.com
# 
# Author      Date       Notes/Changes
# ========   =========   ========
# C-P. Hu    27/01/18    add more notations
#
#
### input data ####
#
# Oringinal data: sav file: 'penguin v1d_7f.sav' 
#
# Revised data: 'Data_Raw_HPP_Multi_Site_Share.csv' (with codebook 'Codebook_HPP_mul_sites_0612.xlsx')
#       We thanks Jixin Yin for check the data and prepare the code book.
# 
### output file and Variables ####
#
# output file: 'Data_Sum_HPP_Multisite_Share.csv'
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
# valid data    (excluded 48) (exclude 8, 92 valid)
# selfcontrol                   
# stress                        
# attachphone                   
# onlineid                      
# ECR-total                     
# ECR-anxiety                   
# ECR-avoidance                 
# nostalgia                     
# Alex-didf                     
# Alex-eot                      
# attachhome                    
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
pkgNeeded <- (c("tidyverse",'lattice','stargazer',"summarytools","psych","car"))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# read data
valid.data <- read.csv("Data_Raw_HPP_Multi_Site_NO_Share.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# define the output file colnames:
# colnames used for comparing with reported data
nameMultSite <- c('age','romantic','sex','sexpref','monogamous',
                 'heightm','weightkg','health','meds',
                 'gluctot',"artgluctot","smoke","cigs", "eatdrink","exercise", 
                 'avgtemp','AvgHumidity','mintemp','endtime',
                 'language', "langfamily","Site",'DEQ','longitude')

# create an empty data frame with colnames
sumMultSite <- valid.data[,nameMultSite]

describeMulSite1 <- valid.data %>%
        select(Site,age,romantic,sex,heightm,weightkg,sexpref,monogamous,avgtemp,mintemp, 
               AvgHumidity,artgluctot,gluctot, Temperature_t1,Temperature_t2,health) %>%
        group_by(Site) %>%
        summarise(N = length(avgtemp),   # sample size for each site
                  age_m = mean(age,na.rm = T), age_sd = sd(age,na.rm = T),age_NA = sum(is.na(age)),
                  romantic_yes = sum(romantic ==1,na.rm = T)/length(romantic),
                  romantic_no  = sum(romantic ==2,na.rm = T)/length(romantic),
                  romantic_NA = sum(is.na(romantic))/length(romantic),
                  male = sum(sex ==1,na.rm = T)/length(sex),female = sum(sex ==2,na.rm = T)/length(sex),
                  sex_other = sum(sex ==3,na.rm = T)/length(sex),sex_NA = sum(is.na(sex))/length(sex),
                  height_m = mean(heightm,na.rm = T),height_sd = sd(heightm,na.rm = T),
                  weight_m = mean(weightkg,na.rm = T),weight_sd = sd(weightkg,na.rm = T),
                  hetero   = sum(sexpref == 1,na.rm = T)/length(sexpref),
                  homo     = sum(sexpref == 2,na.rm = T)/length(sexpref),
                  bi       = sum(sexpref == 3,na.rm = T)/length(sexpref),
                  othersexpref= sum(sexpref == 4,na.rm = T)/length(sexpref),
                  sexpref_NA  = sum(is.na(sexpref))/length(sexpref),
                  monog_m = mean(monogamous,na.rm = T),monog_sd = sd(monogamous,na.rm = T),monog_Na = sum(is.na(monogamous)),
                  mintemp_m = mean(mintemp,na.rm = T), mintemp_sd = sd(mintemp,na.rm = T),
                  AvgHum = mean(AvgHumidity,na.rm = T), AvgHum_sd = sd(AvgHumidity,na.rm = T),
                  artgluctot_m = mean(artgluctot,na.rm = T),artgluctot_sd = sd(artgluctot,na.rm = T),
                  gluctot_m = mean(gluctot,na.rm = T),gluctot_sd = sd(gluctot,na.rm = T),
                  health_m = mean(health,na.rm = T), health_sd = sd(health,na.rm = T),
                  temp_T1_m = mean(Temperature_t1,na.rm = T),temp_T1_sd = sd(Temperature_t1,na.rm = T),
                  temp_T2_m = mean(Temperature_t2,na.rm = T),temp_T2_sd = sd(Temperature_t2,na.rm = T))

#### calculate social network index ####
## calculate the social diveristy
# social diversity sum up different relationship type, therefore, each relationship was binarized.
# for social diversity, we re-code the types of relationship into 1 or 0
# so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were recoded by function car::recoded
snDivNames  <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
                 "SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32")    # colnames of the extra groups

# get data for diversity
snDivData <- setNames(data.frame(matrix(ncol = length(snDivNames), nrow = nrow(valid.data))), snDivNames)
#snDivData <- valid.data[,snDivNames]
# snDivData <- valid.data[,snDivNames]

# recode Q10 (spouse): 1-> 1; else ->0
snDivData$SNI1_r <- car::recode(valid.data$SNI1,"1= 1; else = 0")

# re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
snDivData[,snDivNames] <- apply(valid.data[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 
# socDivData_r   <- data.frame(socDivData_r)
colnames(snDivData[,snDivNames]) <- paste(snDivNames,"div",  sep = "_")    # add suffix to the colnames

snDivData$SNIwork   <- snDivData$SNI17 + snDivData$SNI18    # combine the diversity of work (SNI17, SNI18)
snDivData$SNIwork_r <- car::recode(snDivData$SNIwork,"0 = 0;1:10 = 1")

# # re-code extra groups, 0/NA --> 0; more than 0 --> 1
extrDivData <- valid.data[,extrDivName]  # Get extra data
# sum the other groups
extrDivData$sum <- rowSums(extrDivData)
snDivData$extrDiv_r <- car::recode(extrDivData$sum,"0 = 0; NA = 0; else = 1")  # recode

# add social diversity with other groups
snDivNames_r <- c("SNI1_r","SNI3","SNI5","SNI7","SNI9","SNI11","SNI13","SNI15","SNIwork_r",
                  "SNI19","SNI21","extrDiv_r")

# get the social diveristy score
snDivData$SNdiversity <- rowSums(snDivData[,snDivNames_r])
sumMultSite$socialdiversity <- snDivData$SNdiversity  # assign it to the output file

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
sumMultSite$networksize <- snSizeData$snSize # calculate the network size score

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
snEmbedData$family      <- rowSums(snSizeData[,c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9")])
snEmbedData$family_r    <- car::recode(snEmbedData$family,"1:4 = 0; 0 = 0; else = 1")
snEmbedData$friends_r   <- car::recode(snSizeData$SNI11,"0:4 = 0; else = 1")
snEmbedData$Church_r    <- car::recode(snSizeData$SNI13,"0:4 = 0; else = 1")
snEmbedData$StuSchool_r <- car::recode(snSizeData$SNI15,"0:4 = 0; else = 1")
snEmbedData$work        <- snSizeData$SNI17 + snSizeData$SNI18 
snEmbedData$work_r      <- car::recode(snEmbedData$work,"0:4 = 0; else = 1")
snEmbedData$neighbor_r  <- car::recode(snSizeData$SNI19,"0:4 = 0; else = 1")
snEmbedData$volun_r     <- car::recode(snEmbedData$SNI21,"0:4 = 0;else = 1")
snEmbedData$extra       <- rowSums(snSizeData[,c("SNI28","SNI29","SNI30","SNI31","SNI32")])
snEmbedData$extra_r     <- car::recode(snEmbedData$extra,"0:4 = 0; else = 1")

# calculate the social embedded score
snEmbedData$socEmbd <- rowSums(snEmbedData[,c("family_r","friends_r","Church_r","StuSchool_r","work_r","neighbor_r","volun_r","extra_r")])
sumMultSite$socialembedded <- snEmbedData$socEmbd  # assign the value to output file

# remove the temporary varialbes the no longer needed
rm(snDivData,snSizeData,snEmbedData,snDivNames,snDivNames_r,snEmbedNames,snSizeNames,snSizeNames_r,extrDivData)

#### below is the calculating of scale score and aphla coefficient for each scale ####

## score and alpha for self control scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" ,
                   "scontrol6" , "scontrol7","scontrol8", "scontrol9", "scontrol10",
                   "scontrol11" ,"scontrol12", "scontrol13" )
# scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)   #  this dataset is already reverse coded 
scontrolKeys2 <- c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                        "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" )

scontrolAlpha <- psych::alpha(valid.data[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  #

# McDonald's omega
scontrolOmega <- psych::omega(valid.data[,scontrolNames])
print(scontrolOmega$omega_h) # 0.604

SelfControlScore <- psych::scoreItems(scontrolNames,valid.data[,scontrolNames], totals = T, min = 1, max = 5)

sumMultSite$scontrol <- SelfControlScore$scores # self control score

# Reliability for each site for self control
siteName <- unique(valid.data$Site)
sitesReliability <- data.frame(sites = siteName, Scontrol_alpha = NA, Scontrol_omega = NA)

sitesReliability$sites <- as.character(sitesReliability$sites)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,scontrolNames]
        tmpAlpha <- psych::alpha(tmpdf, keys=scontrolKeys) 
        tmpOmega <- psych::omega(tmpdf)
        sitesReliability$Scontrol_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$Scontrol_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose the omega_h as omega index
}

## score and alpha for perceive stress
# recode the data from poland
valid.data_r <- valid.data
rev_names <- c("stress4", "stress5", "stress6", "stress7","stress9", "stress10", "stress13")
#valid.data_r[valid.data,rev_names]
valid.data_r[valid.data_r$Site == 'Poland',rev_names] <- 6 - valid.data_r[valid.data_r$Site == 'Poland',rev_names]

stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", 
                 "stress9", "stress10","stress11", "stress12", "stress13", "stress14")
#stressKeys_r <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # for current dataset

#stressKeys2_r <- c("stress1" , "stress2" ,"stress3","-stress4", "-stress5", "-stress6", "-stress7", "stress8",
#                   "-stress9", "-stress10","stress11", "stress12", "-stress13", "stress14")
# Alpha
stressAlpha <- psych::alpha(valid.data_r[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.83586  Not right

# McDonald's omega
stressOmega <- psych::omega(valid.data_r[,stressNames])
print(stressOmega$omega_h) # 0.5188

stressScore <- psych::scoreItems(stressNames,valid.data_r[,stressNames], totals = T, min = 1, max = 5)
sumMultSite$stress <- stressScore$scores

# special case for poland data (not reverse coding)
#stressscore_pol <- psych::scoreItems(stressNames,valid.data_r[valid.data$Site == 'Poland',stressNames], totals = T, min = 1, max = 5)
#sumMultSite$stress[sumMultSite$Site == 'Poland'] <- stressscore_pol$scores

# alpha for each site
siteName <- unique(valid.data$Site)
siteName_stress <- siteName[!siteName %in% c('Southampton')] # item 7 of Southampton is invariant, not able to calculate alph
for (i in siteName_stress){
        tmpdf <- valid.data_r[valid.data_r$Site == i,stressNames]
        tmpAlpha <- psych::alpha(tmpdf,keys = stressKeys)
        tmpOmega <- psych::omega(tmpdf)
        sitesReliability$stress_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$stress_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose the Standard alpha
}

rm(valid.data_r) # remove the intermediate variables
##
#stressAlpha_pol <- psych::alpha(valid.data_r[valid.data_r$Site == 'Poland',stressNames], keys = stressKeys)  # calculate the alpha coefficient 
#stressAlpha_pol$total
#sitesReliability$stress_alpha[sitesReliability$sites == 'Poland'] <- stressAlpha_pol$total[2]

## score and alpha for attach phone
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )

phoneAlpha <- psych::alpha(valid.data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8868

# McDonald's omega
phoneOmega <- psych::omega(valid.data[,phoneNames])
print(phoneOmega$omega_h) # 0.8049

attachphoneScore <- psych::scoreItems(phoneNames,valid.data[,phoneNames], min = 1, max = 5) # mean score
sumMultSite$attachphone <- attachphoneScore$scores  # mean score

# alpha for each site
siteName <- unique(valid.data$Site)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,phoneNames]
        tmpAlpha <- psych::alpha(tmpdf, 
                                 keys= c(1,2,3,4,5,6,7,8,9)) 
        tmpOmega <- psych::omega(tmpdf)
        sitesReliability$phone_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$phone_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for online
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")

onlineAlpha <- psych::alpha(valid.data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8977

# McDonald's omega
onlineOmega <- psych::omega(valid.data[,onlineNames])
print(onlineOmega$omega_h) # 0.725

onlineScore <- psych::scoreItems(onlineNames,valid.data[,onlineNames], min = 1, max = 5) # mean score
sumMultSite$onlineid <- onlineScore$scores  # mean score

# alpha for each site
siteName <- unique(valid.data$Site)
#options(warn=1)
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,onlineNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= c(1,2,3,4,5,6,7,8,9,10,11))
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$online_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$online_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for ECR
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
# ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
#              19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
             19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36) # reverse coded as negative

ECRAlpha <- psych::alpha(valid.data[,ECRNames], 
                            keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.776, instead of 0.932

# McDonald's omega
ECROmega <- psych::omega(valid.data[,ECRNames])
print(ECROmega$omega_h) # 0.5126

#sumMultSite$ECR <- rowSums(valid.data[,ECRNames],na.rm = T)/length(ECRNames) # average score

# alpha for each site
siteName <- unique(valid.data$Site)
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,ECRNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= ECRKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$ECR_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$ECR_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for ECR Anxiety
anxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
# anxietyKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
anxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) 

anxietyAlpha <- psych::alpha(valid.data[,anxietyNames], 
                         keys=anxietyKeys)  # calculate the alpha coefficient 
print(anxietyAlpha$total)  # std. alpha 0.876, instead of 0.92

# McDonald's omega
anxietyOmega <- psych::omega(valid.data[,anxietyNames])
print(anxietyOmega$omega_h) # 0.7755

sumMultSite$anxiety <- rowSums(valid.data[,anxietyNames],na.rm = T)/length(anxietyNames) # average score

# standardize the anxiety score for each site
sumMultSite <- ddply(sumMultSite,c('Site'),transform,anxiety_r = scale(anxiety))

# alpha for each site
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,anxietyNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= anxietyKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$anxiety_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$anxiety_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for ECR avoidance
avoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                   "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
# avoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
avoidanceKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

avoidanceAlpha <- psych::alpha(valid.data[,avoidanceNames], 
                             keys=avoidanceKeys)  # calculate the alpha coefficient 
print(avoidanceAlpha$total)  # std. alpha 0.838, instead of 0.916

# McDonald's omega
avoidanceOmega <- psych::omega(valid.data[,avoidanceNames])
print(avoidanceOmega$omega_h) # 0.7906

sumMultSite$avoidance <- rowSums(valid.data[,avoidanceNames],na.rm = T)/length(avoidanceNames) # average score

# standardize for each group
sumMultSite <- ddply(sumMultSite,c('Site'),transform,avoidance_r = scale(avoidance))

# alpha for each site
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,avoidanceNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= avoidanceKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$avoidance_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$avoidance_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for nostalgia, without Q83(SNS1)
nostalgiaNames <- c( "SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaKeys <- c(1,2,3,4,5,6) # reverse coded as negative
nostalgiaKeys2 <- c("SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaAlpha <- psych::alpha(valid.data[,nostalgiaNames], keys=nostalgiaKeys)  # calculate the alpha coefficient 
print(nostalgiaAlpha$total)  # 0.8823, std. alpha 0.9226

# McDonald's omega
nostalgiaOmega <- psych::omega(valid.data[,nostalgiaNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(nostalgiaOmega$omega_h) # 0.8177

nostalgiaScore <- psych::scoreItems(nostalgiaKeys2,valid.data[,nostalgiaNames], totals = T, min = 1, max = 7) ## 
sumMultSite$nostalgia <- nostalgiaScore$scores

# score and alpha for nostalgia (with Q83/SNS1)
#nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
# nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
#nostagliaKeys <- c(1,2,3,4,5,6,7)
#nostagliaAlpha <- psych::alpha(valid.data[,nostagliaNames], 
#                               keys=nostagliaKeys)  # calculate the alpha coefficient 
#print(nostagliaAlpha$total)  # std. alpha 0.765, instead of 0.92
#nostagliaItem <- psych::scoreItems(nostagliaKeys,valid.data[,nostagliaNames],min = 1, max = 7) ## 

# alpha for each site
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,nostalgiaNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= nostalgiaKeys)
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$nostalgia_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$nostalgia_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha coefficient for ALEX
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
#didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11)

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
# eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys <- c(1,2,3,4,5)

# sumMultSite$didf <- rowSums(valid.data[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.data[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF
# McDonald's omega
didfOmega <- psych::omega(valid.data[,didfNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(didfOmega$omega_h) # 0.7794

didfScore <- psych::scoreItems(didfNames,valid.data[,didfNames], min = 1, max = 5)
sumMultSite$didf <- didfScore$scores

#sumMultSite$eot <- rowSums(valid.data[,eotNames],na.rm = T)/length(eotNames) # average score
eotAlpha <-  psych::alpha(valid.data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotAlpha$total)  # print the alpha for eot:0.51
# McDonald's omega
eotOmega <- psych::omega(valid.data[,eotNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(eotOmega$omega_h) # 0.354
eotScore <- psych::scoreItems(eotNames,valid.data[,eotNames], min = 1, max = 5)
sumMultSite$eot <- eotScore$scores

# alpha for each site for didf
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,didfNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= didfKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$didf_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$didf_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}

for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,eotNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= eotKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$eot_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$eot_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.data[,homeNames], 
                               keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9049, instead of 0.901

# McDonald's omega
homeOmega <- psych::omega(valid.data[,homeNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(homeOmega$omega_h) # 0.759


homeScore <- psych::scoreItems(homeKeys,valid.data[,homeNames],min = 1, max = 5) ## 
sumMultSite$attachhome <- homeScore$scores

warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,homeNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= homeKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$home_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$home_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

## score and alpha for KAMF
# recode to 1 - 8

kamfNames <- c("KAMF1" ,"KAMF2","KAMF3","KAMF4","KAMF5","KAMF6","KAMF7")
kamfData <- valid.data[,kamfNames]
summary(kamfData)
kamfData$KAMF1_r <-kamfData$KAMF1*1.75 - 0.75
kamfData$KAMF3_r <-kamfData$KAMF3*1.166 - 0.166
kamfNames_r <- c("KAMF1_r" ,"KAMF2","KAMF3_r","KAMF4","KAMF5","KAMF6","KAMF7")
kamfKeys <- c(1,2,3,4,5,6,7) # reverse coded as negative

kamfAlpha <- psych::alpha(kamfData[,kamfNames], keys=kamfKeys)  # calculate the alpha coefficient for not re-coded
print(kamfAlpha$total) # std.alpha:0.867

# McDonald's omega
kamfOmega <- psych::omega(kamfData[,kamfNames])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(kamfOmega$omega_h) # 0.769

kamfAlpha_r <- psych::alpha(kamfData[,kamfNames_r], keys=kamfKeys)  # calculate the alpha coefficient 
print(kamfAlpha_r$total)  # std. alpha 0.8672, instead of 0.901
# McDonald's omega
kamfOmega_r <- psych::omega(kamfData[,kamfNames_r])  # warnings: a loading greater than abs(1) was detected; An ultra-Heywook case;
print(kamfOmega_r$omega_h) # 0.769

kamfScore <- psych::scoreItems(kamfKeys,valid.data[,kamfNames],min = 1, max = 5) ## 
sumMultSite$kamf <- kamfScore$scores

# 
warnSite <- data.frame(siteName)
for (i in siteName){
        tmpdf <- valid.data[valid.data$Site == i,kamfNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= kamfKeys) 
        tryCatch(tmpOmega <- psych::omega(tmpdf),
                 warning = function(w){
                         print(w)
                         warnSite$Wrong[warnSite$siteName == i] <<- 1
                         
                 })
        sitesReliability$kampf_alpha[sitesReliability$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$kampf_omega[sitesReliability$sites == i] <- as.numeric(tmpOmega$omega_h) # chose Omega_H
}

# write the data
sumMultSite_reord <- sumMultSite[order(sumMultSite$Site),order(names(sumMultSite))]
sitesReliability_reord <- sitesReliability[order(sitesReliability$sites),]

# calculate the descriptives of each scales
describeMulSite2 <- sumMultSite %>%
        select(Site,scontrol,stress,attachphone,onlineid,anxiety,avoidance,nostalgia,didf, 
               eot,attachhome,networksize, socialdiversity, socialembedded) %>%
        group_by(Site) %>%
        summarise(scontrol_m = mean(scontrol,na.rm = T), scontrol_sd = sd(scontrol,na.rm = T),
                  stress_m = mean(stress,na.rm = T),stress_sd = sd(stress,na.rm = T),
                  attachphone_m = mean(attachphone,na.rm = T),attachphone_sd = sd(attachphone,na.rm = T),
                  onlineid_m = mean(onlineid,na.rm = T),onlineid_sd = sd(onlineid,na.rm = T),
                  anxiety_m = mean(anxiety,na.rm = T), mintemp_sd = sd(anxiety,na.rm = T),
                  avoidance_m = mean(avoidance,na.rm = T), avoidance_sd = sd(avoidance,na.rm = T),
                  nostalgia_m = mean(nostalgia,na.rm = T),nostalgia_sd = sd(nostalgia,na.rm = T),
                  didf_m = mean(didf,na.rm = T),didf_sd = sd(didf,na.rm = T),
                  eot_m = mean(eot,na.rm = T), eot_sd = sd(eot,na.rm = T),
                  attachhome_m = mean(attachhome,na.rm = T),attachhome_sd = sd(attachhome,na.rm = T),
                  networksize_m = mean(networksize,na.rm = T),networksize_sd = sd(networksize,na.rm = T),
                  socialdiversity_m = mean(socialdiversity,na.rm = T),socialdiversity_sd = sd(socialdiversity,na.rm = T),
                  socialembedded_m = mean(socialembedded,na.rm = T),socialembedded_sd = sd(socialembedded,na.rm = T))

describeMulSite <- merge(describeMulSite1,describeMulSite2,by = 'Site')

write.csv(sumMultSite_reord,'Data_Sum_HPP_Multi_Site_No_Share.csv',row.names = F)
write.csv(sitesReliability_reord,'Reliability_HPP_Multi_Site_Share.csv',row.names = F)
write.csv(describeMulSite,'Descriptives_HPP_Multi_Site_Share.csv',row.names = F)

##### end ###=======

