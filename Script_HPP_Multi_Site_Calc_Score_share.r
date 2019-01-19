########################################################################################
########################################################################################
###                                                                                  ###
###                 R script for Human Penguin Project (Multi-Site)                  ###
###                  Hu, C-P., Yin, J-X., ..., IJzerman, H.(2018)                    ### 
###               Email = hcp4715@gmail.com       twitter= @hcp4715                  ###
###                                                                                  ###
########################################################################################
########################################################################################

########################################################################################
########################################################################################
###                                                                                  ###
###  Purpose:                                                                        ###
###  Processing the data from data from multi-site dataset as reported               ### 
###  in IJzerman et al.(2018), Human Penguin Project (HPP):https://osf.io/2rm5b/     ###
###                                                                                  ###
###  Code author: Chuan-Peng Hu, PhD, Neuroimaging Center (NIC), Johannes Gutenberg  ### 
###               University Medical Center, 55131 Mainz, Germany;                   ###
###   This script is largely based on spss syntax file from Hans:                    ###
###     'Syntax to Calculate Scales and Reliabilities.sps'                           ###
###                                                                                  ###
###  Input data                                                                      ###
###      Oringinal data: sav file: 'penguin v1d_7f.sav'                              ###
###      Revised data:'Data_Raw_HPP_Multi_Site_Share.csv', by Ji-Xing Yin            ###
###                                                                                  ###
###  Output file and Variables:                                                      ###
###     'Data_Sum_HPP_Multisite_Share.csv'                                           ###
###      including following variables:                                              ###
###        Age                                                                       ###
###        Sex                                                                       ###
###        stress         -- Perceived stress (Cohen & Wills, 1985)                  ###
###        nostalgia      -- (Routledge et al., 2008)                                ###
###        attachhome     -- attachment to home; Harris et al., 1996                 ###
###        selfcontrol    -- self-control, Tangney et al., 2004                      ###
###        avoidance      -- subscale of attachment, Fraley et al., 2000             ###
###        anxiety        -- subscale of attachment, Fraley et al., 2000             ###
###        EOT            -- alexithymia subscale; Kooiman et al., 2002              ###
###        DIDF           -- alexithymia subscale; Kooiman et al., 2002              ###
###        networksize    -- social network; Cohen et al., 1997                      ###
###        socialembedded -- social network; Cohen et al., 1997                      ###
###        CSI            -- diversity of social network; Cohen et al., 1997         ###
###        gluctot        -- daily sugary drink consumption, Henriksen et al., 2014  ###
###        artgluctot     -- diet drinks consumption, Henriksen et al., 2014         ###
###        mintemp        -- minimum temperature of the day                          ###
###        avghumidity    -- average humidity of the day                             ###
###     'Descriptives_HPP_Multi_Site_Share.csv', including descripitve info.         ###
###     'Reliability_HPP_Multi_Site_Share.csv', including reliability indices        ###
###                                                                                  ###
########################################################################################
########################################################################################


# ---------- Table of Contents ----------------------------------------------------------
# ---------- 1. Load libraries ----------------------------------------------------------
# ---------- 2. Load & manipulate data --------------------------------------------------
# ---------- 3. Calulate the social network indices -------------------------------------
# ---------- 4. Score and reliability of scales -----------------------------------------
# ---------- 5. Descriptives of the dataset ---------------------------------------------
# ---------- 6. Save output files -------------------------------------------------------

# ---------------------------------------------------------------------------------------
# ---------- 1. Load libraries and preparing --------------------------------------------
# ---------------------------------------------------------------------------------------
rm(list = ls())        # remove all variables in memory

### Get the directory ofcurrent script (only for r-studio)
curWD <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(curWD)

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English
options(digits = 3)                 

### define a function to install pacakges that not installed.
pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

### packages that needed
pkgNeeded <- (c("tidyverse",'stargazer',"psych","car",'lubridate'))

### load library
lapply(pkgNeeded,pkgTest) 

rm('pkgNeeded') # remove the variable 'pkgNeeded';


# ---------------------------------------------------------------------------------------
# ---------- 2. Load & manipulate data --------------------------------------------------
# ---------------------------------------------------------------------------------------

### load data
data <- read.csv("Data_Raw_HPP_Multi_Site_Share.csv", header = TRUE,
                 sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

table(data$sex) # check the number of male and female

### define the output file colnames:
### colnames used for comparing with reported data
nameMultSite <- c('age','romantic','sex','monogamous','health','gluctot',"artgluctot","smoke",
                 "cigs", "eatdrink","exercise", 'avgtemp','AvgHumidity','mintemp',
                 'language', "langfamily","Site")

### create an empty data frame with colnames
sumMultSite <- data[,nameMultSite]

### create an empty data frame for storing reliability
siteName <- unique(data$Site)        # get site names
sitesReliability <- data.frame(sites = siteName, scontrol_alpha = NA)   # create a empty dataframe for reliability
totalRow <- data.frame(sites = 'Total', scontrol_alpha = NA)            # add a row for total data
sitesReliability <- rbind(sitesReliability,totalRow)
sitesReliability$sites <- as.character(sitesReliability$sites)

siteName <- as.character(sitesReliability$sites)

warnSite <- data.frame(sitesReliability$sites)  # record which site have warning for omega estimation
colnames(warnSite) <- 'siteName'


# ---------------------------------------------------------------------------------------
# ---------- 3. calculate social network indices ----------------------------------------
# ---------------------------------------------------------------------------------------

#######################################
### Calculate the social diveristy ####
#######################################

###     social diversity sum up different relationship type, therefore, each relationship was binarized.
###     for social diversity, we re-code the types of relationship into 1 or 0
###     so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were recoded by function car::recoded

### get the column names:
snDivNames  <- c("SNI3", "SNI5", "SNI7", "SNI9", "SNI11", "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32")    # colnames of the extra groups

### create a empty dataframe for social network diversity
snDivData <- setNames(data.frame(matrix(ncol = length(snDivNames), nrow = nrow(data))), snDivNames)

### recode Q10 (spouse): 1-> 1; else ->0
snDivData$SNI1_r <- car::recode(data$SNI1,"1= 1; else = 0")

####re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
snDivData[,snDivNames] <- apply(data[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 

### add suffix to the colnames
colnames(snDivData[,snDivNames]) <- paste(snDivNames,"div",  sep = "_")    

### recode the social network at work by combining SNI17, SNI18
snDivData$SNIwork   <- snDivData$SNI17 + snDivData$SNI18
snDivData$SNIwork_r <- car::recode(snDivData$SNIwork,"0 = 0;1:10 = 1")

### re-code extra groups, 0/NA --> 0; more than 0 --> 1
extrDivData <- data[,extrDivName]          # Get extra data
extrDivData$sum <- rowSums(extrDivData)    # sum the other groups
snDivData$extrDiv_r <- car::recode(extrDivData$sum,"0 = 0; NA = 0; else = 1")  # recode

### Get the column names for social diversity 
snDivNames_r <- c("SNI1_r","SNI3","SNI5","SNI7","SNI9","SNI11","SNI13","SNI15","SNIwork_r",
                  "SNI19","SNI21","extrDiv_r")

### Get the social diveristy score
snDivData$SNdiversity <- rowSums(snDivData[,snDivNames_r])
sumMultSite$socialdiversity <- snDivData$SNdiversity       # assign it to the output file


##########################################
### Calculate the social Network size ####
##########################################
###     This index is the number of people in social network
###
###     Q10 - SNI1 (marital status): 1 or 0
###     Q12 ~ Q30  - SNI3 ~ SNI21 (odd numbers): as indicated
###     for volunteer or other social group,0, 1-6, or 7
###     Q33_2_1_1_text - SNI28 (group 1 members, see or talk)
###     Q33_2_1_2_text - SNI29 (group 2 members, see or talk)
###     Q33_2_1_3_text - SNI30 (group 3 members, see or talk)
###     Q33_2_1_4_text - SNI31 (group 4 members, see or talk)
###     Q33_2_1_5_text - SNI32 (group 5 members, see or talk)
###     NOTE: In our experience, individuals sometimes interpret the SNI item inquiring about the number of "other group" 
###     members with whom they interact at least once every 2 weeks more broadly than we intended, with some respondents 
###     reporting up to 100 or more fellow group-members. To ensure that social network size scores are not artificially inflated by 
###     individuals reporting large group memberships, we recommend recoding the variable so that all values over 6 are given a 
###     score of 7, thus keeping it consistent with all other quantitative SNI items.

### Get the colnames for calculating network size
snSizeNames <- c("SNI3", "SNI5", "SNI7", "SNI9", "SNI11", "SNI13", "SNI15", "SNI17","SNI18","SNI19","SNI21")
snSizeData <- data[,snSizeNames]      # get the data
snSizeData[is.na(snSizeData)] <- 0    # missing data to zero

### recode spouse data:
snSizeData$SNI1_r <- car::recode(data$SNI1,"1= 1; else = 0")

### recode extra group data:
snSizeData[,c("SNI28","SNI29","SNI30","SNI31",'SNI32')] <- 
        apply(data[,c("SNI28","SNI29","SNI30","SNI31",'SNI32')],2,
              function(x) {x <- car::recode(x,"0 = 0; NA = 0;1 = 1; 2= 2; 3= 3; 4= 4;5= 5; 6 = 6; else = 7"); x}) 

### Cobmine data
snSizeNames_r <- c("SNI1_r","SNI3", "SNI5", "SNI7", "SNI9" , "SNI11", "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21",
                   "SNI28","SNI29","SNI30","SNI31","SNI32")

snSizeData$snSize     <- rowSums(snSizeData[,snSizeNames_r],na.rm=TRUE)  # calculate the network size score
sumMultSite$networksize <- snSizeData$snSize                             # store to sumMultSite

##################################################
### Calculate the number of embedded networks ####
##################################################
###     family: SNI1_r, SNI3,SNI5,SNI7,SNI9 (if total > 4, family equal to 1, otherwise, 0);
###     friends: SNI11 (>4);
###     Church: SNI13 (>4);
###     Students/school: SNI 15 (>4)
###     Work: SNI17 + SNI 18 >4
###     neighbor: SNI19 >4
###     volunteer SNI21 >4
###     other groups: totoal > 4

### recode based on the above rules:
snEmbedNames <- c('family') # generate a dataframe for embed index.
snEmbedData <- setNames(data.frame(matrix(ncol = length(snEmbedNames), nrow = nrow(data))), snEmbedNames)

### calculated the embeded index for each network
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

### calculate the social embedded score
snEmbedData$socEmbd <- rowSums(snEmbedData[,c("family_r","friends_r","Church_r","StuSchool_r","work_r","neighbor_r","volun_r","extra_r")])
sumMultSite$socialembedded <- snEmbedData$socEmbd  # assign the value to output file

#### remove the temporary varialbes the no longer needed
rm(snDivData,snSizeData,snEmbedData,snDivNames,snDivNames_r,snEmbedNames,snSizeNames,snSizeNames_r,extrDivData)

# ---------------------------------------------------------------------------------------
# ---------- 4. Score and reliability of scales -----------------------------------------
# ---------------------------------------------------------------------------------------

######################################
######## self control scale ##########
######################################
### Get column names
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" ,
                   "scontrol6" , "scontrol7","scontrol8", "scontrol9", "scontrol10",
                   "scontrol11" ,"scontrol12", "scontrol13" )

### define keys for score
scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)   #  this dataset is already reverse coded 
scontrolKeys2 <- c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                        "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" )

### calculate the score:
SelfControlScore <- psych::scoreItems(scontrolNames,data[,scontrolNames], totals = T, min = 1, max = 5)
sumMultSite$scontrol <- SelfControlScore$scores # self control score

#### Reliability for each site for self control

### calculate the reliability for each site and total data
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, scontrolNames]
        } else {
                tmpdf <- data[data$Site == i, scontrolNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = scontrolKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$scontrolWarn[warnSite$siteName == i] <<- 1
                }
        )
        tmpOmega <- psych::omega(tmpdf)
        sitesReliability$scontrol_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$scontrol_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$scontrol_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### remove intermedian variables
rm(scontrolKeys,scontrolKeys2,scontrolNames,SelfControlScore)

######################################
###### perceived stress scale ########
######################################

### special case for poland data (reverse coding)
data_r <- data
rev_names <- c("stress4", "stress5", "stress6", "stress7","stress9", "stress10", "stress13")

data_r[data_r$Site == 'Poland',rev_names] <- 6 - data_r[data_r$Site == 'Poland',rev_names]

### get column name for stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", 
                 "stress9", "stress10","stress11", "stress12", "stress13", "stress14")

### define score key
stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # for current dataset

### calculate the score
stressScore <- psych::scoreItems(stressNames,data_r[,stressNames], totals = T, min = 1, max = 5)
sumMultSite$stress <- stressScore$scores

### calculate the alpha for each site
### Note: item 7 of Southampton is invariant, not able to calculate alpha, remove this site
siteName_stress <- siteName[!siteName %in% c('Southampton')] 
for (i in siteName_stress) {
        if (i == 'Total') {
                tmpdf <- data_r[, stressNames]
        } else {
                tmpdf <- data_r[data_r$Site == i, stressNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = stressKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$stressWarn[warnSite$siteName == i] <<- 1
                }
        )
        tmpOmega <- psych::omega(tmpdf)
        sitesReliability$stress_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$stress_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose the Standard alpha
        sitesReliability$stress_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose the Standard alpha
}

### remove variables
rm(data_r,stressNames,stressKeys, stressScore) # remove the intermediate variable 

######################################
########## attach to phone ###########
######################################

### get column names for the current scale
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )

attachphoneScore <- psych::scoreItems(phoneNames,data[,phoneNames], min = 1, max = 5) # mean score
sumMultSite$attachphone <- attachphoneScore$scores  # mean score

### alpha for each site
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, phoneNames]
                
        } else {
                tmpdf <- data[data$Site == i, phoneNames]
        }
        tmpAlpha <- psych::alpha(tmpdf,
                                 keys = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$phoneWarn[warnSite$siteName == i] <<- 1     
                }
        )
        sitesReliability$phone_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$phone_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h)  # chose Omega_H
        sitesReliability$phone_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### remove variables
rm(phoneNames)

######################################
############# Online Id ##############
######################################

### get the names
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")

onlineScore <- psych::scoreItems(onlineNames,data[,onlineNames], min = 1, max = 5) # mean score
sumMultSite$onlineid <- onlineScore$scores  # mean score

# alpha for each site
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, onlineNames]
        } else {
                tmpdf <- data[data$Site == i, onlineNames]
        }
        tmpAlpha <-
                psych::alpha(tmpdf, keys = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$onlineWarn[warnSite$siteName == i] <<- 1
                        }
        )
        sitesReliability$online_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$online_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$online_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}


######################################
####  ECR (anxiety & avoidance) ######
######################################

### score and alpha for ECR
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")

ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
             19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36) # reverse coded as negative

### alpha for each site
for (i in sitesReliability$sites) {
        if (i == 'Total') {
                tmpdf <- data[, ECRNames]
        } else {
                tmpdf <- data[data$Site == i, ECRNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = ECRKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$ECRWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$ECR_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$ECR_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$ECR_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### score and alpha for ECR Anxiety
anxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
### score keys
anxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) 

sumMultSite$anxiety <- rowSums(data[,anxietyNames],na.rm = T)/length(anxietyNames) # average score

### standardize the anxiety score for each site
sumMultSite <- plyr::ddply(sumMultSite,c('Site'),transform,anxiety_r = scale(anxiety))

### alpha for each site
### warnSite <- data.frame(siteName)
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, anxietyNames]
        } else {
                tmpdf <- data[data$Site == i, anxietyNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = anxietyKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$anxietyWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$anxiety_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$anxiety_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$anxiety_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### score and alpha for ECR avoidance
avoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                   "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
### score key
avoidanceKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

sumMultSite$avoidance <- rowSums(data[,avoidanceNames],na.rm = T)/length(avoidanceNames) # average score

### standardize for each group (special for this scale)
sumMultSite <- plyr::ddply(sumMultSite,c('Site'),transform,avoidance_r = scale(avoidance))

### alpha for each site
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, avoidanceNames]
        } else {
                tmpdf <- data[data$Site == i, avoidanceNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = avoidanceKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$avoidWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$avoidance_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$avoidance_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$avoidance_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

######################################
#### Southampton Nostalgia scale #####
######################################

### score and alpha for nostalgia, without Q83(SNS1)
nostalgiaNames <- c( "SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaKeys <- c(1,2,3,4,5,6) # reverse coded as negative
nostalgiaKeys2 <- c("SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")

nostalgiaScore <- psych::scoreItems(nostalgiaKeys2,data[,nostalgiaNames], totals = T, min = 1, max = 7) ## 
sumMultSite$nostalgia <- nostalgiaScore$scores

### reliability for each site
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, nostalgiaNames]
        } else {
                tmpdf <- data[data$Site == i, nostalgiaNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = nostalgiaKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$nostalgiaWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$nostalgia_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$nostalgia_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$nostalgia_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

######################################
###############  ALEX  ###############
######################################

## score and alpha coefficient for ALEX
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11)

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
eotKeys <- c(1,2,3,4,5)

### didf score
didfScore <- psych::scoreItems(didfNames,data[,didfNames], min = 1, max = 5)
sumMultSite$didf <- didfScore$scores

### EOT score
eotScore <- psych::scoreItems(eotNames,data[,eotNames], min = 1, max = 5)
sumMultSite$eot <- eotScore$scores


### reliability for each site for didf
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, didfNames]
        } else {
                tmpdf <- data[data$Site == i, didfNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = didfKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$didfWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$didf_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$didf_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$didf_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### reliability for each site for eot
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, eotNames]
        } else {
                tmpdf <- data[data$Site == i, eotNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = eotKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$eotWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$eot_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$eot_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$eot_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

######################################
####### attachement to home ##########
######################################

### score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

### caculate the score
homeScore <- psych::scoreItems(homeKeys,data[,homeNames],min = 1, max = 5) ## 
sumMultSite$attachhome <- homeScore$scores

### Reliability
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, homeNames]
        } else {
                tmpdf <- data[data$Site == i, homeNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = homeKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$homeWarn[warnSite$siteName == i] <<- 1
                }
        )
        sitesReliability$home_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$home_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$home_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

######################################
####### STRAQ-1         ##############
######################################

### score and alpha for STRAQ-1
straqNames_all  <- paste('STRAQ',c(1:57), sep = '_')

# High Temperature Sensitivity
# STRAQ_5,STRAQ_4,STRAQ_2,STRAQ_16,STRAQ_14,STRAQ_11,STRAQ_10
hiTempSensNames <- paste('STRAQ', c(5,4,2,16,14,11,10), sep = '_')
hiTempSensKeys  <- c(-1,-2,3,4,5,6,7)

# Social Thermoregulation
# STRAQ_19,STRAQ_20,STRAQ_44,STRAQ_48,STRAQ_49
socThermNames   <- paste('STRAQ', c(19,20,44,48,49), sep = '_')
socThermKeys    <- c(1,2,3,4,5)

# Unified Solitary Thermoregulation
# STRAQ_45,STRAQ_1,STRAQ_15,STRAQ_9,STRAQ_6; STRAQ_52,STRAQ_55,STRAQ_54
soliThermNames<- paste('STRAQ', c(45,1,15,9,6,52,55,54), sep = '_')
soliThermKeys <- c(-1,-2, 3,4,5,6,7,8)

# Risk Avoidance
# STRAQ_41,STRAQ_40,STRAQ_43
riskAvdNames    <- paste('STRAQ', c(41,40,43), sep = '_')
riskAvdKeys     <- c(1,2,3)
tmp <- psych::omega(data[,riskAvdNames])

# All items in final questionnaire
straq_vNames    <- c(hiTempSensNames,socThermNames,soliThermNames,riskAvdNames)
straq_vKeys     <- c(-1,-2,3,4,5,6,7,
                     8,9,10,11,12,
                     -13,-14, 15,16,17,18,19,10,
                     21,22,23)

### caculate the score
hiTempScore <- psych::scoreItems(hiTempSensKeys,data[,hiTempSensNames],min = 1, max = 5)  ## 
sumMultSite$hiTemp <- hiTempScore$scores
socThermScore <- psych::scoreItems(socThermKeys,data[,socThermNames],min = 1, max = 5)    ## 
sumMultSite$socTherm <- socThermScore$scores
soliThermScore <- psych::scoreItems(soliThermKeys,data[,soliThermNames],min = 1, max = 5) ## 
sumMultSite$soliTherm<- soliThermScore$scores
riskAvdScore <- psych::scoreItems(riskAvdKeys,data[,riskAvdNames],min = 1, max = 5)       ## 
sumMultSite$riskAvd <- riskAvdScore$scores

subscaleNames <- c('hiTempSens','socTherm','soliTherm','riskAvd','straq_v')  # for later loop of subscale and whole scale
### Reliability
for (j in subscaleNames) {                       
        tmpName = paste(j,'Names',sep = '')
        tmpName = eval(as.symbol(tmpName))  # call the variable use the tmpName 
        tmpKeys = paste(j,'Keys',sep = '')
        tmpKeys = eval(as.symbol(tmpKeys))
        alphaName = paste(j,'Alpha',sep = '')
        omega_hName = paste(j,'omega_h',sep = '')
        omega_tName = paste(j,'omega_t',sep = '')
        
        for (i in siteName) {
                if (i == 'Total') {
                        tmpdf <- data[, tmpName]
                } else {
                        tmpdf <- data[data$Site == i, tmpName]
                }
                #tmpAlpha <- psych::alpha(tmpdf, keys = tmpKeys)
                #tryCatch(
                tmpOmega <- psych::omega(tmpdf)
                #        warning = function(w) {
                #                print(w)
                #                warnSite[warnSite$siteName == i,j] <<- 1
                #        }
                #)
                sitesReliability[sitesReliability$sites == i,c(alphaName)] <-
                        as.numeric(tmpOmega$alpha)      # chose the Standard alpha
                sitesReliability[sitesReliability$sites == i,c(omega_hName)] <-
                        as.numeric(tmpOmega$omega_h)    # chose Omega_H
                sitesReliability[sitesReliability$sites == i,c(omega_tName)] <-
                        as.numeric(tmpOmega$omega.tot)  # chose Omega_H
        }
}

#Internal Consistencies by Language
#for (i in 1:length(siteName[1:15])){
#        print(levels(factor(data$siteName))[i])
#        print("Omega total")
#        print(omega(subset(data, Site ==levels(factor(data$Site))[i], select=c(STRAQ_5,STRAQ_4,STRAQ_2,STRAQ_16,STRAQ_14,STRAQ_11,STRAQ_10)))$omega.tot)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_19,STRAQ_20,STRAQ_44,STRAQ_48,STRAQ_49)))$omega.tot)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_45,STRAQ_1,STRAQ_15,STRAQ_9,STRAQ_6)))$omega.tot)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_41,STRAQ_40,STRAQ_43)))$omega.tot)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_52,STRAQ_55,STRAQ_54)))$omega.tot)  
        
#        print("Omega Hierarchical")
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_5,STRAQ_4,STRAQ_2,STRAQ_16,STRAQ_14,STRAQ_11,STRAQ_10)))$omega_h)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_19,STRAQ_20,STRAQ_44,STRAQ_48,STRAQ_49)))$omega_h)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_45,STRAQ_1,STRAQ_15,STRAQ_9,STRAQ_6)))$omega_h)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_41,STRAQ_40,STRAQ_43)))$omega_h)  
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_52,STRAQ_55,STRAQ_54)))$omega_h)  
        
#        print("Alpha")
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_5,STRAQ_4,STRAQ_2,STRAQ_16,STRAQ_14,STRAQ_11,STRAQ_10)))$alpha)
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_19,STRAQ_20,STRAQ_44,STRAQ_48,STRAQ_49)))$alpha)
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_45,STRAQ_1,STRAQ_15,STRAQ_9,STRAQ_6)))$alpha)
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_41,STRAQ_40,STRAQ_43)))$alpha)
#        print(omega(subset(data, Site==levels(factor(data$Site))[i], select=c(STRAQ_52,STRAQ_55,STRAQ_54)))$alpha)
#}

######################################
############### KAMF #################
######################################

### score and alpha for KAMF
kamfNames <- c("KAMF1" ,"KAMF2","KAMF3","KAMF4","KAMF5","KAMF6","KAMF7")
kamfData <- data[,kamfNames]

### recode to 1 - 8
kamfData$KAMF1_r <-kamfData$KAMF1*1.75 - 0.75
kamfData$KAMF3_r <-kamfData$KAMF3*1.166 - 0.166
kamfNames_r <- c("KAMF1_r" ,"KAMF2","KAMF3_r","KAMF4","KAMF5","KAMF6","KAMF7")
kamfKeys <- c(1,2,3,4,5,6,7) # reverse coded as negative

### score
kamfScore <- psych::scoreItems(kamfKeys,data[,kamfNames],min = 1, max = 5) ## 
sumMultSite$kamf <- kamfScore$scores

# reliability
for (i in siteName) {
        if (i == 'Total') {
                tmpdf <- data[, kamfNames]
        } else {
                tmpdf <- data[data$Site == i, kamfNames]
        }
        tmpAlpha <- psych::alpha(tmpdf, keys = kamfKeys)
        tryCatch(
                tmpOmega <- psych::omega(tmpdf),
                warning = function(w) {
                        print(w)
                        warnSite$kamfWarn[warnSite$siteName == i] <<-
                                1
                }
        )
        sitesReliability$kampf_alpha[sitesReliability$sites == i] <-
                as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
        sitesReliability$kampf_omega_h[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega_h) # chose Omega_H
        sitesReliability$kampf_omega_t[sitesReliability$sites == i] <-
                as.numeric(tmpOmega$omega.tot) # chose Omega_H
}

### re-order the data
sumMultSite_reord <- sumMultSite[order(sumMultSite$Site),order(names(sumMultSite))]
sitesReliability_reord <- sitesReliability[order(sitesReliability$sites),]

# ---------------------------------------------------------------------------------------
# ---------- 5. descriptive of the data -------------------------------------------------
# ---------------------------------------------------------------------------------------

### get the describtive data for variables, tidyverse style
describeMulSite1 <- data %>%
        select(Site,                          # select the columns
               age,
               romantic,
               sex,
               monogamous,
               avgtemp,
               mintemp,
               AvgHumidity,
               artgluctot,
               gluctot,
               Temperature_t1,
               Temperature_t2,
               health) %>%
        group_by(Site)  %>%                    # separate by site name
        dplyr::summarise(N = length(avgtemp),  # sample size for each site
                         age_m = mean(age, na.rm = T),  # age
                         age_sd = sd(age, na.rm = T),
                         age_NA = sum(is.na(age)),
                         romantic_yes = sum(romantic == 1, na.rm = T) / length(romantic),
                         romantic_no  = sum(romantic == 2, na.rm = T) / length(romantic),
                         romantic_NA = sum(is.na(romantic)) / length(romantic),
                         male = sum(sex == 1, na.rm = T) / length(sex),
                         female = sum(sex == 2, na.rm = T) / length(sex),
                         sex_other = sum(sex == 3, na.rm = T) / length(sex),
                         sex_NA = sum(is.na(sex)) / length(sex),
                         monog_m = mean(monogamous, na.rm = T),
                         monog_sd = sd(monogamous, na.rm = T),
                         monog_Na = sum(is.na(monogamous)),
                         mintemp_m = mean(mintemp, na.rm = T),
                         mintemp_sd = sd(mintemp, na.rm = T),
                         AvgHum = mean(AvgHumidity, na.rm = T),
                         AvgHum_sd = sd(AvgHumidity, na.rm = T),
                         artgluctot_m = mean(artgluctot, na.rm = T),
                         artgluctot_sd = sd(artgluctot, na.rm = T),
                         gluctot_m = mean(gluctot, na.rm = T),
                         gluctot_sd = sd(gluctot, na.rm = T),
                         health_m = mean(health, na.rm = T),
                         health_sd = sd(health, na.rm = T),
                         temp_T1_m = mean(Temperature_t1, na.rm = T),
                         temp_T1_sd = sd(Temperature_t1, na.rm = T),
                         temp_T2_m = mean(Temperature_t2, na.rm = T),
                         temp_T2_sd = sd(Temperature_t2, na.rm = T))

# calculate the descriptives of each scales
describeMulSite2 <- sumMultSite %>%
        select(Site,
                scontrol,
                stress,
                attachphone,
                onlineid,
                anxiety,
                avoidance,
                nostalgia,
                didf,
                eot,
                attachhome,
                kamf,
                networksize,
                socialdiversity,
                socialembedded) %>%
        group_by(Site) %>%
        dplyr::summarise(scontrol_m = mean(scontrol, na.rm = T),
                scontrol_sd = sd(scontrol, na.rm = T),
                stress_m = mean(stress, na.rm = T),
                stress_sd = sd(stress, na.rm = T),
                attachphone_m = mean(attachphone, na.rm = T),
                attachphone_sd = sd(attachphone, na.rm = T),
                onlineid_m = mean(onlineid, na.rm = T),
                onlineid_sd = sd(onlineid, na.rm = T),
                anxiety_m = mean(anxiety, na.rm = T),
                anxiety_sd = sd(anxiety, na.rm = T),
                mintemp_sd = sd(anxiety, na.rm = T),
                avoidance_m = mean(avoidance, na.rm = T),
                avoidance_sd = sd(avoidance, na.rm = T),
                nostalgia_m = mean(nostalgia, na.rm = T),
                nostalgia_sd = sd(nostalgia, na.rm = T),
                didf_m = mean(didf, na.rm = T),
                didf_sd = sd(didf, na.rm = T),
                eot_m = mean(eot, na.rm = T),
                eot_sd = sd(eot, na.rm = T),
                attachhome_m = mean(attachhome, na.rm = T),
                attachhome_sd = sd(attachhome, na.rm = T),
                kamf_m = mean(kamf, na.rm = T),
                kamf_sd = sd(kamf, na.rm = T),
                networksize_m = mean(networksize, na.rm = T),
                networksize_sd = sd(networksize, na.rm = T),
                socialdiversity_m = mean(socialdiversity, na.rm = T),
                socialdiversity_sd = sd(socialdiversity, na.rm = T),
                socialembedded_m = mean(socialembedded, na.rm = T),
                socialembedded_sd = sd(socialembedded, na.rm = T))

describeMulSite <- merge(describeMulSite1,describeMulSite2,by = 'Site')

# ---------------------------------------------------------------------------------------
# ---------- 6. Save output files -------------------------------------------------------
# ---------------------------------------------------------------------------------------

write.csv(sumMultSite_reord,'Data_Sum_HPP_Multi_Site_Share.csv',row.names = F)
write.csv(sitesReliability_reord,'Reliability_HPP_Multi_Site_Share.csv',row.names = F)
write.csv(describeMulSite,'Descriptives_HPP_Multi_Site_Share.csv',row.names = F)
write.csv(warnSite,'Data_warnings_omega.csv',row.names = F, na = '')

# ---------------------------------------------------------------------------------------
# ---------- End ------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------