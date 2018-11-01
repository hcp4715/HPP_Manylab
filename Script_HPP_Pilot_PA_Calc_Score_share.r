########################################################################################
########################################################################################
###                                                                                  ###
###                 R script for Human Penguin Project (prolific academic)           ###
###                  Hu, C-P., Yin, J-X., ..., IJzerman, H.(2018)                    ### 
###               Email = hcp4715@gmail.com       twitter= @hcp4715                  ###
###                                                                                  ###
########################################################################################
########################################################################################

########################################################################################
########################################################################################
###                                                                                  ###
###  Purpose:                                                                        ###
###  Pre-processing the data from pilot data from prolific_academic as reported      ### 
###  in IJzerman et al.(2018), Human Penguin Project (HPP):https://osf.io/2rm5b/     ###
###                                                                                  ###
###  Code author: Chuan-Peng Hu, PhD, Neuroimaging Center (NIC), Johannes Gutenberg  ### 
###               University Medical Center, 55131 Mainz, Germany;                   ###
###   This script is largely based on spss syntax file from Hans:                    ###
###     'Syntax to Calculate Scales and Reliabilities.sps'                           ###
###                                                                                  ###
###  Input data                                                                      ###
###      Oringinal data: sav file:                                                   ###
###                'prolific academic corrected dataset december 2015.sav'           ###
###      Revised data:'Data_Raw_HPP_Pilot_PA_Share.csv', by Ji-Xing Yin              ###
###                                                                                  ###
###  Output file and Variables:                                                      ###
###     'Data_Sum_HPP_Pilot_PA_Share.csv'                                            ###
###                                                                                  ###
########################################################################################
########################################################################################


# ---------- Table of Contents ----------------------------------------------------------
# ---------- 1. Load libraries ----------------------------------------------------------
# ---------- 2. Load & manipulate data --------------------------------------------------
# ---------- 3. Calulate the social network indices -------------------------------------
# ---------- 4. Score and reliability of scales -----------------------------------------
# ---------- 5. Descriptives of the dataset ---------------------------------------------


# ---------------------------------------------------------------------------------------
# ---------- 1. Load libraries and preparing --------------------------------------------
# ---------------------------------------------------------------------------------------

rm(list = ls())        # remove all variables in memory

### Get the directory ofcurrent script (only for r-studio)
curWD <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(curWD)

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English

pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

# packages
pkgNeeded <- (c("plyr", "psych","car",'lubridate'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';


# ---------------------------------------------------------------------------------------
# ---------- 2. Load & manipulate data --------------------------------------------------
# ---------------------------------------------------------------------------------------

#### Load data
data <- read.csv("Data_Raw_HPP_Pilot_PA_Share.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

### percentag of female:
table(data$Sex)  # male 37; female 55.

### Information of age
meanAge <- mean(data$birthyear,na.rm=TRUE)
sdAge   <- sd(data$birthyear,na.rm = T) 

### define the output file colnames:
### colnames used for comparing with reported data
namepilotSum <- c("birthyear", "anxiety", "attachhome", "attachphone", "avghumid", "avgtemp", "avoidance", "glucoseplosone",
                 "health", "Medication", "mintemp", "networksize", "nostalgia", "selfcontrol", "Sex",
                 "Site", "Smoking", "socialdiversity", "socialembedded", "stress")

### create an empty data frame with colnames
pilotSum <- setNames(data.frame(matrix(ncol = length(namepilotSum), nrow = nrow(data))), namepilotSum)

### copy the variables that don't need calcuation
copyName <- c('birthyear','glucoseplosone','Site','Sex','avghumid','mintemp','avgtemp','health','Medication','Smoking')
pilotSum[,copyName] <- data[,copyName]

# ---------------------------------------------------------------------------------------
# ---------- 3. calculate social network indices ----------------------------------------
# ---------------------------------------------------------------------------------------

#######################################
### Calculate the social diveristy ####
#######################################

### Social network diveristy is the CSI the paper#
### the corresponding between Questions number in SPSS and colnames in sharable data
###
### Social diversity sum up different relationship type, therefore, each relationship was binarized.
### for social diversity, we re-code the types of relationship into 1 or 0
### so, Q10, Q12,Q14,Q16,Q18,Q20,Q22,Q24,Q26(combined with Q27), Q28, Q30 were recoded by function car::recoded

### get the column names
snDivNames  <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
                "SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32")    # colnames of the extra groups

### get data for diversity
snDivData <- setNames(data.frame(matrix(ncol = length(snDivNames), nrow = nrow(data))), snDivNames)

### recode spouse/partner Q10 (spouse): 1-> 1; else ->0
snDivData$SNI1_r <- car::recode(data$SNI1,"1= 1; else = 0")

### re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
snDivData[,snDivNames] <- apply(data[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 
colnames(snDivData[,snDivNames]) <- paste(snDivNames,"div",  sep = "_")   # add suffix to the colnames

snDivData$SNIwork   <- snDivData$SNI17 + snDivData$SNI18                  # combine the diversity of work (SNI17, SNI18)
snDivData$SNIwork_r <- car::recode(snDivData$SNIwork,"0 = 0;1:10 = 1")

### re-code extra groups, 0/NA --> 0; more than 0 --> 1
extrDivData <- data[,extrDivName]  # Get extra data

### sum and recode the extra groups
extrDivData$sum <- rowSums(extrDivData)
snDivData$extrDiv_r <- car::recode(extrDivData$sum,"0 = 0; NA = 0; else = 1")

### combine the recoded variables
snDivNames_r <- c("SNI1_r","SNI3","SNI5","SNI7","SNI9","SNI11","SNI13","SNI15","SNIwork_r",
                  "SNI19","SNI21","extrDiv_r")

### get the social diveristy score
snDivData$SNdiversity   <- rowSums(snDivData[,snDivNames_r])

### save the social diversity index
pilotSum$socialdiversity <- snDivData$SNdiversity

#######################################
### Calculate social Network size  ####
#######################################
### Social Network size is the number of people in social network

### NOTE: In our experience, individuals sometimes interpret the SNI item inquiring about the number of "other group" 
### members with whom they interact at least once every 2 weeks more broadly than we intended, with some respondents 
### reporting up to 100 or more fellow group-members. To ensure that social network size scores are not artificially inflated by 
### individuals reporting large group memberships, we recommend recoding the variable so that all values over 6 are given a 
### score of 7, thus keeping it consistent with all other quantitative SNI items.

### the colnames for the columns that needed to be recoded for calculating network size
snSizeNames <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21")
snSizeData <- data[,snSizeNames]        # get the data
snSizeData[is.na(snSizeData)] <- 0      # missing data equal to zero

### recode data
snSizeData$SNI1_r <- car::recode(data$SNI1,"1= 1; else = 0")

snSizeData[, c("SNI28", "SNI29", "SNI30", "SNI31", 'SNI32')] <-
        apply(data[, c("SNI28", "SNI29", "SNI30", "SNI31", 'SNI32')],
              2, function(x) {x <- car::recode(x, "0 = 0; NA = 0;1 = 1;
                                       2= 2; 3= 3; 4= 4;5= 5; 6 = 6; else = 7") x })

### cobmine network size data
snSizeNames_r <- c("SNI1_r","SNI3", "SNI5", "SNI7", "SNI9", "SNI11", "SNI13",
                   "SNI15", "SNI17","SNI18","SNI19","SNI21","SNI28","SNI29","SNI30","SNI31","SNI32")

### calculate the network size
snSizeData$snSize   <- rowSums(snSizeData[,snSizeNames_r],na.rm=TRUE)

### save the data
pilotSum$networksize <- snSizeData$snSize                              

#######################################
### Calculate embedded networks    ####
#######################################
### number of embedded networks
### family: SNI1_r, SNI3,SNI5,SNI7,SNI9 (total > 4);
### friends: SNI11 (>4);
### Church: SNI13 (>4);
### Students/school: SNI 15 (>4)
### Work: SNI17 + SNI 18 >4
### neighbor: SNI19 >4
### volunteer SNI21 >4
### other groups: totoal > 4

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
snEmbedData$socEmbd    <- rowSums(snEmbedData[,c("family_r","friends_r","Church_r","StuSchool_r",
                                                 "work_r","neighbor_r","volun_r","extra_r")])

### save the value to output file
pilotSum$socialembedded <- snEmbedData$socEmbd  

### remove the temporary varialbes the no longer needed
rm(snDivData,snSizeData,snEmbedData,snDivNames,snDivNames_r,snEmbedNames,snSizeNames,snSizeNames_r,extrDivData)


# ---------------------------------------------------------------------------------------
# ---------- 4. Score and reliability of scales -----------------------------------------
# ---------------------------------------------------------------------------------------

######################################
######## self control scale ##########
######################################

### always pay attention to this scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , 
                   "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )

### keys for reliability, original scale with reverse coding
scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13)

### Keys for score
scontrolKeys2 <- c("scontrol1","-scontrol2","-scontrol3" ,"-scontrol4","-scontrol5", "scontrol6", "-scontrol7",
                        "scontrol8", "-scontrol9", "-scontrol10", "scontrol11","-scontrol12", "-scontrol13" )

### alpha coefficient 
scontrolAlpha <- psych::alpha(data[,scontrolNames], keys=scontrolKeys)  
print(scontrolAlpha$total)  # 0.879 

### McDonald's omega
scontrolOmega <- psych::omega(data[,scontrolNames])
print(c(scontrolOmega$omega_h,scontrolOmega$omega.tot)) # print omega-hierarchical and omega_total

### self control score
SelfControlScore <- psych::scoreItems(scontrolKeys2,data[,scontrolNames], totals = T, min = 1, max = 5)
pilotSum$selfcontrol <- SelfControlScore$scores 

######################################
###### perceived stress scale ########
######################################

### variable column names
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13","stress14")

### keys for reliability
stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,-12,13,14) 

### keys for score
stressKeys2 <- c("stress1" , "stress2" ,"stress3","-stress4", "-stress5", "-stress6", "-stress7", 
                      "stress8","-stress9", "-stress10","stress11", "-stress12", "stress13",'stress14')

### Alpha coefficient
stressAlpha <- psych::alpha(data[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.9107

### McDonald's omega
stressOmega <- psych::omega(data[,stressNames])      # warnings a loading great than 1 was detected
print(c(stressOmega$omega_h, stressOmega$omega.tot)) 

### stress score
stressScore <- psych::scoreItems(stressKeys2,data[,stressNames],totals = T, min = 1, max = 5)
pilotSum$stress <-stressScore$scores


######################################
########## attach to phone ###########
######################################

### get the column names for current scale
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )

### calculate the alpha coefficient
phoneAlpha <- psych::alpha(data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))   
print(phoneAlpha$total)   # std. alpha 0.8698

### McDonald's omega
phoneOmega <- psych::omega(data[,phoneNames])     # warning: an ultra-Heywood case 1 was detected
print(c(phoneOmega$omega_h,phoneOmega$omega.tot)) # 

### score
pilotSum$attachphone <- rowSums(data[,phoneNames],na.rm = T) # sum score

######################################
############# Online Id ##############
######################################

### get the names
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")

### calculate the alpha coefficient
onlineAlpha <- psych::alpha(data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  
print(onlineAlpha$total)  

### McDonald's omega
onlineOmega <- psych::omega(data[,onlineNames]) 
print(c(onlineOmega$omega_h,onlineOmega$omega.tot)) # 0.6933

### score
pilotSum$onlineid <- rowSums(data[,onlineNames],na.rm = T)/length(onlineNames)


######################################
####  ECR (anxiety & avoidance) ######
######################################

### variable names
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
             19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding

### make the key list for scoreItems
ECRKeys2 <- list(c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "-ECR11",
                    "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","-ECR20","ECR21","-ECR22",
                    "ECR23","ECR24","ECR25","-ECR26","ECR27","-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33",
                    "-ECR34","-ECR35","-ECR36"))

### Alpha
ECRAlpha <- psych::alpha(data[,ECRNames], 
                         keys=ECRKeys)   
print(ECRAlpha$total)  # std. alpha 0.95382

### Omega
ECROmega <- psych::omega(data[,ECRNames]) # warnings
print(c(ECROmega$omega_h,ECROmega$omega.tot)) # 0.672

### score and alpha for ECR Anxiety
ECRanxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
ECRanxietyKeys  <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
ECRanxietyKeys2 <- c("ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "-ECR9", "ECR10", "-ECR11",
                          "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")

ECRanxietyAlpha <- psych::alpha(data[,ECRanxietyNames], 
                                keys=ECRanxietyKeys)  # calculate the alpha coefficient 
print(ECRanxietyAlpha$total)                          # std. alpha 0.9371

ECRanxietyOmega <- psych::omega(data[,ECRanxietyNames]) 
print(c(ECRanxietyOmega$omega_h,ECRanxietyOmega$omega.tot)) # 0.6846

ECRanxietyScore <- psych::scoreItems(ECRanxietyKeys2,data[,ECRanxietyNames], totals = T, min = 1, max = 7) 
pilotSum$anxiety <-  ECRanxietyScore$scores                  # sum score

### score and alpha for ECR avoidance
ECRavoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                        "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
ECRavoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
ECRavoidanceKeys2 <- c("ECR19","-ECR20","ECR21","-ECR22", "ECR23","ECR24","ECR25","-ECR26","-ECR27",
                            "-ECR28","-ECR29","-ECR30","-ECR31","ECR32","-ECR33", "-ECR34","-ECR35","-ECR36")

ECRavoidanceAlpha <- psych::alpha(data[,ECRavoidanceNames], 
                                  keys=ECRavoidanceKeys)       # calculate the alpha coefficient 
print(ECRavoidanceAlpha$total)  # std. alpha 0.9459, 

ECRavoidanceOmega <- psych::omega(data[,ECRavoidanceNames]) 
print(c(ECRavoidanceOmega$omega_h,ECRavoidanceOmega$omega.tot)) # 0.784

ECRavoidanceScore <- psych::scoreItems(ECRavoidanceKeys2,data[,ECRavoidanceNames], totals = T, min = 1, max = 7)
pilotSum$avoidance <- ECRavoidanceScore$scores # sum score


######################################
#### Southampton Nostalgia scale #####
######################################

### variable names 
nostalgiaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7")
nostalgiaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostalgiaKeys2 <- c( "-SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7")

### Alpha
nostalgiaAlpha <- psych::alpha(data[,nostalgiaNames], keys=nostalgiaKeys)  # calculate the alpha coefficient 
print(nostalgiaAlpha$total)  # 0.9152, std. alpha 0.9527

### McDonald's omega
nostalgiaOmega <- psych::omega(data[,nostalgiaNames]) 
print(c(nostalgiaOmega$omega_h,nostalgiaOmega$omega.tot)) # 0.8776

nostalgiaScore <- psych::scoreItems(nostalgiaKeys2,data[,nostalgiaNames], totals = T, min = 1, max = 7) ## 
pilotSum$nostalgia <- nostalgiaScore$scores

### another way to calculate the score and alpha for nostalgia, without Q83(SNS1)
nostalgiaNames_2 <- c( "SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaKeys_2 <- c(1,2,3,4,5,6) # reverse coded as negative
nostalgiaKeys2_2 <- c("SNS2","SNS3","SNS4", "SNS5","SNS6","SNS7")
nostalgiaAlpha2 <- psych::alpha(data[,nostalgiaNames_2], keys=nostalgiaKeys_2)  # calculate the alpha coefficient 
print(nostalgiaAlpha2$total)  # 0.9079, std. alpha 0.95425
nostalgiaOmega2 <- psych::omega(data[,nostalgiaNames_2])    # warnings: a loading greater than abs(1) was detected;
print(c(nostalgiaOmega2$omega_h,nostalgiaOmega2$omega.tot)) # 0.871

nostalgiaScore2 <- psych::scoreItems(nostalgiaKeys2_2,data[,nostalgiaNames_2], totals = T, min = 1, max = 7) 
pilotSum$nostalgia2 <- nostalgiaScore2$scores

# remove temporary variables to accelerate the processing
rm(nostalgiaNames,nostalgiaKeys,nostalgiaKeys2,nostalgiaAlpha)

######################################
###############  ALEX  ###############
######################################

### didf
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys2 <- c("ALEX1","ALEX2","ALEX3","-ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
#didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11) # in case the score is already re-coded

### Alpha
didfAlpha <-  psych::alpha(data[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF: std.aplha: 0.9081569
didfScore <- psych::scoreItems(didfKeys2,data[,didfNames], min = 1, max = 5)
pilotSum$didf <- didfScore$scores

### McDonald's omega
didfOmega <- psych::omega(data[,didfNames])  # warnings: a loading greater than abs(1) was detected;
print(c(didfOmega$omega_h,didfOmega$omega.tot)) # 0.8447

### EOT
eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys2 <- c("-ALEX12","ALEX13","-ALEX14","ALEX15" ,"-ALEX16")

### alpha
eotAlpha <-  psych::alpha(data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotAlpha$total)  # print the alpha for eot:std. alpha: 0.560
eotScore <- psych::scoreItems(eotKeys2,data[,eotNames], min = 1, max = 5)
pilotSum$eot <- eotScore$scores

eotOmega <- psych::omega(data[,eotNames])     # warnings: a loading greater than abs(1) was detected;
print(c(eotOmega$omega_h,eotOmega$omega.tot)) # 0.5074


######################################
####### attachement to home ##########
######################################
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(data[,homeNames], 
                          keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9061

### McDonald's omega
homeOmega <- psych::omega(data[,homeNames])     # warnings: An ultra-Heywook case;
print(c(homeOmega$omega_h,homeOmega$omega.tot)) # 0.688

pilotSum$attachhome <- rowSums(data[,homeNames],na.rm = T)/length(homeNames)

# ---------------------------------------------------------------------------------------
# ---------- 5. Save output files -------------------------------------------------------
# ---------------------------------------------------------------------------------------

### re-order the columns
pilotSum_reord <- pilotSum[ , order(names(pilotSum))]
write.csv(pilotSum_reord,'Data_Sum_HPP_Pilot_PA_Share.csv',row.names = F)

# ---------------------------------------------------------------------------------------
# ---------- End ------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------