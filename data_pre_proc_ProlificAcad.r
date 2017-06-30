## Code accompanying Hu, IJzerman et al.
## Please cite the "Penguin Project" when using this syntax (https://osf.io/2rm5b/)
## Install these packages below first(!) - not all used.

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
pkgNeeded <- (c("randomForest","plyr","foreign", "party", 'tree','lattice','stargazer',"summarytools","psych","car",'memisc'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# this belowing code was not used.
## to read spss file with duplicated labels, fixed this error from: https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x){
        if(!is.null(attr(x, "value.labels"))){
                vlab <- attr(x, "value.labels")
                if(sum(duplicated(vlab)) > 0)
                        cat("Duplicated levels:", vlab, "\n")
                else if(sum(duplicated(names(vlab))) > 0)
                        cat("Duplicated labels:",
                            names(vlab)[duplicated(names(vlab))], "\n")
                else
                        x <- factor(x, levels = as.numeric(vlab),
                                    labels = names(vlab))
        }
        x
}

mul_data_raw <- read.spss("HPP_pilot_MTurk_cleaned_deidentified.sav", use.value.labels = FALSE)
mul_data_raw <- lapply(mul_data_raw, Int2Factor)
mul_data_raw <- as.data.frame(mul_data_raw, stringsAsFactors = FALSE)

attach(mul_data_raw)

data_spss <- data.frame(as.data.set(spss.portable.file("HPP_pilot_MTurk_cleaned_deidentified.por")))

# I used the following code to read data
DataRaw <- read.csv("HPP_pilot_PA_deidentified_hcp.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# recode the temperature:
#DataRaw$Temperature_t1 <- if (any(DataRaw$Q8 == 2)) (((DataRaw$Q7-32)*5)/9) else DataRaw$Q7
DataRaw$Temperature_t1_r <- DataRaw$Q7
DataRaw$Temperature_t1[DataRaw$Q8 ==2] <- ((DataRaw$Q7[DataRaw$Q8 ==2]-32)*5)/9
#DataRaw$Temperature_t2 <- if (any(DataRaw$Q66 == 2)) (((DataRaw$Q65-32)*5)/9) else DataRaw$Q65
DataRaw$Temperature_t2 <- DataRaw$Q65
DataRaw$Q66[is.na(DataRaw$Q66)] <- 0
DataRaw$Temperature_t2[DataRaw$Q66== 2] <- ((DataRaw$Q65[DataRaw$Q66 == 2]-32)*5)/9
DataRaw$avgtemp_r <- rowSums(DataRaw[,c('Temperature_t1','Temperature_t2')],na.rm = T)/2
DataRaw$avgtemp_r[is.na(DataRaw$Q65)] <- DataRaw$Temperature_t1[is.na(DataRaw$Q65)]

# there was one participants filled 32 for Q7 and 2 for Q8, resulted 0 for t1; however, the results of Q66 was 1, 
# and again the answer for Q65 was 32. so here I change the answer for Q8 as 1.
DataRaw$Q8[DataRaw$Q7 == 32 & DataRaw$Q8 == 2] <- 1

DataRaw$Temperature_t1 <- DataRaw$Q7
DataRaw$Temperature_t1[DataRaw$Q8 ==2] <- ((DataRaw$Q7[DataRaw$Q8 ==2]-32)*5)/9
#DataRaw$Temperature_t2 <- if (any(DataRaw$Q66 == 2)) (((DataRaw$Q65-32)*5)/9) else DataRaw$Q65
DataRaw$Temperature_t2 <- DataRaw$Q65
DataRaw$Q66[is.na(DataRaw$Q66)] <- 0
DataRaw$Temperature_t2[DataRaw$Q66== 2] <- ((DataRaw$Q65[DataRaw$Q66 == 2]-32)*5)/9
DataRaw$avgtemp_r <- rowSums(DataRaw[,c('Temperature_t1','Temperature_t2')],na.rm = T)/2
DataRaw$avgtemp_r[is.na(DataRaw$Q65)] <- DataRaw$Temperature_t1[is.na(DataRaw$Q65)]

# DataRaw$country_region <- data_spss$country_

# birth year
DataRaw$birthyear <- as.integer(paste("19",as.character(round(DataRaw$Q87,2)),sep = ''))


valid.data <- subset(DataRaw,avgtemp_r > 34.99) # average temperature higher than 34.99 is valid
valid.data1 <- subset(DataRaw,Temperature_t1 > 34.99)
valid.data2 <- subset(DataRaw,Temperature_t2 > 34.99)
valid.data3 <- subset(DataRaw,Temperature_t2 > 34.99 & Temperature_t1 > 34.99 )


## dataframe for summary data
Datasum <- valid.data[,c('age','sex')]               # age, sex
Datasum$num <- seq(1:nrow(Datasum))                  # add an index
Datasum <- Datasum[,c('num','age','sex')]            # re-order the columns
Datasum$Temperature_t1 <- valid.data$Temperature_t1  # temperature at time point 1
Datasum$Temperature_t2 <- valid.data$Temperature_t2  # temperature at time point 2
Datasum$avgtemp <- (Datasum$Temperature_t1 + Datasum$Temperature_t2)/2  # average temperature

#### calculate social network index ####
## calculate the soical diveristy
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
Datasum$socialDiversity <- SNIData$SNdiversity # complex social integration
Datasum$snSize <- SNSizeData$snSize 
Datasum$socEmbd <- SNSizeData$socEmbd 

#### below is the calculating of scale score and aphla coefficient for each scale ####

## score and alpha for self control scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
# scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # in case if the score in this dataset is already reversed
scontrolAlpha <- psych::alpha(valid.data[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  # 0.467!!!!  problematic
Datasum$scontrol <- rowSums(valid.data[,scontrolNames],na.rm = T)/length(scontrolNames) # average score


## score and alpha for perceive stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13", "stress14")
stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
# stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # in case the score is already re-coded

stressAlpha <- psych::alpha(valid.data[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.6778  Not right
Datasum$stress <- rowSums(valid.data[,stressNames],na.rm = T)/length(stressNames) # average score

## score and alpha for attach phone
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )
phoneAlpha <- psych::alpha(valid.data[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8868
Datasum$phone <- rowSums(valid.data[,phoneNames],na.rm = T)/length(phoneNames) # average score


## score and alpha for online
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")
onlineAlpha <- psych::alpha(valid.data[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8977
Datasum$online <- rowSums(valid.data[,onlineNames],na.rm = T)/length(onlineNames) # average score

## score and alpha for ECR
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
             19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
#ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
#             19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36) # in case the score is already re-coded

ECRAlpha <- psych::alpha(valid.data[,ECRNames], 
                            keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.776, instead of 0.932
Datasum$ECR <- rowSums(valid.data[,ECRNames],na.rm = T)/length(ECRNames) # average score

## score and alpha for ECR Anxiety
anxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
anxietyKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
# anxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) # in case the score is already re-coded

anxietyAlpha <- psych::alpha(valid.data[,anxietyNames], 
                         keys=anxietyKeys)  # calculate the alpha coefficient 
print(anxietyAlpha$total)  # std. alpha 0.876, instead of 0.92
Datasum$anxiety <- rowSums(valid.data[,anxietyNames],na.rm = T)/length(anxietyNames) # average score

## score and alpha for ECR avoidance
avoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                   "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
avoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
# avoidanceKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)   # in case the score is already re-coded

avoidanceAlpha <- psych::alpha(valid.data[,avoidanceNames], 
                             keys=avoidanceKeys)  # calculate the alpha coefficient 
print(avoidanceAlpha$total)  # std. alpha 0.838, instead of 0.916
Datasum$avoidance <- rowSums(valid.data[,avoidanceNames],na.rm = T)/length(avoidanceNames) # average score

## score and alpha for nostaglia
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
# nostagliaKeys <- c(1,2,3,4,5,6,7) # in case the score is already re-coded
nostagliaAlpha <- psych::alpha(valid.data[,nostagliaNames], 
                               keys=nostagliaKeys)  # calculate the alpha coefficient 
print(nostagliaAlpha$total)  # std. alpha 0.765, instead of 0.92
nostagliaItem <- psych::scoreItems(nostagliaKeys,valid.data[,nostagliaNames],min = 1, max = 7) ## 

## score and alpha coefficient for ALEX
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
#didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11) # in case the score is already re-coded

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
eotKeys <- c(-1,2,-3,4,-5) # original
# eotKeys <- c(1,2,3,4,5) # in case the score is already re-coded

Datasum$didf <- rowSums(valid.data[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.data[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF

Datasum$eot <- rowSums(valid.data[,eotNames],na.rm = T)/length(eotNames) # average score
eotfAlpha <-  psych::alpha(valid.data[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotfAlpha$total)  # print the alpha for eot

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.data[,homeNames], 
                               keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9049, instead of 0.901

## gluctot and artgluctot (already calculated in multi-site dataset)
Datasum$gluctot <- rowSums(valid.data[,c("Q89_6_1_TEXT",'Q89_7_1_TEXT','Q89_12_1_TEXT')],na.rm = T)
Datasum$artgluctot <- rowSums(valid.data[,c("Q89_8_1_TEXT",'Q89_9_1_TEXT','Q89_13_1_TEXT')],na.rm = T)


##### end ####

