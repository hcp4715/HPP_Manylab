<<<<<<< HEAD
## Code accompanying IJzerman et al.
## Some of the code below based on http://www.stanford.edu/~stephsus/R-randomforest-guide.pdf, and further modified by Thomas Pollet and Hans IJzerman
## Please cite the "Penguin Project" when using this syntax (https://osf.io/2rm5b/)
## Install these packages below first(!) - not all used.

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English

pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

# packages
pkgNeeded <- (c("randomForest","plyr","foreign", "party", 'tree','lattice','stargazer',"summarytools","psych"))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# this belowing code was not used.
## to read spss file with duplicated labels, fixed this error from: https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x)
{
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

mul_data_raw <- read.spss("penguin v1d_7f.sav", use.value.labels = FALSE)
mul_data_raw <- lapply(mul_data_raw, Int2Factor)
mul_data_raw <- as.data.frame(mul_data_raw, stringsAsFactors = FALSE)

attach(dataset.cleaned)


# I used the following code to read data
mulDataRaw <- read.csv("HPP_mul_site_0613.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

valid.mulRaw <- subset(mulDataRaw,avgtemp > 34.99) # average temperature higher than 34.99 is valid
valid.mulRaw2 <- subset(mulDataRaw,Temperature_t1 > 34.99)
invalid.mulRaw <- subset(mulDataRaw,avgtemp <= 34.99)

##
mulDatasum <- valid.mulRaw[,c('age','sex')]
mulDatasum$num <- seq(1:nrow(mulDatasum))
mulDatasum <- mulDatasum[,c('num','age','sex')]
mulDatasum$Temperature_t1 <- valid.mulRaw$Temperature_t1
mulDatasum$Temperature_t2 <- valid.mulRaw$Temperature_t2
mulDatasum$avgtemp <- (mulDatasum$Temperature_t1 + mulDatasum$Temperature_t2)/2

#### below is the calculating of scale score and aphla coefficient for each scale ####
## calculate the complex social integration
valid.mulRaw$SNI1_r <- valid.mulRaw$SNI1 
valid.mulRaw$SNI1_r[valid.mulRaw$SNI1_r >= 2] <- 0 # re-code data without spoue as 0
SNINames <- c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17" ,"SNI18", "SNI19","SNI21",
              "SNI28" , "SNI29" , "SNI30" , "SNI31"  ,  "SNI32" )

valid.mulRaw[,SNINames][is.na(valid.mulRaw[,SNINames])] <- 0 # change the NAs to 0,

mulDatasum$CSI <- rowSums(valid.mulRaw[,SNINames])

## score and alpha for self control scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
# scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # the score in this dataset is already reversed
scontrolAlpha <- psych::alpha(valid.mulRaw[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  # 0.467!!!!  problematic
mulDatasum$scontrol <- rowSums(valid.mulRaw[,scontrolNames],na.rm = T)/length(scontrolNames) # average score

# alpha for each site for self control
siteName <- unique(valid.mulRaw$Site)
sitesAlpha <- data.frame(sites = siteName, alphaScontrol = NA)

sitesAlpha$sites <- as.character(sitesAlpha$sites)
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,scontrolNames]
        tmpAlpha <- psych::alpha(tmpdf, keys=scontrolKeys) 
        sitesAlpha$alphaScontrol[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for perceive stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13", "stress14")
# stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # for current dataset

stressAlpha <- psych::alpha(valid.mulRaw[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.6778  Not right
mulDatasum$stress <- rowSums(valid.mulRaw[,stressNames],na.rm = T)/length(stressNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,stressNames]
        tmpAlpha <- psych::alpha(tmpdf,keys = stressKeys)
        sitesAlpha$alphaStress[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for attach phone
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )

phoneAlpha <- psych::alpha(valid.mulRaw[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8868
mulDatasum$phone <- rowSums(valid.mulRaw[,phoneNames],na.rm = T)/length(phoneNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,phoneNames]
        tmpAlpha <- psych::alpha(tmpdf, 
                                 keys= c(1,2,3,4,5,6,7,8,9)) 
        sitesAlpha$alphaPhone[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for online
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")

onlineAlpha <- psych::alpha(valid.mulRaw[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8977
mulDatasum$online <- rowSums(valid.mulRaw[,onlineNames],na.rm = T)/length(onlineNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,onlineNames]
        tmpAlpha <- psych::alpha(tmpdf, 
                                 keys= c(1,2,3,4,5,6,7,8,9,10,11)) 
        sitesAlpha$alphaOnline[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha


## score and alpha for ECR
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
# ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
#              19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
             19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36) # reverse coded as negative

ECRAlpha <- psych::alpha(valid.mulRaw[,ECRNames], 
                            keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.776, instead of 0.932
mulDatasum$ECR <- rowSums(valid.mulRaw[,ECRNames],na.rm = T)/length(ECRNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,ECRNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= ECRKeys) 
        sitesAlpha$alphaECR[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for ECR Anxiety
anxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
# anxietyKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
anxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) 

anxietyAlpha <- psych::alpha(valid.mulRaw[,anxietyNames], 
                         keys=anxietyKeys)  # calculate the alpha coefficient 
print(anxietyAlpha$total)  # std. alpha 0.876, instead of 0.92
mulDatasum$anxiety <- rowSums(valid.mulRaw[,anxietyNames],na.rm = T)/length(anxietyNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,anxietyNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= anxietyKeys) 
        sitesAlpha$alphaECRAnxiety[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for ECR avoidance
avoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                   "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
# avoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
avoidanceKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

avoidanceAlpha <- psych::alpha(valid.mulRaw[,avoidanceNames], 
                             keys=avoidanceKeys)  # calculate the alpha coefficient 
print(avoidanceAlpha$total)  # std. alpha 0.838, instead of 0.916
mulDatasum$avoidance <- rowSums(valid.mulRaw[,avoidanceNames],na.rm = T)/length(avoidanceNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,avoidanceNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= avoidanceKeys) 
        sitesAlpha$alphaECRAvoidance[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha


## score and alpha for nostaglia
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
# nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostagliaKeys <- c(1,2,3,4,5,6,7)
nostagliaAlpha <- psych::alpha(valid.mulRaw[,nostagliaNames], 
                               keys=nostagliaKeys)  # calculate the alpha coefficient 
print(nostagliaAlpha$total)  # std. alpha 0.765, instead of 0.92
nostagliaItem <- psych::scoreItems(nostagliaKeys,valid.mulRaw[,nostagliaNames],min = 1, max = 7) ## 

## score and alpha coefficient for ALEX
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
#didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11)

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
# eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys <- c(1,2,3,4,5)

mulDatasum$didf <- rowSums(valid.mulRaw[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.mulRaw[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF

mulDatasum$eot <- rowSums(valid.mulRaw[,eotNames],na.rm = T)/length(eotNames) # average score
eotfAlpha <-  psych::alpha(valid.mulRaw[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotfAlpha$total)  # print the alpha for eot

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.mulRaw[,homeNames], 
                               keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9049, instead of 0.901

homeItem <- psych::scoreItems(homeKeys,valid.mulRaw[,homeNames],min = 1, max = 5) ## 

## score and alpha for KAMF

kamfNames <- c("KAMF1" ,"KAMF2","KAMF3","KAMF4","KAMF5","KAMF6","KAMF7")
kamfKeys <- c(1,2,3,4,5,6,7) # reverse coded as negative

kamfAlpha <- psych::alpha(valid.mulRaw[,kamfNames], 
                          keys=kamfKeys)  # calculate the alpha coefficient 
print(kamfAlpha$total)  # std. alpha 0.9049, instead of 0.901

kamfItem <- psych::scoreItems(kamfKeys,valid.mulRaw[,kamfNames],min = 1, max = 5) ## 


##### end ####
=======
## Code accompanying IJzerman et al.
## Some of the code below based on http://www.stanford.edu/~stephsus/R-randomforest-guide.pdf, and further modified by Thomas Pollet and Hans IJzerman
## Please cite the "Penguin Project" when using this syntax (https://osf.io/2rm5b/)
## Install these packages below first(!) - not all used.

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English

pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

# packages
pkgNeeded <- (c("randomForest","plyr","foreign", "party", 'tree','lattice','stargazer',"summarytools","psych"))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# this belowing code was not used.
## to read spss file with duplicated labels, fixed this error from: https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x)
{
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

mul_data_raw <- read.spss("penguin v1d_7f.sav", use.value.labels = FALSE)
mul_data_raw <- lapply(mul_data_raw, Int2Factor)
mul_data_raw <- as.data.frame(mul_data_raw, stringsAsFactors = FALSE)

attach(dataset.cleaned)


# I used the following code to read data
mulDataRaw <- read.csv("HPP_mul_site_0613.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

valid.mulRaw <- subset(mulDataRaw,avgtemp > 34.99) # average temperature higher than 34.99 is valid
valid.mulRaw2 <- subset(mulDataRaw,Temperature_t1 > 34.99)
invalid.mulRaw <- subset(mulDataRaw,avgtemp <= 34.99)

##
mulDatasum <- valid.mulRaw[,c('age','sex')]
mulDatasum$num <- seq(1:nrow(mulDatasum))
mulDatasum <- mulDatasum[,c('num','age','sex')]
mulDatasum$Temperature_t1 <- valid.mulRaw$Temperature_t1
mulDatasum$Temperature_t2 <- valid.mulRaw$Temperature_t2
mulDatasum$avgtemp <- (mulDatasum$Temperature_t1 + mulDatasum$Temperature_t2)/2

#### below is the calculating of scale score and aphla coefficient for each scale ####
## calculate the complex social integration
valid.mulRaw$SNI1_r <- valid.mulRaw$SNI1 
valid.mulRaw$SNI1_r[valid.mulRaw$SNI1_r >= 2] <- 0 # re-code data without spoue as 0
SNINames <- c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17" ,"SNI18", "SNI19","SNI21",
              "SNI28" , "SNI29" , "SNI30" , "SNI31"  ,  "SNI32" )

valid.mulRaw[,SNINames][is.na(valid.mulRaw[,SNINames])] <- 0 # change the NAs to 0,

mulDatasum$CSI <- rowSums(valid.mulRaw[,SNINames])

## score and alpha for self control scale
scontrolNames <- c("scontrol1","scontrol2","scontrol3" ,"scontrol4","scontrol5" , "scontrol6" , "scontrol7","scontrol8", "scontrol9", "scontrol10", "scontrol11" ,"scontrol12", "scontrol13" )
# scontrolKeys <- c(1,-2,-3,-4,-5,6,-7,8,-9,-10,11,-12,-13) #  this is the original scale with reverse coding
scontrolKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13) # the score in this dataset is already reversed
scontrolAlpha <- psych::alpha(valid.mulRaw[,scontrolNames], keys=scontrolKeys)  # calculate the alpha coefficient 
print(scontrolAlpha$total)  # 0.467!!!!  problematic
mulDatasum$scontrol <- rowSums(valid.mulRaw[,scontrolNames],na.rm = T)/length(scontrolNames) # average score

# alpha for each site for self control
siteName <- unique(valid.mulRaw$Site)
sitesAlpha <- data.frame(sites = siteName, alphaScontrol = NA)

sitesAlpha$sites <- as.character(sitesAlpha$sites)
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,scontrolNames]
        tmpAlpha <- psych::alpha(tmpdf, keys=scontrolKeys) 
        sitesAlpha$alphaScontrol[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for perceive stress
stressNames <- c("stress1" , "stress2" ,"stress3","stress4", "stress5", "stress6", "stress7", "stress8", "stress9", "stress10",
                 "stress11", "stress12", "stress13", "stress14")
# stressKeys <- c(1,2,3,-4,-5,-6,-7,8,-9,-10,11,12,-13,14) # original key for reverse coding
stressKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)        # for current dataset

stressAlpha <- psych::alpha(valid.mulRaw[,stressNames], keys = stressKeys)  # calculate the alpha coefficient 
print(stressAlpha$total)  # 0.6778  Not right
mulDatasum$stress <- rowSums(valid.mulRaw[,stressNames],na.rm = T)/length(stressNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,stressNames]
        tmpAlpha <- psych::alpha(tmpdf,keys = stressKeys)
        sitesAlpha$alphaStress[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for attach phone
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )

phoneAlpha <- psych::alpha(valid.mulRaw[,phoneNames], 
                            keys=c(1,2,3,4,5,6,7,8,9))  # calculate the alpha coefficient 
print(phoneAlpha$total)  # std. alpha 0.8868
mulDatasum$phone <- rowSums(valid.mulRaw[,phoneNames],na.rm = T)/length(phoneNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,phoneNames]
        tmpAlpha <- psych::alpha(tmpdf, 
                                 keys= c(1,2,3,4,5,6,7,8,9)) 
        sitesAlpha$alphaPhone[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for online
onlineNames <- c( "onlineid1", "onlineid2","onlineid3","onlineid4", "onlineid5", "onlineid6","onlineid7","onlineid8",
                 "onlineid9", "onlineid10", "onlineide11")

onlineAlpha <- psych::alpha(valid.mulRaw[,onlineNames], 
                           keys=c(1,2,3,4,5,6,7,8,9,10,11))  # calculate the alpha coefficient 
print(onlineAlpha$total)  # std. alpha 0.8977
mulDatasum$online <- rowSums(valid.mulRaw[,onlineNames],na.rm = T)/length(onlineNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,onlineNames]
        tmpAlpha <- psych::alpha(tmpdf, 
                                 keys= c(1,2,3,4,5,6,7,8,9,10,11)) 
        sitesAlpha$alphaOnline[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha


## score and alpha for ECR
ECRNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18","ECR19","ECR20","ECR21","ECR22",
               "ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29","ECR30","ECR31","ECR32","ECR33",
               "ECR34","ECR35","ECR36")
# ECRKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18,
#              19,-20,21,-22,23,24,25,-26,-27,-28,-29,-30,-31,32,-33,-34,-35,-36) # original reverse coding
ECRKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
             19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36) # reverse coded as negative

ECRAlpha <- psych::alpha(valid.mulRaw[,ECRNames], 
                            keys=ECRKeys)  # calculate the alpha coefficient 
print(ECRAlpha$total)  # std. alpha 0.776, instead of 0.932
mulDatasum$ECR <- rowSums(valid.mulRaw[,ECRNames],na.rm = T)/length(ECRNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,ECRNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= ECRKeys) 
        sitesAlpha$alphaECR[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for ECR Anxiety
anxietyNames <- c( "ECR1", "ECR2", "ECR3", "ECR4","ECR5", "ECR6", "ECR7", "ECR8", "ECR9", "ECR10", "ECR11",
               "ECR12","ECR13","ECR14","ECR15","ECR16", "ECR17","ECR18")
# anxietyKeys <- c(1,2,3,4,5,6,7,8,-9,10,-11,12,13,14,15,16,17,18) # reverse coded as negative
anxietyKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) 

anxietyAlpha <- psych::alpha(valid.mulRaw[,anxietyNames], 
                         keys=anxietyKeys)  # calculate the alpha coefficient 
print(anxietyAlpha$total)  # std. alpha 0.876, instead of 0.92
mulDatasum$anxiety <- rowSums(valid.mulRaw[,anxietyNames],na.rm = T)/length(anxietyNames) # average score

# alpha for each site
for (i in siteName){
        tmpdf <- valid.mulRaw[valid.mulRaw$Site == i,anxietyNames]
        tmpAlpha <- psych::alpha(tmpdf, keys= anxietyKeys) 
        sitesAlpha$alphaECRAnxiety[sitesAlpha$sites == i] <- as.numeric(tmpAlpha$total[2]) # chose the Standard alpha
}
sitesAlpha

## score and alpha for ECR avoidance
avoidanceNames <- c( "ECR19","ECR20","ECR21","ECR22","ECR23","ECR24","ECR25","ECR26","ECR27","ECR28","ECR29",
                   "ECR30","ECR31","ECR32","ECR33", "ECR34","ECR35","ECR36")
# avoidanceKeys <- c(1,-2,3,-4,5,6,7,-8,-9,-10,-11,-12,-13,14,-15,-16,-17,-18) # reverse coded as negative
avoidanceKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

avoidanceAlpha <- psych::alpha(valid.mulRaw[,avoidanceNames], 
                             keys=avoidanceKeys)  # calculate the alpha coefficient 
print(avoidanceAlpha$total)  # std. alpha 0.838, instead of 0.916
mulDatasum$avoidance <- rowSums(valid.mulRaw[,avoidanceNames],na.rm = T)/length(avoidanceNames) # average score


## score and alpha for nostaglia
nostagliaNames <- c( "SNS1" ,"SNS2","SNS3","SNS4", "SNS5","SNS6" ,"SNS7" )
# nostagliaKeys <- c(-1,2,3,4,5,6,7) # reverse coded as negative
nostagliaKeys <- c(1,2,3,4,5,6,7)
nostagliaAlpha <- psych::alpha(valid.mulRaw[,nostagliaNames], 
                               keys=nostagliaKeys)  # calculate the alpha coefficient 
print(nostagliaAlpha$total)  # std. alpha 0.765, instead of 0.92
nostagliaItem <- psych::scoreItems(nostagliaKeys,valid.mulRaw[,nostagliaNames],min = 1, max = 7) ## 

## score and alpha coefficient for ALEX
didfNames <- c("ALEX1","ALEX2","ALEX3","ALEX4","ALEX5" ,"ALEX6", "ALEX7", "ALEX8", "ALEX9" ,"ALEX10","ALEX11")
#didfKeys <- c(1,2,3,-4,5,6,7,8,9,10,11) # original
didfKeys <- c(1,2,3,4,5,6,7,8,9,10,11)

eotNames <- c("ALEX12","ALEX13","ALEX14","ALEX15" ,"ALEX16")
# eotKeys <- c(-1,2,-3,4,-5) # original
eotKeys <- c(1,2,3,4,5)

mulDatasum$didf <- rowSums(valid.mulRaw[,didfNames],na.rm = T)/length(didfNames) # average score
didfAlpha <-  psych::alpha(valid.mulRaw[,didfNames], keys=didfKeys)  # calculate the alpha coefficient of DIDF
print(didfAlpha$total)  # print the alpha for DIDF

mulDatasum$eot <- rowSums(valid.mulRaw[,eotNames],na.rm = T)/length(eotNames) # average score
eotfAlpha <-  psych::alpha(valid.mulRaw[,eotNames], keys=eotKeys)  # calculate the alpha coefficient of eot
print(eotfAlpha$total)  # print the alpha for eot

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeKeys <- c(1,2,3,4,5,6,7,8,9) # reverse coded as negative

homeAlpha <- psych::alpha(valid.mulRaw[,homeNames], 
                               keys=homeKeys)  # calculate the alpha coefficient 
print(homeAlpha$total)  # std. alpha 0.9049, instead of 0.901

homeItem <- psych::scoreItems(homeKeys,valid.mulRaw[,homeNames],min = 1, max = 5) ## 

## score and alpha for KAMF

kamfNames <- c("KAMF1" ,"KAMF2","KAMF3","KAMF4","KAMF5","KAMF6","KAMF7")
kamfKeys <- c(1,2,3,4,5,6,7) # reverse coded as negative

kamfAlpha <- psych::alpha(valid.mulRaw[,kamfNames], 
                          keys=kamfKeys)  # calculate the alpha coefficient 
print(kamfAlpha$total)  # std. alpha 0.9049, instead of 0.901

kamfItem <- psych::scoreItems(kamfKeys,valid.mulRaw[,kamfNames],min = 1, max = 5) ## 


##### end ####
>>>>>>> 6da8f7989dcfa6e80fae9e142009262087180203
