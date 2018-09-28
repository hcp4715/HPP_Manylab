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
# Revised data: 'HPP_mul_site_0627.csv' (with codebook 'Codebook_HPP_mul_sites_0612.xlsx')
#       We thanks Jixin Yin for check the data and prepare the code book.
# 
### output file and Variables ####
#
# output file: 'Data_Raw_HPP_Multi_Site_Share.csv'
# 
# including raw data for following variables:
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
# CSI            -- complex social integration, social network(diversity); Cohen et al., 1997
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

# packages, if not exist, install.
pkgTest <- function(x){
        if (!require(x,character.only = TRUE)){
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

# packages
pkgNeeded <- c("psych",'tidyverse','foreign')

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

if(length(new.packages)) install.packages(new.packages)

# this belowing code was not used.
## to read spss file with duplicated labels, fixed this error from: https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
# Int2Factor <- function(x)
# {
#         if(!is.null(attr(x, "value.labels"))){
#                 vlab <- attr(x, "value.labels")
#                 if(sum(duplicated(vlab)) > 0)
#                         cat("Duplicated levels:", vlab, "\n")
#                 else if(sum(duplicated(names(vlab))) > 0)
#                         cat("Duplicated labels:",
#                             names(vlab)[duplicated(names(vlab))], "\n")
#                 else
#                         x <- factor(x, levels = as.numeric(vlab),
#                                     labels = names(vlab))
#         }
#         x
# }

# mul_data_raw <- read.spss("penguin v1d_7f.sav", use.value.labels = FALSE)
# mul_data_raw <- lapply(mul_data_raw, Int2Factor)
# mul_data_raw <- as.data.frame(mul_data_raw, stringsAsFactors = FALSE)

# attach(dataset.cleaned)

# read data
# raw data
mulDataRaw <- read.csv("HPP_mul_site_0627.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# data reported on osf
repoData1 <- read.csv("Multi_site_confirmpenguin_share.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
colnames(repoData1)[colnames(repoData1) == 'ï..age'] <- 'age'
colnames(repoData1)[colnames(repoData1) == 'deq'] <- 'DEQ'
repoData1$filter_. <- NA
repoData1 <- repoData1[ , order(names(repoData1))]

#repoData1_s <- repoData1[,c("age",'cbt','deq',"attachphone","attachhome","langfamily","networksize","csi","socialembedded")]
#colnames(repoData1_s) <- c("age",'avgtemp','DEQ',"attachphone","attachhome","langfamily","networksize","csi","socialembedded")

repoData2 <- read.csv("Multi_site_explorepenguin_share.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
colnames(repoData2)[colnames(repoData2) == 'ï..age'] <- 'age'
colnames(repoData2)[colnames(repoData2) == 'anxi']   <- 'anxiety'
colnames(repoData2)[colnames(repoData2) == 'home']   <- 'attachhome'
colnames(repoData2)[colnames(repoData2) == 'humid']   <- 'AvgHumidity'
colnames(repoData2)[colnames(repoData2) == 'eng']   <- 'english'
colnames(repoData2)[colnames(repoData2) == 'avoid']   <- 'avoidance'
colnames(repoData2)[colnames(repoData2) == 'monog']   <- 'monogamous'
colnames(repoData2)[colnames(repoData2) == 'norweg']   <- 'norwegian'
colnames(repoData2)[colnames(repoData2) == 'nwsize']   <- 'networksize'
colnames(repoData2)[colnames(repoData2) == 'pol']   <- 'polish'
colnames(repoData2)[colnames(repoData2) == 'selfc']   <- 'selfcontrol'
colnames(repoData2)[colnames(repoData2) == 'serb']   <- 'serbian'
colnames(repoData2)[colnames(repoData2) == 'turk']   <- 'turkish'
colnames(repoData2)[colnames(repoData2) == 'weight']   <- 'weightkg'
repoData2 <- repoData2[ , order(names(repoData2))]

#repoData2_s <- repoData2[,c("age",'cbt','DEQ',"attachphone","home","langfamily","nwsize","csi","socialembedded")]
#colnames(repoData2) <- c("age",'avgtemp','DEQ',"attachphone","attachhome","langfamily","networksize","csi","socialembedded")

# combine the results from both exploration and conformation datasets
repoData  <- rbind(repoData2,repoData1)  # N = 1523, equal to raw data, that's no data were removed
colnames(repoData)[colnames(repoData) == 'cbt']   <- 'avgtemp'
repoData_reord <- repoData[with(repoData, order(age,avgtemp,attachphone,attachhome)), ] # order based on "age", "anxiety", and "avoidance"

# exclude participants no performed in reported version
# criteria:average temperation is greater than 34.99
# valid.mulRaw <- subset(mulDataRaw,avgtemp > 34.99) # average temperature higher than 34.99 is valid

# recode the temperature:
# exclude participants
# criteria: 
# c1: average temperation is greater than 34.99
# c2: not drink or eat somethin cold or warm in 10 minutes before (eatdrink = 1)
# c3: no exercise in 60 mintues before the survey (exercise = 2)

# first: filter eatdrinking
#valid.data_Eat <- subset(mulDataRaw, eatdrink != 1)   # eat or drink  (38 participants)
#valid.data_NA <- subset(mulDataRaw, is.na(eatdrink))  # eat or drink data is NA (31 participants)
#valid.data_NoEat <- subset(mulDataRaw, eatdrink == 1) # No eat of drink

# Second: filter exercise
#valid.data_exercise <- subset(valid.data_NoEat, exercise != 2)      # exercised within one hour (19 participants)
#valid.data_exercise_NA <- subset(valid.data_NoEat, is.na(exercise)) # missing exercise data (0 participant)
#valid.data_NoExercise <- subset(valid.data_NoEat, exercise == 2)    # did not exercise within one hour

# Third: filter average temperature
#valid.data_Tmp <- subset(valid.data_NoExercise, avgtemp < 34.99)  # participant that not excluded by the other two criteria (0 participant)

#valid.data <- subset(mulDataRaw,avgtemp > 34.99 & eatdrink == 1 & exercise == 2) # average temperature higher than 34.99 is valid

## this means: if we applied the no ead or drink and no exercise criteria, 1435 participant left.

#### detecting the differences ####
# detect differences between osf data and my data
## calculated the anxiety and attachhome score for re-ordering

## score and alpha for attach phone ####
phoneNames <- c( "phone1", "phone2","phone3", "phone4","phone5", "phone6","phone7","phone8","phone9" )
attachphoneScore <- psych::scoreItems(phoneNames,mulDataRaw[,phoneNames], min = 1, max = 5) # mean score
mulDataRaw$attachphone <- attachphoneScore$scores

## score and alpha for attachemnt to home
homeNames <- c( "HOME1","HOME2","HOME3","HOME4","HOME5","HOME6","HOME7","HOME8","HOME9" )
homeScore <- psych::scoreItems(homeNames,mulDataRaw[,homeNames], min = 1, max = 5) # mean score
mulDataRaw$attachhome <- homeScore$scores

## re-order the data
mulDataRaw_reord <- mulDataRaw[with(mulDataRaw, order(age, avgtemp,attachphone,attachhome)), ] # order based on "age", "attachphone", and "attachhome"
mulDataRaw_reord_s <- mulDataRaw_reord[,c()]

## compare my data with the osf, using avgtemp as the index:

identical(round(mulDataRaw_reord$avgtemp,2),round(repoData_reord$avgtemp,2)) # true

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
kamfNames <- c("KAMF1" ,"KAMF2","KAMF3","KAMF4","KAMF5","KAMF6","KAMF7")
straqNames <- paste('STRAQ', 1:57,sep = '_')

OtherNames <- c("Site",'age','sex','sexpref','romantic','monogamous',
                'heightm','weightkg','health','meds',
                'gluctot',"artgluctot","smoke","cigs", "eatdrink","exercise", 
                'Temperature_t1','Temperature_t2',
                'avgtemp','AvgHumidity','mintemp','endtime',
                'language', "langfamily")

selectNames <- c(OtherNames,SNINames,scontrolNames,stressNames,phoneNames,onlineNames,ECRNames,homeNames,nostagliaNames,didfNames,eotNames,kamfNames,straqNames)

mulDataRaw_share <- mulDataRaw_reord[,selectNames]

# correct the site name for "tsinghua"
mulDataRaw_share$Site[mulDataRaw_share$Site == 'Tsinhua'] <- 'Tsinghua'

mulDataRaw_share$DEQ <- repoData_reord$DEQ
mulDataRaw_NO_share <- mulDataRaw_share
mulDataRaw_NO_share$longitude <- mulDataRaw_reord$LocationLongitude

mulDataRaw_share <- mulDataRaw_NO_share[, -which(names(mulDataRaw_NO_share) %in% c("sexpref","heightm","weightkg",'longitude','endtime','meds'))]
# correct the site name for "tsinghua"

mulDataRaw_share <- mulDataRaw_share %>%
        dplyr::select("Site","age","sex","monogamous", "romantic","health",
                      "exercise","eatdrink","gluctot",'artgluctot',"smoke",'cigs', 
                      "avgtemp","Temperature_t1","Temperature_t2",
                      "DEQ","AvgHumidity","mintemp","language","langfamily",
                      everything())

# save the re-ordered summary data from OSF, i.e., reported in our manuscript.
write.csv(repoData_reord,'mult_site_from_OSF_ordered.csv',row.names = F)

# write the data
write.csv(mulDataRaw_share,'Data_Raw_HPP_Multi_Site_Share.csv',row.names = F)        # can be open later
write.csv(mulDataRaw_NO_share,'Data_Raw_HPP_Multi_Site_No_Share.csv',row.names = F)  # can not be open later

##### end ####

