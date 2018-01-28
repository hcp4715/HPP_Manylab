## to merge mintemp and avghumid from the data online

# read data
pilot_PA <- read.csv("prolific_academic_corrected_201512_rev_yjx2_3.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
namesBasic <- c("heightm", "Sex","weightkg")

pilot_PA_basic <- pilot_PA[,namesBasic]
pilot_PA_basic$site <- 'PA'

pilot_MT <- read.csv("HPP_MTurk_cleaned_valid.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

pilot_MT_basic <- pilot_MT[,c("heightm", "sex","weightkg")]
colnames(pilot_MT_basic) <- namesBasic

pilot_MT_basic$site <- 'MT'

pilot_basic <- rbind(pilot_PA_basic,pilot_MT_basic)

pilot_basic_sort <- pilot_basic[order(pilot_basic$heightm, na.last = F),]

pilot_basic2 <- Dataset[,c("heightm", "Sex","weightkg",'Site')] 
