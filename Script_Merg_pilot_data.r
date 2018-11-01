# merge the summary of pilot data for both m turk and prolific academic

# read files
df_mturk <- read.csv('summaryMTurk.csv', header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
df_PA <- read.csv('summaryProflificAcd.csv', header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
df_pilot <- read.csv('pilotpenguins_hans.csv', header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

df_pilot.m <- subset(df_pilot, Site == 1)
df_pilot.p <- subset(df_pilot, Site == 2)
