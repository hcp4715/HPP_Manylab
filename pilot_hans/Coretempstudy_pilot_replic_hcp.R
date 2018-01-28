## Code accompanying IJzerman et al.
## Some of the code below based on http://www.stanford.edu/~stephsus/R-randomforest-guide.pdf
## Install these packages below first(!) - not all used.


## installing of packages
install.packages("randomForest")
install.packages("foreign")
install.packages("party")
install.packages("tree")
install.packages("lattice")
install.packages("plyr")
install.packages("stargazer")
install.packages("summarytools")

require(foreign)
require(randomForest) # Party is slower but superior for handling categories
require(party)
require(tree) # alternative trees.
require(lattice)
require(plyr)
require(stargazer)
require(summarytools)

#Load Data
Dataset<- read.spss("pilotpenguins.sav", to.data.frame=TRUE)
 Dataset<- read.spss("D:\\HCP_cloud\\Exp.s\\Project6_Open_Science\\OS1_Human_Peguin_Projects\\Data\\Data_for_share\\R_analysis_4_share\\pilot_hans\\pilotpenguins.sav", to.data.frame=TRUE)

attach(Dataset)

setwd("D:\\HCP_cloud\\Exp.s\\Project6_Open_Science\\OS1_Human_Peguin_Projects\\Data\\Data_for_share\\R_analysis_4_share\\pilot_hans\\coretemp")

# Summary of Dataframe
summary<-(dfSummary(Dataset))
print(summary, method="browser") #

#Party models (trees)
set.seed(666)
ctree <- ctree(avgtemp ~ ., data = Dataset)
plot(ctree)
set.seed(667)
ctree2 <- ctree(avgtemp ~ ., data = Dataset)
plot(ctree2)

#Forest
set.seed (1)
data.controls <- cforest_unbiased(ntree=1000, mtry=7)
mycforest<- cforest(avgtemp ~ ., data = Dataset, 
                    control = data.controls)

myvarimp<-varimp(mycforest)
as.data.frame(myvarimp)
write.csv(myvarimp, file= 'myvarimp1-avgtemp.csv')
dev.off()
postscript("dotplot1.eps",  horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed vertical line are significant)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})
dev.off()
y_hat<-predict(mycforest)
write.csv(y_hat, file='y_hat.csv')

#Replication of forest
set.seed (2)
data.controls2 <- cforest_unbiased(ntree=1000, mtry=7)
mycforest2<- cforest(avgtemp ~ ., data = Dataset, 
                     control = data.controls2)
myvarimp2<-varimp(mycforest2)
write.csv(myvarimp2, file= 'myvarimp2-avgtemp.csv')
dev.off()
postscript("dotplot2.eps",  horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
dotplot(sort(myvarimp2), xlab="Variable Importance (predictors to right of dashed vertical line are significant)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp2)), col='red', lty='longdash', lwd=2)})
dev.off()
y_hat2<-predict(mycforest2)
write.csv(y_hat2, file='y_hat2.csv')


require(mleda)
plot_ml(the_data = Dataset[!is.na(Dataset$socialdiversity) & !is.na(Dataset$avgtemp), ],
outcome = "avgtemp",
var_name = c("socialdiversity"),
var_level = c(1),
interact = FALSE,
the_mod = mycforest,
cluster = "socialdiversity")

