#Author:  Sam Mourad
#Date:    1/9/2016
#Notes:
#   Script to contain utility functions including data load.

#Clear environment
rm(list=ls())

#Load Libraries
library(RODBC)
source("Class3_Sol.R")

targetVar = 'NextReport'

predThreshold = 0.9

trainPerc = 0.75

#Set Variables####
reportData = loadData(db=T)

dObj = DataSplit(reportData, "NextReport")

# #Shuffle/split data for cross-validation
# dObj = splitData(x=reportData, perc=0.8,
#                  targetVar=targetVar, indexVar='ReportID')


#Check labels of Target Variable
length(unique(dObj$Train[,dObj$targetIdx]))

tbt = as.data.frame(table(dObj$Train[,dObj$targetIdx]))
tbt = tbt[order(tbt$Freq, decreasing=T),]
View(tbt)

tbv = as.data.frame(table(dObj$Valid[,dObj$targetIdx]))
tbv = tbv[order(tbv$Freq, decreasing=T),]
View(tbv)

rm(list=c('tbt','tbv'))


#Model Training####

#Train randomForest
library(randomForest)
rfMod = trainRF(dObj$Train, dObj$Valid, targetVar, predThreshold)

# #Remove classes with low count
# tb = as.data.frame(table(reportData$NextReport))
# tb = tb[order(tb$Freq, decreasing=T),]
# tb$cumperc =
#   apply(as.data.frame(tb[,"Freq"]), 2, function(x) cumsum(x)/sum(x))
# tb = tb[which(tb$cumperc<=.99),]

# #Train extraTrees
# library(extraTrees)
# 
# tgtIdx = which(colnames(dObj$Train)==targetVar)
# etMod = extraTrees(x=dObj$Train[,-tgtIdx], 
#                    y=dObj$Train[,tgtIdx],
#                    ntree=100,
#                    mtry=6,
#                    numRandomCuts=10,
#                    numThreads=1)

sprintf('Rate of Prediction: %f, Error rate: %f',
        rfMod$predPerc, predObj$errorRate)

#Graph Error Rate and Rate of Prediction against Prediction Threshold
threshmat = matrix(NaN, nrow=101, ncol=3)
colnames(threshmat) = c('Thresh', 'Error', 'Pred')
threshstep = 1/(dim(threshmat)[1] - 1)

for (i in 1:dim(threshmat)[1]){
  threshmat[i, 1] = (i-1) * threshstep
  ntot = length(predObj$Label)
  npred = sum(predObj$Prob >= threshmat[i,1])
  ncorrect = sum(predObj$Prob>=threshmat[i,1] & predObj$Label!=predObj$Actual)
  threshmat[i,2] = ncorrect / npred
  threshmat[i,3] = npred / ntot
}

x11(8,6)
#p = recordPlot()
par(mar=c(5,4,4,6) + 0.1)
plot(x=threshmat[,1], y=threshmat[,2], type='l',
     main='Prediction Threshold',
     col='#FF4500', axes=FALSE, xlab="Threshold", ylab="", pch=1)
axis(side=1, at=threshmat[,1][
  seq(1, length(threshmat[,1]), (length(threshmat[,1]) - 1)/10)])
axis(side=2, ylim=c(0,max(threshmat[,2])), col=c('#FF4500','#FF4500'),
     col.axis='#FF4500')
mtext(side=2, text='Error Rate', col='#FF4500', line=2.5)
par(new=TRUE)
plot(x=threshmat[,1], y=threshmat[,3], type='l',
     col='#008000', axes=FALSE, xlab="", ylab="", pch=2)
axis(side=4, ylim=c(0,max(threshmat,3)), col=c('#008000','#008000'),
     col.axis='#008000')
mtext(side=4, text='Prediction Rate', col='#008000', line=2.5)
legend("topright", legend=c('Error Rate','Prediction Rate'),
       text.col=c('#FF4500','#008000'), lty = 1, #pch=c(1,2),
       col=c('#FF4500','#008000'))
box()
plot(p)

#Model Tuning####
mtryVar = c(6, 12)
ntreeVar = c(50, 100)

tuninggrid = merge(x=mtryVar, y=ntreeVar, all=TRUE)
colnames(tuninggrid) = c('mtry','ntree')
tuninggrid$error = rep(NaN, length(tuninggrid$mtry))
tuninggrid$perc = rep(NaN, length(tuninggrid$mtry))

for (i in 1:dim(tuninggrid)[1]){
  rfMod = randomForest(NextReport ~ .,
                       data=dObj$Train,
                       importance=T,
                       proximity=F,
                       mtry=tuninggrid$mtry[i],
                       ntree=tuninggrid$ntree[i])
  
  predMat = predict(object=rfMod,
                    newdata=dObj$Valid[,-dObj$targetIdx],
                    type='prob')
  
  predMax = apply(X=predMat, MARGIN=1, FUN=max)
  predCol = max.col(m=predMat, ties.method='first')
  predLab = colnames(predMat)[predCol]
  
  predObj = {}
  predObj$Prob = predMax
  predObj$Label = predLab
  predObj$Pred = predObj$Label
  predObj$Pred[predObj$Prob<predThreshold] = 'N/A'
  predObj$Actual = as.character(dObj$Valid[, targetVar])
  predObj$predPerc = sum(predObj$Pred!='N/A')/length(predObj$Label)
  predObj$errorRate = 
    sum(predObj$Pred!=predObj$Actual & predObj$Pred!='N/A')/
    sum(predObj$Pred!='N/A')
  
  tuninggrid$error[i] = predObj$errorRate
  tuninggrid$perc[i] = predObj$predPerc
}


#Generate predictions file####
write.csv(x=predObj,
          file='C:/temp/Predictions.txt',
          row.names=T)



#EDA####
#jpeg("C:/temp/testPlot.jpg")
#par(mar = par("mar") - c(4, 0, 2, 0))
layout(mat = c(1, 2, 3), heights = c(2, 3, 3))
boxplot(dObj$Train$ReportCodeID, horizontal = T)
hist(dObj$Train$ReportCodeID, freq = F)
d = density(dObj$Train$ReportCodeID)
lines(x = d, col = 'red')
box()
grid()
#par(mar = par("mar") + c(3, 0, 2, 0))
plot(dObj$Train$ReportCodeID)





























