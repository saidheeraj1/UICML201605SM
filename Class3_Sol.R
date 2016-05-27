

DataSplit = function(x, targetVar, removeFirst = T, perc = .75){

  #Number of data points in the training set
  nTrain = ceiling(dim(x)[1] * perc)
  
  #Random data points for training set
  idxTrain = sample(x=as.integer(rownames(x)), size=nTrain)
  
  #Get Training and Validation sets
  finalData = {}
  finalData$Train = x[idxTrain,]
  finalData$Valid = x[-idxTrain,]
  
  #Remove index variable
  #Get column index of index variable
  if (removeFirst){
    #Move index values to row names and delete index variable
    rownames(finalData$Train) = finalData$Train[,1]
    finalData$Train = finalData$Train[, -1]
    
    rownames(finalData$Valid) = finalData$Valid[,1]
    finalData$Valid = finalData$Valid[, -1]
  }

  #Get index of Target Variable
  finalData$targetIdx = which(colnames(finalData$Train)==targetVar)
  
  return(finalData)
}

loadData = function(db=T){
  if(db){
    #Set environment variables
    dbConnStr = paste0('driver={SQL Server Native Client 11.0};',
                       'server=.;',
                       'database=UIC2;',
                       'trusted_connection=yes;')
    
    #Set up connection and load data object -- replace by a function
    dbConn = odbcDriverConnect(connection=dbConnStr)
    reportData = sqlQuery(dbConn, 'SELECT * FROM dbo.vw_DataFrame')
    close(dbConn)
  }else{
    reportData = read.csv("report.csv")
    names(reportData)[1] = 'ReportID'
  }
  
  return(reportData)
}


trainRF = function(x.train, x.valid, targetVar, threshold=0.9, ...){
  trainFormula = formula(paste0(targetVar, " ~ ."))
  rfM = randomForest(formula=trainFormula,
                     data=x.train, ...)
  
  #Cross-Validation
  targetIdx = which(names(x.valid)==targetVar)
  predMat = predict(object=rfM,
                    newdata=x.valid[,-targetIdx],
                    type='prob')
  
  predMax = apply(X=predMat, MARGIN=1, FUN=max)
  predCol = max.col(m=predMat, ties.method='first')
  predLab = colnames(predMat)[predCol]
  
  validObj = {}
  validObj$Prob = predMax
  validObj$Label = predLab
  validObj$Pred = validObj$Label
  validObj$Pred[validObj$Prob<threshold] = 'N/A'
  validObj$Actual = as.character(x.valid[,targetVar])
  validObj$predPerc = sum(validObj$Pred!='N/A')/length(validObj$Label)
  validObj$errorRate = 
    sum(validObj$Pred!=validObj$Actual & validObj$Pred!='N/A')/
    sum(validObj$Pred!='N/A')
  
  sprintf('Rate of Prediction: %f, Error rate: %f',
          validObj$predPerc, validObj$errorRate)
  
  rfMod = {}
  rfMod$mod = rfM
  rfMod$valid = validObj[1:4]
  rfMod$errorRate = validObj$errorRate
  rfMod$predPerc = validObj$predPerc
  
  return(rfMod)
}

