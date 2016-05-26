targetVar = 'NextReport'

predThreshold = 0.9

trainPerc = 0.75

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

loadData = function(){
  
  #Set environment variables
  dbConnStr = paste0('driver={SQL Server Native Client 11.0};',
                     'server=.;',
                     'database=UIC2;',
                     'trusted_connection=yes;')
  
  #Set up connection and load data object -- replace by a function
  dbConn = odbcDriverConnect(connection=dbConnStr)
  reportData = sqlQuery(dbConn, 'SELECT * FROM dbo.vw_DataFrame')
  close(dbConn)
  
  return(reportData)
}