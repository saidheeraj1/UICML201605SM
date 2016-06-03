#Clear environment
rm(list=ls()[which(ls() != 'd')])

#Load library
library(googleVis)

#Load loan data
d <- read.csv("c:/temp/loan.csv")

#Keep needed variables
df <- d[,c("member_id","addr_state","loan_amnt","grade","emp_length","issue_d",
           "int_rate", "home_ownership","verification_status","annual_inc",
           "purpose")]

#Keep the year in date####
#Aggregate and find sum of loan amount
df$issue_d <- as.character(df$issue_d)
df$year <- as.numeric(substr(df$issue_d,
                             nchar(df$issue_d)-3, nchar(df$issue_d)))
loan_amnt <- aggregate(df$loan_amnt, by=list(df$addr_state, df$year),
                       FUN=sum)
names(loan_amnt) <- c("addr_state","year","totalamount")

#Sort data for viewing - optional
loan_amnt <- loan_amnt[order(loan_amnt$addr_state, loan_amnt$year),]

#Aggregate and find mean of annual income and interest rate
int_income <- aggregate(df[,c("annual_inc","int_rate")],
                        by=list(df$addr_state, df$year),
                        FUN=mean)
names(int_income) <- c("addr_state","year","mean_income","mean_int")

#Sort data for viewing - optional
int_income <- int_income[order(int_income$addr_state, int_income$year),]

#Combine tables
finalDF = merge(loan_amnt, int_income, 
                by = intersect(names(loan_amnt), names(int_income)))
finalDF$addr_state <- as.character(finalDF$addr_state)

#Plot
motion <- gvisMotionChart(finalDF, idvar = "addr_state",
                          timevar = "year", sizevar = "totalamount",
                          xvar = "mean_int", yvar = "mean_income",
                          options=list(
                            chartArea=
                              '{left:100, top:100, width:"75%", height:"75%"}',
                            height=700,
                            width=1500
                          ))

plot(motion)


#Filter top 10 states
loan_bystate <- aggregate(df$loan_amnt, by=list(df$addr_state), FUN=sum)
loan_bystate <- loan_bystate[order(loan_bystate$x, decreasing=T),]
names(loan_bystate) <- c("state","totalamount")
loan_bystate$state <- as.character(loan_bystate$state)
topStates <- head(loan_bystate$state, 10)

finalDF <- finalDF[which(finalDF$addr_state %in% topStates),]

#Plot motion chart
motion <- gvisMotionChart(finalDF, idvar = "addr_state",
                          timevar = "year", sizevar = "totalamount",
                          xvar = "mean_int", yvar = "mean_income",
                          options=list(
                            chartArea=
                              '{left:100, top:100, width:"75%", height:"75%"}',
                            height=700,
                            width=1500
                          ))

plot(motion)


#Geochart
geoS <- gvisGeoChart(loan_bystate, "state", "totalamount",
                     options=list(region="US",
                                  displayMode="regions",
                                  resolution="provinces",
                                  width=1200, height=700))

plot(geoS)


#Calendar Chart
loan_bydate <- aggregate(df$loan_amnt, by=list(df$issue_d), FUN=sum)
names(loan_bydate) <- c("date","totalamount")
loan_bydate$date <- paste0("01-",loan_bydate$date)
loan_bydate$date <- as.Date(loan_bydate$date, format= "%d-%b-%Y")

calChart <- gvisCalendar(loan_bydate, 
                         datevar="date",
                         numvar="totalamount",
                         options=list(
                           title="Monthly loan amounts",
                           height=1200,
                           calendar="{yearlabel: { fontName: 'Times-Roman',
                              fontSize: 32, color: '#1A8763', bold: true},
                              cellSize: 10,
                              cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                              focusedCellColor: { stroke: 'red' } }"))

plot(calChart)

#Notice that only the first day of the month is colored since we only 
#have first-of-the-month dates in the data set.
