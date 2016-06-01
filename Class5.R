rm(list=ls())

#Package Vignette
# install.packages("googleVis")
library(googleVis)
data("Fruits")
Fruits

M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
                     options=list(
                       chartArea=
                         '{left:100, top:100, width:"75%", height:"75%"}',
                       height=700,
                       width=1500
                       )
                     )

plot(M)

M$type
M$chartid
M$html$header #non-formatted screen output
print(M, tag='header') #to achive formatted screen output
names(M$html$chart)
print(M, tag='chart')
cat(M$html$chart['jsChart'])
print(M, tag='caption')
print(M, tag='footer')


#RCA project
rm(list=ls())

#Load data
reportData = read.csv("Report.csv")
names(reportData)[1] = "ReportID"

#Bubble Chart####

#Prepare data frame to graph
n = 10
t = as.data.frame(table(reportData$NextReport))
t = head(t[order(t$Freq, decreasing=T),], n)
names(t) = c("NextReport.str", "N")
t$NextReport = as.numeric(t$NextReport.str)


#Prepare the chart's HTML - all vars NUMERIC
bubble = gvisBubbleChart(t, idvar="NextReport",
                         xvar="NextReport", yvar="NextReport",
                         colorvar="NextReport",
                         sizevar="N"
)
#Plot chart in browser
plot(bubble)


#Prepare the chart's HTML - change colorvar to FACTOR - notice the error
bubble = gvisBubbleChart(t, idvar="NextReport",
                         xvar="NextReport", yvar="NextReport",
                         colorvar="NextReport.str",
                         sizevar="N"
)
plot(bubble)


#Prepare the chart's HTML - change colorvar and idvar to FACTOR - error fixed
bubble = gvisBubbleChart(t, idvar="NextReport.str",
                         xvar="NextReport", yvar="NextReport",
                         colorvar="NextReport.str",
                         sizevar="N"
)
plot(bubble)


#Prepare the chart's HTML - expand axis limits
bubble = gvisBubbleChart(t, xvar="NextReport", yvar="NextReport",
                         colorvar="NextReport.str", idvar="NextReport.str",
                         sizevar="N",
                         options=list(
                           hAxis='{minValue:0, maxValue:50}',
                           vAxis='{minValue:0, maxValue:50}')
)
plot(bubble)


#Prepare the chart's HTML - expand the graph height
bubble = gvisBubbleChart(t, xvar="NextReport", yvar="NextReport",
                         colorvar="NextReport.str", idvar="NextReport.str",
                         sizevar="N",
                         options=list(
                           hAxis='{minValue:0, maxValue:50}',
                           vAxis='{minValue:0, maxValue:50}',
                           height=700)
)
#Plot chart in browser
plot(bubble)


#Prepare the chart's HTML - specify chart area dimensions and bubble size
bubble = 
  gvisBubbleChart(t, xvar="NextReport", yvar="NextReport",
                  colorvar="NextReport.str", idvar="NextReport.str",
                  sizevar="N",
                  options=list(
                    chartArea='{left:100, top:50, width:"80%", height:"80%"}',
                    hAxis='{minValue:0, maxValue:50}',
                    vAxis='{minValue:0, maxValue:50}',
                    sizeAxis='{minValue:20, maxSize:60}',
                    height=700)
  )
#Plot chart in browser
plot(bubble)


#Prepare data frame with number of reports
library(psych)

n = 10
byVar = 'UserID'

t = describeBy(
  x=reportData[,c('NextReport','ReportMonth','UserID')],
  group=reportData$ReportMonth, mat=T, digits=0)
names(t)[2] = "NextReport"

t.tc = 
  t[grep(byVar, rownames(t)), c('NextReport','n','mean','sd')]

t = head(t.tc[order(t.tc$n, decreasing=T),], n)


#Prepare the chart's HTML - add titles
bubble = 
  gvisBubbleChart(t, idvar="NextReport", xvar="mean", yvar="sd",
                  colorvar="NextReport", sizevar="n",
                  options=list(
                    chartArea='{left:100, top:80, width:"75%", height:"75%"}',
                    sizeAxis='{minValue:30, maxSize:60}',
                    height=700,
                    title=paste0('Next Report Label by ', byVar),
                    titleTextStyle='{fontSize:24}',
                    hAxis="{title: 'Mean',
                    titleTextStyle:{fontSize:20}}",
                    vAxis="{title: 'Std. Dev.',
                    titleTextStyle:{fontSize:20}}")
                  )
#Plot chart in browser
plot(bubble)



#Geographic Charts
library(datasets)
states = data.frame(state.name, state.x77)
GeoStates = gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=1200, height=700))
plot(GeoStates)



