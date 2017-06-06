# comrade_trend
# Computes the trends signal of the UK election polls. Uses a local linear approximation, 
# which assumpes that trends are constant, rather than the mean is constant, but noisy.

require("htmltab") # https://cran.r-project.org/package=htmltab

url = "Opinion polling for the United Kingdom general election, 2017 - Wikipedia.htm" # I downloaded the wikipedia page to my working directory and removed everything in the body before the table, not sure if that was necessary, but initially I could not get htmltab running (my first time).
whichStr = "//th[text() = 'date(s)']"
fileName = "fig10_uk_election_polls.png"

table = htmltab(doc = url, which = 1) # whichStr)
dateStr = table[[1]]

noRows = length(table$Con)
percentageCon = vector("numeric", noRows)
percentageLab = vector("numeric", noRows)

for(iRow in 1:noRows) {
  temp = table$Con[iRow]
  temp2 = unlist(strsplit(temp, "%"))
  print(temp2)
  percentageCon[iRow] = as.numeric(temp2)
  
  temp = table$Lab[iRow]
  temp2 = unlist(strsplit(temp, "%"))
  print(temp2)
  percentageLab[iRow] = as.numeric(temp2)
}
index = is.na(percentageCon)==FALSE

percentageCon = subset(percentageCon, index)
percentageLab = subset(percentageLab, index)
noRows = length(percentageCon)

date = seq( as.Date("2000-01-01"), by=1, len=noRows)
Sys.setlocale("LC_TIME", "UK")

temp = table[[1]] # Date
temp = subset(temp, index)
year = 2017
for(iRow in 1:noRows) {
  temp1 = temp[iRow]
  # Check if the date starts with "Pre-", if yes remove this and subtract one day from the date
  if( is.na(pmatch("Pre-", temp1)) == FALSE) {
    temp2 = substr(temp1, start=5, stop=nchar(temp1))
    timeDelay = 1;
  } else {
    temp2 = temp1
    timeDelay = 0;
  }
  # Check if the date gives a range
  temp3 = strsplit(temp2, "–")
  temp4 = unlist(temp3)
  if(length(temp4) == 2) {
    temp5 = strptime(paste(temp4[2], year), format = "%d %b %Y") # end date of polling period
    temp6 = strptime(paste(temp4[1], year), format = "%d %b %Y") # begin date of polling period, if just a number this will fail and the next IF is TRUE
    if(is.na(temp6)) {
      temp6a = paste0(  substr(temp4[1], start=1, stop=2), substr( temp4[2], start=3, stop=nchar(temp4[2]) )  )
      temp6 = strptime(paste(temp6a, year), format = "%d %b %Y")
    }
    date[iRow] = mean(c(temp5, temp6))
    a=0
  } else {
    temp5 = strptime(paste(temp4, year), format = "%d %b %Y")
    date[iRow] = temp5 - timeDelay
    print(temp5)
  }
  a=0
}

# Generate plot, first make linear prediction based on a recent period of data.
dateElection = as.Date("2017-06-08")
dateCut = as.Date("2017-05-01")
index = which(date > dateCut)
dates = seq(from=dateCut, to=dateElection, by=1)
dateCut = date[index]
conCut = percentageCon[index]
labCut= percentageLab[index]
conFit = lm(conCut ~ dateCut)
labFit = lm(labCut ~ dateCut)

conPredict = predict(conFit, data.frame(dateCut = dates))
labPredict = predict(labFit, data.frame(dateCut = dates))

# Compute LOESS smoothed data
percentageCon.lo = loess(percentageCon ~ as.numeric(date), span = 0.5, degree=1)
conSmooth = predict(percentageCon.lo, sort(date))

percentageLab.lo = loess(percentageLab ~ as.numeric(date), span = 0.5, degree=1)
labSmooth = predict(percentageLab.lo, sort(date))

png(file=fileName, 700, 700, type="cairo")
par(mar=c(2,5,3,2)) # +0.1)

xMin = min(date)
xMax = dateElection + 25

maxy= max(c(percentageCon, percentageLab))
miny= min(c(percentageCon, percentageLab))
yMax = 10*ceiling(maxy/10)
yMin = 10*floor(miny/10)

yLabStr = "Percentage"
plot(date, percentageLab, type="p", pch=1, col="darkred", lwd=1, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
     cex.main=1.5, xlab="Date", ylab=yLabStr, main="UK election polls", ylim=c(yMin, yMax), xlim=c(xMin, xMax)) 
points(date, percentageCon, pch=1, col="darkblue", cex=1.5)

lines(sort(date), labSmooth, col="darkred", lwd=4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)          
lines(sort(date), conSmooth, col="darkblue", lwd=4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)          

labPredict = as.numeric(labPredict[length(labPredict)])
conPredict = as.numeric(conPredict[length(conPredict)])
points(dateElection, labPredict, col="darkred",  pch=4, lwd=4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(dateElection, conPredict, col="darkblue", pch=4, lwd=4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

labPredictStr = sprintf("%.1f", labPredict)
conPredictStr = sprintf("%.1f", conPredict)
text(dateElection+8, labPredict, labPredictStr, adj=c(0,NA), col="darkred", font=2)
text(dateElection+8, conPredict, conPredictStr, adj=c(0,NA), col="darkblue", font=2)

grid()
dev.off()
