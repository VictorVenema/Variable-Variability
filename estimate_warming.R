# estimate_warming 
# This code makes the plots for a blog post to illustrate that linear regression is 
# not a good way to estimate total warming.


## 1. Read data of 2 observational datasets

## 1a. Figure 1, plot data and decompose it into long-term trends and variability. Estimate the size of the variability.
year = vector("list", 2)
names(year) = c("GISS", "BEST")
temp = vector("list", 2)
names(temp) = c("GISS", "BEST")
dataFileName = "temperatureAnnualMeans_GISSTEMP_BEST.RData"
if(file.exists(dataFileName) == FALSE) {
  # Global temperature estimate based on stations only, from GISTEMP.
  # data = read.table("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts.csv", sep=",", skip=1, header=TRUE, na.strings=naStr, colClasses="numeric")
  # Default GISSTEMP LOTI, land ocean temperature index
  naStr = "***"
  data = read.table("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv", 
                    sep=",", skip=1, header=TRUE, na.strings=naStr, colClasses="numeric")
  a=0 # used to set breakpoints while debugging
  year[[1]] = data$Year
  temp[[1]] = data$J.D
  
  # 
  naStr = "NaN"
  data = read.table("http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_summary.txt", 
                    skip=48, header=FALSE, na.strings=naStr, colClasses="numeric")
  yearBest = data$V1
  tempBest = data$V2
  temp[[2]] = tempBest[yearBest>=1880]
  year[[2]] = yearBest[yearBest>=1880]
  
  fileStr = c("GISS",                         "BEST"                          )
  varStr = c("Surface temperature (GISTEMP)", "Surface temperature (Berkeley)")
  save(year, temp, fileStr, varStr, file=dataFileName)
} else {
  load(file=dataFileName)
}


## 1b. Plot the data
fileNames = c("fig1_warming_linear_fit.png", "fig2_quadratic_curve.png")
# iFile = 1
for(iFile in 1:2) {
  fileName = fileNames[[iFile]]
  png(file=fileName, 700, 1200, type="cairo")
  par(mfrow = c(2,1))
  par(mar=c(2,5,1,2)+0.1)
  if(iFile == 2) {
    year[[1]] = 1000:2000
    year[[2]] = 1000:2000
    t = (0:1000)/1000
    temp[[1]] =  t*sqrt(t) # t*t*t #
    # temp[[2]] = sqrt(t) # t*t*t*t
    temp[[2]] = 1-exp(-4*t)
    varStr[[1]] = "Quadratic"
    varStr[[2]] = "Square root"
    varStr[[1]] = "Cubic "
    varStr[[2]] = "Negative exponential"
  }
  
  for(iCase in 1:2) {
    noYears = length(temp[[iCase]])
    # Remove last year, if it is not complete
    if(is.na(temp[[iCase]][noYears])==TRUE) {
      year[[iCase]] = year[[iCase]][1:noYears-1]
      temp[[iCase]] = temp[[iCase]][1:noYears-1]
      noYears = length(temp[[iCase]])
    }

    # Compute LOESS smoothed data
    tempCase = temp[[iCase]]
    yearCase = year[[iCase]]
    
    temp.lo = loess(tempCase ~ yearCase, span = 50/noYears, degree=2)
    tempSmooth = predict(temp.lo, yearCase)
    
    yearBegin = yearCase[1]
    yearEnd = yearCase[length(yearCase)]
    endTempLOESS = predict(temp.lo, yearEnd)
    beginTempLOESS = predict(temp.lo, yearBegin)
    tempDiffLOESS = endTempLOESS - beginTempLOESS
    print(tempDiffLOESS)
    
    # Compute linear regression
    fitLin = lm(tempCase ~ yearCase)
    sumfit_high=summary(fitLin)
    
    beginTempLin = predict(fitLin, data.frame(yearCase=yearBegin))
    endTempLin = predict(fitLin, data.frame(yearCase=yearEnd))
    tempDiffLin = endTempLin - beginTempLin
    print(tempDiffLin)
    
    # Plot data itself
    yMin = min(c(beginTempLin, beginTempLOESS, min(tempCase, na.rm=TRUE)))
    yMax = max(c(endTempLin,   endTempLOESS,   max(tempCase, na.rm=TRUE)))
    yMin = 0.1*floor(yMin*10)
    yMax = 0.1*ceiling(yMax*10)
    
    yLabStr = expression(Temperature~anomaly~(degree*C))
    plot(yearCase, tempCase, type="l", col="gray", lwd=1, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
         cex.main=1.5, xlab="Year", ylab=yLabStr, ylim=c(yMin, yMax)) 
    points(yearCase, tempCase, pch=19, col="darkred", cex=1.5)
    abline(0,0)
    grid()
    
    # Plot data name
    xPos = year[[iCase]][5]
    # maxT = max(temp[[iCase]], na.rm=TRUE)
    # minT = min(temp[[iCase]], na.rm=TRUE)
    maxT = yMax
    minT = yMin
    yPos = maxT - (maxT - minT)*0.05
    text(xPos, yPos, sprintf("%s %d-%d", varStr[[iCase]], year[[iCase]][1], year[[iCase]][noYears]), adj=c(0,0), cex=2, col="darkred")

    # Plot LOESS smoothed data
    lines(year[[iCase]], tempSmooth, col="darkblue", lwd=4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)          
    
    lines(c(yearBegin, yearBegin+60), c(beginTempLOESS, beginTempLOESS), col="darkblue", lwd=4, lty=5)
    lines(c(yearBegin, yearEnd),      c(endTempLOESS,   endTempLOESS),   col="darkblue", lwd=4, lty=5)
    arrows(yearBegin, beginTempLOESS, yearBegin, endTempLOESS, col="darkblue", length=0.15, angle=30, code=3, lwd=3)
    
    x = yearBegin + 5
    y = mean(c(beginTempLOESS, endTempLOESS))
    textStr = sprintf("Nonlinear\n%.2f", tempDiffLOESS)
    text(x, y, textStr, adj=c(0,NA), col="darkblue", cex=2) 
    
    # Plot linear regression
    abline(fitLin, col="darkgreen", lwd=4) # , cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)  
    
    lines(c(yearBegin,  yearEnd), c(beginTempLin, beginTempLin), col="darkgreen", lwd=4, lty=5)
    lines(c(yearEnd-60, yearEnd), c(endTempLin,   endTempLin),   col="darkgreen", lwd=4, lty=5)
    arrows(yearEnd, beginTempLin, yearEnd, endTempLin, col="darkgreen", length=0.15, angle=30, code=3, lwd=3)
    
    x = yearEnd - 5
    y = mean(c(beginTempLin, endTempLin))
    textStr = sprintf("Linear\n%.2f", tempDiffLin)
    text(x, y, textStr, adj=c(1,NA), col="darkgreen", cex=2) # format(tempDiffLin),2)
    
    abline(0,0)
    grid()

    a=0
  } # End for iCase, loop over the datasets
  dev.off()
  a=0
}
