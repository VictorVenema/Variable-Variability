# Linear fit to polynomial function
# This code makes the plots for a blog post to illustrate that linear regression is 
# not a good way to estimate total warming.

expo = seq(from=0.2, to=4, by=0.05)
noExpo = length(expo)
tempDiffs = vector(mode="numeric", length=noExpo)
for(iExp in 1:noExpo) {
  fileName = paste0("fig3_fit_exp", expo[[iExp]] ,".png")
  png(file=fileName, 700, 600, type="cairo")
  
  noVal = 100 # Number of values (minus one)
  t = (0:noVal)/noVal
  y = t^expo[[iExp]]
  
  yearBegin = 0
  yearEnd   = 1
  
  fitLin = lm(y ~ t)
  sumfit_high=summary(fitLin)
  
  beginTempLin = predict(fitLin, data.frame(t=0))
  endTempLin   = predict(fitLin, data.frame(t=1))
  tempDiffLin = endTempLin - beginTempLin
  print(tempDiffLin)
  
  tempDiffs[iExp] = tempDiffLin
  
  yMin = min(c(beginTempLin, min(y)))
  yMax = max(c(endTempLin,   max(y)))
  yMin = 0.1*floor(yMin*10)
  yMax = 0.1*ceiling(yMax*10)
  
  plot(t, y, type="l", col="gray", lwd=4, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
       cex.main=1.5, xlab="Time", ylab="Value", ylim=c(yMin, yMax)) 
  abline(0,0)
  grid()
  
  abline(fitLin, col="darkgreen", lwd=4) 
  lines(c(yearBegin,  yearEnd),  c(beginTempLin, beginTempLin), col="darkgreen", lwd=4, lty=5)
  lines(c(yearEnd*0.6, yearEnd), c(endTempLin,   endTempLin),   col="darkgreen", lwd=4, lty=5)
  arrows(yearEnd, beginTempLin, yearEnd, endTempLin, col="darkgreen", length=0.15, angle=30, code=3, lwd=3)
  
  x = 0.95
  y = mean(c(beginTempLin, endTempLin))
  textStr = sprintf("Linear\n%.2f", tempDiffLin)
  text(x, y, textStr, adj=c(1,NA), col="darkgreen", cex=2) 

  x = 0
  y = 1
  textStr = bquote("f(t)=t"^{.(expo[[iExp]])})
  text(x, y, textStr, adj=c(0,NA), col="darkgreen", cex=2) 
  
  dev.off()
  a=0
}

fileName = paste0("fig4_warming_estimates.png")
png(file=fileName, 700, 600, type="cairo")
plot(expo, tempDiffs, type="l", col="darkred", lwd=2, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
     cex.main=1.5, xlab="Exponent", ylab="Linear warming estimate") 
abline(0,0)
grid()

dev.off()
a=0
