# noise_AR_noise
# Show the difference in long-term variability between white and AR noise.

noVal = 500 # 1e3
iPlot = 51:450
t = 1:noVal
y1 = rnorm(noVal)

y2 = arima.sim(list(ar = 0.99), n = noVal)
y2 = y2 - mean(y2)
y2 = y2 / sd(y2)

# plot(t, y1)
# plot(t, y2)

## 1b. Plot the data
fileNames = c("fig1_noise_ar_noise.png")
fileName = fileNames[[iFile]]
png(file=fileName, 700, 700, type="cairo")
par(mfrow = c(2,1))
par(mar=c(2,5,1,2)+0.1)

# Compute LOESS smoothed data 
y1.lo = loess(y1 ~ t, span = 50/noVal, degree=1)
y1Sm = predict(y1.lo, t)

y2.lo = loess(y2 ~ t, span = 50/noVal, degree=1)
y2Sm = predict(y2.lo, t)

# Plot data itself
yMin = min(c(y1, y2))
yMax = max(c(y1,y2,-yMin))
yMin = -yMax
yMin = 0.1*floor(yMin*10)
yMax = 0.1*ceiling(yMax*10)

plot(t[iPlot], y1[iPlot], type="p", col="firebrick1", lwd=1, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
     cex.main=1.5, xlab="Year", ylim=c(yMin, yMax), ylab="", xaxt='n') 
lines(t[iPlot], y1Sm[iPlot], pch=19, col="darkred", cex=1.5, lwd=5)
abline(0,0)
grid()

plot(t[iPlot], y2[iPlot], type="p", col="dodgerblue", lwd=1, cex=1.5, cex.lab=1.5, cex.axis=1.5, 
     cex.main=1.5, xlab="Year", ylim=c(yMin, yMax), ylab="", xaxt='n') 
lines(t[iPlot], y2Sm[iPlot], pch=19, col="darkblue", cex=1.5, lwd=5)
abline(0,0)
grid()

dev.off()
a=0
