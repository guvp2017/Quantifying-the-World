
# R codes for Real-Time Location System Case Study

# -------------------------------------------------------------------------------------------------
# Part 1: Functions for Data File (online.final.trace.txt and offline.final.trace).txt
# -------------------------------------------------------------------------------------------------

### processLine() to process each row in the input file.

processLine = function(x)
{
    tokens = strsplit(x, "[;=,]")[[1]]
    if (length(tokens) == 10)
        return(NULL)
    tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
    cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), ncol = 6,
                 byrow = TRUE), tmp)
}

### roundOrientation() to create the rounded angles

roundOrientation = function(angles){
    refs = seq(0, by = 45, length = 9)
    q = sapply(angles, function(o) which.min(abs(o - refs)))
               c(refs[1:8], 0)[q]
               }

### readData() to read data files

readData = function(
    filename = './offline.final.trace.txt',
    subMacs = c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
                "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d", 
                "00:14:bf:b1:97:81")
)

{
    txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != "#" ]
    tmp = lapply(lines, processLine)
    offline = as.data.frame(do.call("rbind", tmp), stringsAsFactors= FALSE)
    names(offline) = c("time", "scanMac","posX", "posY", "posZ", "orientation","mac", "signal", "channel", "type")
    offline = offline[ offline$mac %in% subMacs, ]
    
    # convert numeric values
    numVars = c("time", "posX", "posY", "orientation", "signal") 
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    
    # convert time to POSIX
    offline$rawTime = offline$time
    offline$time = offline$time/1000 
    class(offline$time) = c("POSIXt", "POSIXct")
    
    # round orientations to nearest 45
    offline$angle = roundOrientation(offline$orientation)
    
    # Create a special factor that contains all of the unique combinations # of the observed (x,y) pairs for the locations
    offline$posXY = paste(offline$posX, offline$posY, sep = "-")
    return(offline)
}

### surfaceSS() has 3 arguments: data for the offline summary data frame, and mac and angle, 
### which supply the MAC address and angle to select the subset of the data that we want smoothed and plotted. 
### We call surfaceSS() with a couple of MAC addresses and angles to compare them. 

surfaceSS = function(data, mac, angle = 45)
{
    # extract subset of the data for specified MAC and angle: 
    oneAPAngle = data[ data$mac == mac & data$angle == angle, ]
    
    # Fit a thin plate spline (TPS) surface to the data:    
    smoothSS = Tps(oneAPAngle[, c("posX","posY")], oneAPAngle$avgSignal)
    
    # Evaluate fitted model on a 2D grid so we can plot it: 
    vizSmooth = predictSurface(smoothSS)
    
    # And... plot:
    plot.surface(vizSmooth, type = "C", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    
    # Add the measurement points
    points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5)
    
    # Annotate the plot 
    text(32,12,paste(angle,intToUtf8(176),sep=''), adj=c(1,0.5)) 
    text(32,1,toupper(mac), adj=c(1,0.5))
}

### reshapeSS() to aggregate the signal strengths from these angles and create a data structure

reshapeSS = function(
        data, varSignal = "signal",
        keepVars = c("posXY", "posX","posY"), sampleAngle = FALSE,
        refs = seq(0, 315, by = 45))
{
    nCol = length(unique(data$mac))
    byLocation = with(data, by(data, list(posXY),
                               function(x) {
                                   if (sampleAngle) {
                                       x = x[x$angle == sample(refs, size = 1), ]}
                                   ans = x[1, keepVars]
                                   avgSS = tapply(x[ , varSignal ], x$mac, mean) 
                                   y = matrix(avgSS, nrow = 1, ncol = nCol,
                                              dimnames = list(ans$posXY, names(avgSS)))
                                   cbind(ans, y) 
                               }))
newDataSS = do.call("rbind", byLocation)
return(newDataSS) 
}

### selectTrain() function has 3 parameters: 
### angleNewObs, the angle of the new observation; 
### signals, the training data, i.e., data in the format of offlineSummary; 
### and m, the number of angles to include from signals. 
### The function returns a data frame that matches trainSS.

selectTrain = function(angleNewObs, signals = NULL, m = 1)
{
    # m is the number of angles to keep between 1 and 5 refs = seq(0, by = 45, length = 8)
    nearestAngle = roundOrientation(angleNewObs)
    
    if (m %% 2 == 1)
        angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    else {
        m=m+1
        angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m) 
        
        if (sign(angleNewObs - nearestAngle) > -1)
            angles = angles[ -1 ] 
        else
            angles = angles[ -m ] 
        }
    
angles = angles + nearestAngle
angles[angles < 0] = angles[ angles < 0 ] + 360 
angles[angles > 360] = angles[ angles > 360 ] - 360 
angles = sort(angles)

offlineSubset = signals[ signals$angle %in% angles, ]
reshapeSS(offlineSubset, varSignal = "avgSignal") 
}

### findNN() function to calculate the distance from the new point to all observations in the training set

findNN = function(newSignal, trainSubset)
{
    # Signal differences at THIS location between training and observed, for each access point
    diffs = apply(trainSubset[,4:9], 1, function(x) x - newSignal) 
                  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)))
                                closest = order(dists)
                                return(trainSubset[closest, 1:3 ])
                                }

### predXY() to formalize and make predictions for all of the test data

predXY = function(newSignals, newAngles, trainData, numAngles=1, k=3)
{
    closeXY = list(length = nrow(newSignals)) # an empty list 
    for (i in 1:nrow(newSignals)) {
        trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
        closeXY[[i]] = findNN(newSignal = as.numeric(newSignals[i,]), trainSS) 
    }
    
    estXY = lapply(closeXY, function(x) sapply(x[,2:3], function(x) mean(x[1:k]))) 
    estXY = do.call("rbind", estXY)
    return(estXY)
}

### findNNalt(newSignal,trainSubset)

findNNalt = function(newSignal, trainSubset)
{
    # Signal differences at THIS location between training and observed, for each access point
    diffs = apply(trainSubset[,4:9], 1, function(x) x - newSignal) 
    dists = apply(diffs, 2, function(x) sqrt(sum(x^2)))
    closest = order(dists)
    retSS = trainSubset[closest, 1:3]
    retSS$dist = dists[closest]
    retSS$wt = 1/retSS$dist
    return(retSS)
}

### predXYalt(newSignals,newAngles,trainData,numAngles,k) for weighted k-NN

predXYalt = function(newSignals, newAngles, trainData, numAngles=1, k=3){
    
    closeXY = list(length = nrow(newSignals))
    
    for (i in 1:nrow(newSignals)) {
        trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
        closeXY[[i]] = findNNalt(newSignal = as.numeric(newSignals[i,]), trainSS) 
    }
    
    estXY = lapply(closeXY,function(a)sapply(a[,2:3],function(b)weighted.mean(b[1:k],a[1:k,5])))
    estXY = do.call("rbind", estXY)
    return(estXY)
}                                      

### calcError(estXY,actualXY) to find the sum of squared errors

calcError = function(estXY, actualXY)
{
    sum( rowSums( (estXY - actualXY)^2) )
}

# -------------------------------------------------------------------------------------------------
# Part 2: Exploratory Data Analysis (EDA)
# -------------------------------------------------------------------------------------------------

options(digits = 2)
library(dplyr)
library(lattice)
library(fields)

allMacs = c("00:0f:a3:39:dd:cd", "00:0f:a3:39:e1:c0", "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:81",
            "00:14:bf:b1:97:8a", "00:14:bf:b1:97:8d", "00:14:bf:b1:97:90") 

offline = readData(subMacs=allMacs)

# create a list of data frames for every combination of (x,y), angle, and access point
byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x))

# calculate summary statistics on each of these data frames
signalSummary = lapply(byLocAngleAP,
                       function(oneLoc) {
                           ans = oneLoc[1, ]
                           ans$medSignal = median(oneLoc$signal) 
                           ans$avgSignal = mean(oneLoc$signal) 
                           ans$num = length(oneLoc$signal) 
                           ans$sdSignal = sd(oneLoc$signal) 
                           ans$iqrSignal = IQR(oneLoc$signal) 
                           ans
                       })

offlineSummary = do.call("rbind", signalSummary)

# filter to only the two interested MACs: dd:cd and e1:c0
off.filt <- filter(offline, mac=='00:0f:a3:39:dd:cd' | mac=='00:0f:a3:39:e1:c0', posX==2, posY==12)

# signal strength by angle for these two access points: dd:cd and e1:c0
png(filename="./f21.png", width = 12, height = 6, units="in", res=300, pointsize=1/600)
bwplot(signal ~ factor(angle) | mac, data=off.filt, layout=c(2,1))
dev.off()

# reproduce Fig 1.8: plot standard deviation of signal strength (y) by mean signal strength (x)
png(filename="./f22.png", width = 12, height = 6, units="in", res=300, pointsize=1/600)
opar = par(mar = c(3.1, 3, 1, 1))
bwplot(sdSignal ~ cut(avgSignal, breaks=seq(-90,-30,by=5)),
       data = offlineSummary,
       subset = mac != "00:0f:a3:39:dd:cd",
       xlab = "Mean Signal", ylab = "SD Signal")
par(opar)
dev.off()

# Show summary statistics for these two MACs
off.filt %>% group_by(mac) %>% summarize(n=n(), sd=sd(signal), min=min(signal), max=max(signal), mean=mean(signal))

# side-by-side visual comparisons with signal strength field plots at 8 angles for two access points: DD:CD and E1:C0
for ( m in allMacs ){
    opar = par(mfrow = c(4,2), mar = rep(2, 4))
    
    # make 4 calls to our surfaceSS() function using mapply() 
    mapply(surfaceSS, mac = rep(m,8),
           angle = seq(0, 315, by=45),
           data = list(data = offlineSummary))
    
    # reset the plotting parameters
    par(opar)
}

# -------------------------------------------------------------------------------------------------
# Part 3: line plot for the cross validated selection of k in 3 scenarios: 
## 6 access points (not including DD:CD)
## 6 access points (not including E1:E0)
## 7 access points (including DD:Cd and E1:C0)
# -------------------------------------------------------------------------------------------------

allMacs = c("00:0f:a3:39:dd:cd", "00:0f:a3:39:e1:c0", "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:81",
            "00:14:bf:b1:97:8a", "00:14:bf:b1:97:8d", "00:14:bf:b1:97:90")

keepVars = c("posXY", "posX", "posY", "orientation", "angle")
set.seed(12345)

# printf <- function(...) invisible(cat(sprintf(...)))

### 1. reproduce predictions from the text (not including access point DD:CD)

# preparing the Test Data
macs1 = allMacs[2:7] # as in the text, without access point DD:CD

offline = readData(subMacs=macs1)
online = readData("./online.final.trace.txt", subMacs=macs1)

# create a list of data frames for every combination of (x,y), angle, and access point 
byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x))

# calculate summary statistics on each of these data frames
signalSummary = lapply(byLocAngleAP,
                       function(oneLoc) {
                           ans = oneLoc[1, ]
                           ans$medSignal = median(oneLoc$signal) 
                           ans$avgSignal = mean(oneLoc$signal) 
                           ans$num = length(oneLoc$signal) 
                           ans$sdSignal = sd(oneLoc$signal) 
                           ans$iqrSignal = IQR(oneLoc$signal) 
                           ans
                       })

offlineSummary = do.call("rbind", signalSummary)

# as with the offline data, we create a unique location identifier with
online$posXY = paste(online$posX, online$posY, sep = "-") 
byLoc = with(online,
             by(online, list(posXY), 
                function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow=1, ncol=6, dimnames=list(ans$posXY, names(avgSS)))
                    cbind(ans, y)
                }))
onlineSummary = do.call("rbind", byLoc)

# cross validation with v=11 for the 166 locations (6 MAC addresses and 8 orientations)
v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol=v, nrow=floor(length(permuteLocs)/v))
onlineCVSummary = reshapeSS(offline, keepVars=keepVars, sampleAngle=TRUE)

# find the k-NN estimates for each fold, here set k = 20
K = 20
err = rep(0, K)
for (j in 1:v)
{
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j]) 
    offlineFold = subset(offlineSummary, posXY %in% permuteLocs[,-j]) 
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for ( k in 1:K )
    {
        estFold = predXY(newSignals = onlineFold[,6:11], newAngles=onlineFold[,4], offlineFold, numAngles=3, k=k)
        thisError = calcError(estFold, actualFold) 
      #  printf("j=%d, k=%d, err=%f\n", j, k, thisError) 
        err[k] = err[k] + thisError
    } }

# put err in a doggie bag for later err.txt = err 
err.txt = err

### 2. Use the DD:CD access point instead of E1:C0

# preparing the Test Data
macs2 = allMacs[c(1,3:7)] # without access point E1:C0

offline = readData(subMacs=macs2)
online = readData("./online.final.trace.txt", subMacs=macs2)

# Create a list of data frames for every combination of (x,y), angle, and access point 
byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x))

# calculate summary statistics on each of these data frames
signalSummary = lapply(byLocAngleAP,
                       function(oneLoc) {
                           ans = oneLoc[1, ]
                           ans$medSignal = median(oneLoc$signal) 
                           ans$avgSignal = mean(oneLoc$signal) 
                           ans$num = length(oneLoc$signal) 
                           ans$sdSignal = sd(oneLoc$signal) 
                           ans$iqrSignal = IQR(oneLoc$signal) 
                           ans
                       })

offlineSummary = do.call("rbind", signalSummary)

# as with the offline data, we create a unique location identifier with
online$posXY = paste(online$posX, online$posY, sep = "-") 
byLoc = with(online,
             by(online, list(posXY), 
                function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow=1, ncol=6, dimnames=list(ans$posXY, names(avgSS)))
                    cbind(ans, y)
                }))
onlineSummary = do.call("rbind", byLoc)                                  

# cross validation with v=11 for the 166 locations (6 MAC addresses and 8 orientations)
v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol=v, nrow=floor(length(permuteLocs)/v))
onlineCVSummary = reshapeSS(offline, keepVars=keepVars, sampleAngle=TRUE)  

# find the k-NN estimates for each fold, here set k = 20
K = 20
err = rep(0, K)
                                
for (j in 1:v){
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j]) 
    offlineFold = subset(offlineSummary, posXY %in% permuteLocs[,-j]) 
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for ( k in 1:K ){
        estFold = predXY(newSignals = onlineFold[,6:11], newAngles = onlineFold[,4], offlineFold, numAngles=3, k=k)
        thisError = calcError(estFold, actualFold) 
        # printf("j=%d, k=%d, err=%f\n", j, k, thisError) 
        err[k] = err[k] + thisError
    } }       

# put err in a doggie bag for later err.txt = err 
err.ddcd = err

### 3. Use all 7 access points

# preparing the Test Data
macs3 = allMacs # with 7access points including E1:C0 and DD:CD

offline = readData(subMacs=macs3)
online = readData("./online.final.trace.txt", subMacs=macs3)

# Create a list of data frames for every combination of (x,y), angle, and access point 
byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x))

# calculate summary statistics on each of these data frames                                
signalSummary = lapply(byLocAngleAP,
                       function(oneLoc) {
                           ans = oneLoc[1, ]
                           ans$medSignal = median(oneLoc$signal) 
                           ans$avgSignal = mean(oneLoc$signal) 
                           ans$num = length(oneLoc$signal) 
                           ans$sdSignal = sd(oneLoc$signal) 
                           ans$iqrSignal = IQR(oneLoc$signal) 
                           ans
                       })

offlineSummary = do.call("rbind", signalSummary)

# as with the offline data, we create a unique location identifier with
online$posXY = paste(online$posX, online$posY, sep = "-") 
byLoc = with(online,
             by(online, list(posXY), 
                function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow=1, ncol=7, dimnames=list(ans$posXY, names(avgSS)))
                    cbind(ans, y)
                }))
onlineSummary = do.call("rbind", byLoc)                                  

# cross validation with v=11 for the 166 locations (6 MAC addresses and 8 orientations)                               
v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol=v, nrow=floor(length(permuteLocs)/v))
onlineCVSummary = reshapeSS(offline, keepVars=keepVars, sampleAngle=TRUE)  

# find the k-NN estimates for each fold, here set k = 20                               
K = 20
err = rep(0, K)
for (j in 1:v)
{
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j]) 
    offlineFold = subset(offlineSummary, posXY %in% permuteLocs[,-j]) 
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for ( k in 1:K )
    {
        estFold = predXY(newSignals = onlineFold[,6:11], newAngles=onlineFold[,4], offlineFold, numAngles=3, k=k)
        thisError = calcError(estFold, actualFold) 
       # printf("j=%d, k=%d, err=%f\n", j, k, thisError) 
        err[k] = err[k] + thisError
    } }       

# put err in a doggie bag for later err.txt = err                                
err.all = err

# cross validated selection of k for all 3 scenarios

png(file="./f4.png",width=600, height=400)
opar = par(mar = c(4, 4, 1, 1))

err.combined = c(err.txt, err.ddcd, err.all)
y_limits = c(round(min(err.combined),-2)-100, round(max(err.combined))+100)

# plot errors for 1st run from the textbook (not including access point DD:CD)
rmseMin = min(err.txt)
kMin = which(err.txt == rmseMin)[1] 
my.col = 'pink'

plot(y=err.txt, x = (1:K), col=my.col, type = "l", lwd= 2, ylim = y_limits, 
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")

segments(x0=0, x1=kMin, y0=rmseMin, col=my.col, lty=2, lwd=2)
segments(x0 = kMin, x1=kMin, y0=y_limits[1], y1=rmseMin, col=my.col, lty=2, lwd=2) 
mtext(kMin, side=1, line=1, at=kMin, col=my.col)
text(x=2, y=rmseMin+40, label=as.character(round(rmseMin)), col=my.col)

# plot errors for 2nd run (not including access point E1:C0)
rmseMin = min(err.ddcd)
kMin = which(err.ddcd == rmseMin)[1] 
my.col = 'blue'

lines(err.ddcd, col=my.col)
segments(x0=0, x1=kMin, y0=rmseMin, col=my.col, lty=2, lwd=2)
segments(x0 = kMin, x1=kMin, y0=y_limits[1], y1=rmseMin, col=my.col, lty=2, lwd=2) 
mtext(kMin, side=1, line=1, at=kMin, col=my.col)
text(x=2, y=rmseMin+40, label=as.character(round(rmseMin)), col=my.col)

# plot errors for 3nd run (including access points DD:CD and E1:C0)
rmseMin = min(err.all)
kMin = which(err.all == rmseMin)[1] 
my.col = 'black'

lines(err.all, col=my.col)
segments(x0=0, x1=kMin, y0=rmseMin, col=my.col, lty=2, lwd=2)
segments(x0 = kMin, x1=kMin, y0=y_limits[1], y1=rmseMin, col=my.col, lty=2, lwd=2) 
mtext(kMin, side=1, line=1, at=kMin, col=my.col)
text(x=2, y=rmseMin+40, label=as.character(round(rmseMin)), col=my.col)

legend(9, 2100, c('6 access points (not include DD:CD)', 
                   '6 access points (not include E1:C0)', 
                   '7 access points (include DD:CD and E1:C0)'), 
       lty=c(1,1,1), col=c('pink', 'blue', 'black'))

par(opar) 
dev.off()

# -------------------------------------------------------------------------------------------------
# Part 4: implement the alternative prediction method with weight
# -------------------------------------------------------------------------------------------------

allMacs = c("00:0f:a3:39:dd:cd", "00:0f:a3:39:e1:c0", "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:81",
            "00:14:bf:b1:97:8a", "00:14:bf:b1:97:8d", "00:14:bf:b1:97:90") 

keepVars = c("posXY", "posX", "posY", "orientation", "angle")
set.seed(12345)

# 1. reproduce predictions from the text (not including access point DD:CD)

macs_text = allMacs[2:7] # as in the text, without access point DD:CD

offline = readData(subMacs=macs_text)
online = readData("./online.final.trace.txt", subMacs=macs_text)

# create a list of data frames for every combination of (x,y), angle, and access point 
byLocAngleAP = with(offline, by(offline, list(posXY, angle, mac), function(x) x))
                                
# calculate summary statistics on each of these data frames 
signalSummary = lapply(byLocAngleAP,
                       function(oneLoc) {
                           ans = oneLoc[1, ]
                           ans$medSignal = median(oneLoc$signal)
                           ans$avgSignal = mean(oneLoc$signal) 
                           ans$num = length(oneLoc$signal) 
                           ans$sdSignal = sd(oneLoc$signal) 
                           ans$iqrSignal = IQR(oneLoc$signal) 
                           ans
                       })
                                
offlineSummary = do.call("rbind", signalSummary)

# as with the offline data, we create a unique location identifier with
online$posXY = paste(online$posX, online$posY, sep = "-") 
byLoc = with(online,
             by(online, list(posXY), 
                function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow=1, ncol=6, dimnames=list(ans$posXY, names(avgSS))) 
                    cbind(ans, y)
                }))
                                
onlineSummary = do.call("rbind", byLoc)

# cross validation with v=11
v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol=v, nrow=floor(length(permuteLocs)/v))
                                
onlineCVSummary = reshapeSS(offline, keepVars=keepVars, sampleAngle=TRUE)
                                
# try K-nearest neighbors prediction for values of k from 1 to 20 
K = 20
err = rep(0, K)
for (j in 1:v)
{
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j]) 
    offlineFold = subset(offlineSummary, posXY %in% permuteLocs[,-j]) 
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for ( k in 1:K )
    {
        estFold = predXY(newSignals = onlineFold[,6:11], newAngles=onlineFold[,4], offlineFold, numAngles=3, k=k)
        err[k] = err[k] + calcError(estFold, actualFold) }
}
                                
# Put err in a doggie bag for later err.txt = err
err.txt = err                              

# 2. same date, but use weighted nearest neighbors for predicting scenario of not including access point DD:CD

# try K-nearest neighbors prediction for values of k from 1 to 20 
K = 20
err = rep(0, K)
for (j in 1:v)
{
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j]) 
    offlineFold = subset(offlineSummary, posXY %in% permuteLocs[,-j]) 
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for ( k in 1:K )
    {
    estFoldalt = predXYalt(newSignals = onlineFold[,6:11], newAngles=onlineFold[,4], offlineFold, numAngles=3, k=k)
    err[k] = err[k] + calcError(estFoldalt, actualFold) }
}

# Put err in a doggie bag for later err.alt = err
err.alt = err

# line plot for cross validated selection of k for all 2 scenarios (unweighted and weighted)

png(file="./f5.png",width=600, height=400)
opar = par(mar = c(4, 4, 1, 1))

err.combined = c(err.txt, err.alt)
y_limits = c(round(min(err.combined),-2)-100, round(max(err.combined))+100)

# plot errors for 1st run from unweighted scenario of not including access point DD:CD
rmseMin = min(err.txt)
kMin = which(err.txt == rmseMin)[1]
my.col = 'pink'

plot(y=err.txt, x = (1:K), col=my.col, type = "l", lwd= 2, ylim = y_limits, 
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")

segments(x0=0, x1=kMin, y0=rmseMin, col=my.col, lty=2, lwd=2)
segments(x0 = kMin, x1=kMin, y0=y_limits[1], y1=rmseMin, col=my.col, lty=2, lwd=2) 
mtext(kMin, side=1, line=1, at=kMin, col=my.col)
text(x=2, y=rmseMin+40, label=as.character(round(rmseMin)), col=my.col)

# plot errors for 2nd run from weighted scenario of ncluding access point DD:CD
rmseMin = min(err.alt)
kMin = which(err.alt == rmseMin)[1]
my.col = 'magenta'

lines(err.alt, col=my.col)

segments(x0=0, x1=kMin, y0=rmseMin, col=my.col, lty=2, lwd=2)
segments(x0 = kMin, x1=kMin, y0=y_limits[1], y1=rmseMin, col=my.col, lty=2, lwd=2) 
mtext(kMin, side=1, line=1, at=kMin, col=my.col)
text(x=2, y=rmseMin+40, label=as.character(round(rmseMin)), col=my.col)

legend(7, 1950, c('unweighted, 6 access points (not include DD:CD)', 
                   'weighted, 6 access points (not include DD:CD)'), 
       lty=c(1,1), col=c('pink', 'magenta'))

par(opar)
dev.off()

# - The End -
