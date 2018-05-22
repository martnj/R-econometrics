

# . -----------------------------------------------------------------------
# Introduction to R

# Creating vectors (data structure which contains objects of the same mode)
x <- c(1,2,3,4,5)       # 'c' for concatenate, gives *numeric* vector 'x' 
x                       # print out 'x'
length(x)
(y <- 1:5)              # define vector and *print* output via ’()’
(y[6] <- 6)             # append to a vector (alternative: y <- c(z, 6))
numeric(5)              # empty numeric of length 5 (default)

# Check if two objects are the same
x == y                  # component-wise 'equal-to' operator
identical(x,y)          # identical as objects? why not?
class(x)                # => x is a *numeric* vector
class(y)                # => y is an *integer* vector
all.equal(x,y)          # numerical equality; see argument 'tolerance'

# Vector arithmetic: component-wise
2*x + 1                 # ’*’ component-wise, ’+1’ adds 1 to all elements
x + x
x*x                     # component-wise product
x*y                     # must be of same length

# Some functions
(x <- c(3,4,2))
rev(x)                   # reverse order
sort(x)                  # sort in increasing order
sort(x, decreasing=TRUE) 
(idx <- order(x))        # create indices that sort x
x[idx]                   # => sorted
log(x)                   # (component-wise) logarithms
x^2                      # (component-wise) squares
exp(x)
sum(x)
cumsum(x)
prod(x)

# Sequences
seq(from=1,to=7,by=2)     
seq(from=1,to=100,length.out=25)
rep(1:3, each=3, times=2) 

# Missing values
z <- 1:3; z[5] <- 4       # two statements in one line (’;’-separated)
z                         # ’not available’ (NA)
c(z, 0/0)                 # 0/0, 0*Inf, Inf-Inf lead to ’not a number’ (NaN)
class(NaN)                # not a number but still of mode ’numeric’
class(NA)

# Matrices
(A <- matrix(1:9, ncol=3))             # OBS! operates on matrix by *columns*
(A <- matrix(1:9, ncol=3, byrow=TRUE)) # row-wise

# Some matrix functions
nrow(A)            # number of rows
ncol(A)
dim(A)             # dimension
diag(A)            # diagonal of A
diag(3)            # identity 3x3 matrix
(D <- diag(1:3))   # diagonal matrix
D%*%A              # matrix multiplication
A*A                # element-wise product (ie. Hadamard product)
log(A)
rowSums(A)
sum(A)             # sums all elements

# Random number generation
(X <- rnorm(2))       # generate two N(0,1) random variates
(Y <- rnorm(2))     

# Reproducibility:

# Set a ’seed’ 
X==Y                 # obviously not equal (here: with probability 1)
set.seed(10)         # with set.seed() we can set the seed
X <- rnorm(2)        # draw two N(0,1) random variates
set.seed(10)         # set the same seed again
Y <- rnorm(2)        
all.equal(X, Y)      # => TRUE




# Plot student-t distributions with various
# degrees of freedom and compare to the normal

x <- seq(-4, 4, length=100)
hx <- dnorm(x)
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")
for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}
legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


# The quantmod library

install.packages("quantmod")               # Download/install
install.packages(c("TTR","xts","zoo"))     # Packages required for qunatmode
library(quantmod)                          # Load the package
?`quantmod-package`

getSymbols("AAPL",src="yahoo")  # Download Apple stock from Yahoo
class(AAPL)
?`xts-package`
dim(AAPL)                                 
names(AAPL)
head(AAPL)                       # See the first 6 rows of the data
tail(AAPL)                       # See the last 6 rows of the data

# Financial chart from 'quantmod'
chartSeries(AAPL,theme="white") 
chartSeries(AAPL,type=c("auto","candlesticks"),subset="last 2 months")

# Log-returns
rtn = diff(log(AAPL$AAPL.Close))  
chartSeries(rtn,theme="white")





#------------------------------------------------------------------------


### Histogram: S&P 500 return data ###

#install.packages("Ecdat")    # Download package Ecdat
data(package="Ecdat")        # Load data from it (SP500),
data(SP500,package="Ecdat")

class(SP500)
names(SP500)                 
plot(SP500$r500,type="l")
hist(SP500$r500)
hist(SP500$r500,breaks=100)
plot(density(SP500$r500))
?density

#install.packages("fBasics")
#library(fBasics) 
basicStats(SP500)


#------------------------------------------------------------------------

### Test for normality: S&P 500 data ###

normalTest(SP500$r500,method='jb')   # JB-test
# STATISTIC:
#   X-squared: 648508.6002
# P VALUE:
#   Asymptotic p Value: < 2.2e-16    # Reject normality
hist(SP500$r500,nclass=30)           # Histogram
d1 = density(SP500$r500)             # Obtain density estimate
range(SP500$r500)                    # Range of SP500 returns
x = seq(-.25,.1,by=.001)             # Create a sequence of x with increment 0.001.
y1 = dnorm(x,mean(SP500$r500),stdev(SP500$r500))
plot(d1$x,d1$y,xlab="return",ylab="density",type="l")
lines(x,y1,lty=2)
plot(d1$x,d1$y,xlab="return",ylab="density",type="l",xlim=c(-0.05,.05))
lines(x,y1,lty=2)

#------------------------------------------------------------------------

# Create sample data 
set.seed(3)
x <- rcauchy(100)     # Undefined moments

# Standard normal fit?
qqnorm(x) 
qqline(x)

# S&P 500 returns
y = (SP500$r500 - mean(SP500$r500))/sd(SP500$r500)
qqnorm(y) 
qqline(y)

#------------------------------------------------------------------------

# Mishkin tb3 - three-month Bonds
# T-bill rate (in percent, annual rate)
data(Mishkin ,package="Ecdat")
plot(Mishkin[,4])
x <- diff(Mishkin[,4])
plot(x)

y <- diff(log(Mishkin[,4]))
plot(y)
abline(h=0,col="grey")


#------------------------------------------------------------------------

### S$P 500 ETF - ACF ###

getSymbols("SPY",src="google")    
head(SPY) 
plot(SPY$SPY.Close)
SPY.rtn = diff(log(SPY$SPY.Close))
plot(SPY.rtn)
acf(SPY.rtn[-1])     # plot autocorrelation function
acf(SPY.rtn[-1]^2)   # x[-1] : skip the first element
     
#------------------------------------------------------------------------

### Stationarity CO2 at Manua Loa ###

data(co2,package="datasets")
plot(co2)
co2.stl= stl(co2,"periodic")
?stl
head(co2.stl$time.series)
plot(co2.stl)

# Reminder:
plot(co2.stl$time.series[,"remainder"],ylab="CO2 data, remainder")

plot(diff(co2,differences = 2))

#------------------------------------------------------------------------

### White noise ###

WN <- rnorm(1024,0,1)
ts.plot(WN)
acf(WN,40,"covariance")
acf(WN,40,"correlation")

# check for normality
qqnorm(WN)
qqline(WN)

#------------------------------------------------------------------------

### Random Walk ###

WN <- rnorm(1024,0,1)
RW <- cumsum(WN)
ts.plot(RW)
acf(RW,40,"correlation")
acf(diff(RW),40,"correlation")

#------------------------------------------------------------------------

### ARIMA process ###

# order = c(p,d,q) where p is AR(p), d differencing, q is MA(q)
x = arima.sim(list(order=c(1,0,0),ar=0.9),n=1000)
plot(x)
acf(x,40,type="correlation")
lines(0.9^(0:40),lty=2)

#------------------------------------------------------------------------

### PACF for AR models ###

# US Gross National Product (GNP)
data = read.table("Data/q-gnp4710.txt",header=T)
head(data)
tail(data)
gnp = data$VALUE
gnp.r = diff(log(gnp))
tVec = seq(1947,2010,length.out=nrow(data))    # create the time index
plot(tVec,gnp,xlab='year',ylab='GNP',type="l")
plot(tVec[-1],gnp.r,type="l",xlab="year",ylab="growth"); abline(h=0)
acf(gnp.r,lag=12)
pacf(gnp.r,lag=12,ylim=c(-1,1)) 

# PACF criteria
m1 = arima(gnp.r,order=c(3,0,0)) # fit AR(3) model
m1

### AIC criteria ###

?ar
m2 = ar(gnp.r,method='mle')
m2$order                      # Find the identified order
names(m2)
print(m2$aic,digits=3)
plot(c(0:12),m2$aic,type='h',xlab='order',ylab='AIC')
lines(0:12,m2$aic,lty=2)


#------------------------------------------------------------------------

### Box-Ljung test ###

vw = read.table("Data/m-ibm3dx2608.txt",header=T)[,3]
m3 = arima(vw-mean(vw),order=c(3,0,0))
m3
names(m3)
sqrt(m3$sigma2)              # Compute standard error of residuals
Box.test(m3$residuals,lag=12,type='Ljung')
pv = 1 - pchisq(16.352,9)    # Compute p-value using 9 dof (lag - nbr params)
pv
m4 = arima(vw-mean(vw),order=c(3,0,0),fixed=c(NA,0,NA,NA))
m4
sqrt(m4$sigma2)              # Compute residual standard error
Box.test(m4$residuals,lag=12,type='Ljung')
pv = 1 - pchisq(16.828,10)   # Compute p-value using 10 dof (lag - nbr params)
pv


#------------------------------------------------------------------------

### Regression model with time series errrors ###

# Treasury rates, weekly data
r1 = read.table("Data/w-gs1yr.txt",header=T)[,4]
r3 = read.table("Data/w-gs3yr.txt",header=T)[,4]
tVec = c(1:2467)/52+1962          
par(mfcol=c(2,1))
plot(tVec,r1,xlab="",ylab="1-year rate",type="l")
plot(tVec,r3,xlab="",ylab="3-year rate",type="l")
par(mfcol=c(1,1))
plot(r1,r3,type="p")

# Linear regression
lm1 = lm(r3~r1)
summary(lm1)
plot(lm1$residuals,type='l'); abline(h=0)
acf(lm1$residuals,lag=36)

#------------------------------------------------------------------------

### Look at first differences

c1 = diff(r1)
c3 = diff(r3)
par(mfcol=c(2,1))
plot(tVec[-1],c1,xlab="",ylab="1-year, difference",type="l")
plot(tVec[-1],c3,xlab="",ylab="3-year, difference",type="l")
par(mfcol=c(1,1))
plot(c1,c3); abline(0,1)

# Linear regression
lm2 = lm(c3~-1+c1)     # '-1' for no intercept
summary(lm2)
plot(lm2$residuals,type="l")
acf(lm2$residuals,lag=36)
pacf(lm2$residuals,lag=36,ylim=c(-1,1))


#------------------------------------------------------------------------

### MA(1) for regression residuals ###

r = lm2$residuals
m = arima(r,order=c(0,0,1),include.mean=F)
m

plot(m$residuals); abline(h=0)
acf(m$residuals)
pacf(m$residuals,ylim=c(-1,1))
Box.test(m$residuals,lag=10,type='Ljung')
pv = 1 - pchisq(50.344,9)    # Compute p-value using dof = lag-#params
pv
qqnorm(m$residuals) 
qqline(m$residuals)

arx = ar(r)
acf(arx$resid[-(1:21)])





#------------------------------------------------------------------------
# Non-linear time series
#------------------------------------------------------------------------

### Non-linearity ###

ts.plot(SP500); abline(h=0)
acf(SP500)
qqnorm(SP500$r500)
qqline(SP500$r500)
acf(SP500$r500^2)
data(Tbrate,package="Ecdat")
Tbill <- Tbrate[,1]
d.Tbill <- diff(Tbill)
qqnorm(d.Tbill)
qqline(d.Tbill)
acf(d.Tbill)
acf(d.Tbill^2)

#------------------------------------------------------------------------

# IBM data:
da = read.table("data/m-intcsp7309.txt",header=T)
head(da)
intc = log(da$intc+1)
rtn = ts(intc,frequency=12,start=c(1973,1))
plot(rtn,type="l",xlab="year",ylab="log-rtn") # time plot
abline(h=0)
t.test(intc) # testing the mean of returns
Box.test(intc,lag=12,type="Ljung")
# do not reject H0: rho(h)=0

par(mfcol=c(2,1))
acf(intc,lag=24) # ACF plots
acf(abs(intc),lag=24)
par(mfcol=c(1,1))
Box.test(abs(intc),lag=12,type="Ljung")
# Reject H0

#------------------------------------------------------------------------

### ARCH fitting ###

#pacf(intc^2,lag=24,ylim=c(-1,1))

# install fGarch from Rmetrics
library(fGarch)                              # Load package
m1 = garchFit(~garch(3,0),data=intc,trace=F) # Fit an ARCH(3) model
?garchFit
summary(m1)
m2 = garchFit(~garch(1,0),data=intc,trace=F)
summary(m2)
resi = residuals(m2,standardize=T)
tdx = c(1:444)/12+1973
par(mfcol=c(1,1))
plot(tdx,resi,xlab="year",ylab="stand-resi",type="l")
acf(resi,lag=20)
acf(resi^2,lag=20)
plot(m2)



#------------------------------------------------------------------------
# Trading with R
#------------------------------------------------------------------------



### Simple trading strategy ###

#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(TTR)

# Step 1: Get the data from Yahoo
getSymbols("^GSPC")       
GSPC = na.omit(GSPC)      # GSPC contains 'NA' -> omit these
names(GSPC)
chartSeries(Cl(GSPC),theme = chartTheme("white"))


# Step 2: Create indicator
# Calculate DVI (momentum indicator) (TTR library)
dvi <- DVI(Cl(GSPC))      # 'Cl' gives closing price      
?DVI
plot(dvi[,3])

# Step 3: Construct your trading rule
# Create signal: (long (short) if DVI is below (above) 0.5)
# 'Lag' so yesterday’s signal is applied to today’s returns
sig <- Lag(ifelse(dvi[,3] < 0.5, 1, -1),k=1)

# Step 4: Equity curve
# calculate signal-based returns
ret <- ROC(Cl(GSPC))*sig

# Step 5: Evaluate strategy performance
# subset returns to period of intrest
ret <- ret["2009-06-02/2010-09-07"]

# Use the PerformanceAnalytics package:
# Cumulative Performance
chart.CumReturns(ret)
# Performance, Drawdonws etc...
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)

# Compare with long buy-and-hold
ret <- ROC(Cl(GSPC))["2009-06-02/2010-09-07"]
charts.PerformanceSummary(ret)

# Compare with long-only
sig <- Lag(ifelse(dvi[,3] < 0.5, 1, 0))
ret <- ROC(Cl(GSPC))*sig
ret <- ret["2009-06-02/2010-09-07"]
charts.PerformanceSummary(ret)

# Compare with random signal
set.seed(10)
sig = runif(length(Cl(GSPC))) < 0.5
sig = 2*sig-1
ret <- ret["2009-06-02/2010-09-07"]
ret <- ROC(Cl(GSPC))*sig
charts.PerformanceSummary(ret)



### Second example with RSI ###

# Pull S&P500 index data from Yahoo! Finance
getSymbols("^GSPC", from="2000-01-01", to="2008-12-07")
chartSeries(Cl(GSPC),theme = chartTheme("white"))
GSPC = na.omit(GSPC)   

# Calculate the RSI indicator
rsi <- RSI(Cl(GSPC),2)
plot(rsi)
?RSI

# Create the long (up) and short (dn) signals
sigup <- ifelse(rsi < 10, 1, 0)
sigdn <- ifelse(rsi > 90, -1, 0)
# Lag signals to align with days in market,
# not days signals were generated
sigup <- lag(sigup,1)
sigdn <- lag(sigdn,1)

# Replace missing signals with no position
# (generally just at beginning of series)
sigup[is.na(sigup)] <- 0
sigdn[is.na(sigdn)] <- 0
# Combine both signals into one vector
sig <- sigup + sigdn

# Calculate Close-to-Close returns
ret <- ROC(Cl(GSPC))*sig
charts.PerformanceSummary(ret)


