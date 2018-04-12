

data <- read.csv("Data/mysterytsUW.csv")
data$period <- seq(1,100,1)

plot(x=data$period, y=data$a, type="l", ylim=range(-4,4))



#trend
fit.a <- lm(a ~ period, data=data)
summary(fit.a)
beta.a <- fit.a$coefficients[2]


aa<-NULL
for (i in 1:100){
  aa[i] <- data$a[i] - beta.a*data$period[i]
}

detrend.ts <- function(ts=NULL, beta=NULL, period=NULL){
  detrend <- NULL
  for (i in 1:length(ts)){
    detrend[i] <- ts[i] - beta*period[i]
  }
  return(detrend)
}

plot(x=data$period, y=aa, type="l", ylim=range(-4,4))

acf(aa)
pacf(aa)


#seasonality
plot(1,type='n',xlim=c(1,12),ylim=c(-4,4),xlab='Month', ylab='Value')
lines(data$a[1:12], type='l', col=rainbow(9)[1])
lines(data$a[12:24], type='l', col=rainbow(9)[2])
lines(data$a[25:36], type='l', col=rainbow(9)[3])
lines(data$a[37:48], type='l', col=rainbow(9)[4])
lines(data$a[49:60], type='l', col=rainbow(9)[5])
lines(data$a[61:72], type='l', col=rainbow(9)[6])
lines(data$a[73:84], type='l', col=rainbow(9)[7])
lines(data$a[85:96], type='l', col=rainbow(9)[8])
lines(data$a[97:100], type='l', col=rainbow(9)[9])

#Look at the monthly cycle by plotting each year separately

# Make some colors
col <- brewer.pal(8, "Blues")

# Gather the data (sort the number of deaths by month and year in a matrix)
deathmat <- matrix(death,nrow=12,ncol=length(death)/12, byrow=FALSE)

# Repeat them as many times as needed
col <-  as.vector(t(matrix(col, nrow=length(col), ncol=ceiling(ncol(deathmat)/length(col)))))

# Plot each year over the months
matplot(deathmat, type="l", col=col, lty=1, xaxt="n", ylab="deaths", xlab="Month",
        main=expression(paste("Monthly view of accident deaths, UK, 1969-1984")))
axis(1, at=1:12, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
abline(a=0,b=0,lty="dashed")
#dev.off()



#ARMA
acf(data$a)
pacf(data$a)


#loop
num.index <- seq(1,18,1)

parts <- NULL
for (i in 1:18){
  parts[[i]] <- data[,i]
}

for (i in 1:18){
  plot(x=data$period, y=parts[[i]], type="l", ylim=range(min(parts[[i]]),max(parts[[i]])), main=num.index[i])
}

beta <- NULL
fit <- NULL
summary <- NULL

for (i in 1:18){
  fit[[i]] <- lm(parts[[i]] ~ data$period)
  summary[[i]] <- summary(fit[[i]])
  beta[i] <- fit[[i]]$coefficients[2]
}

#detrend
detrended <- NULL

detrended[[1]] <- detrend.ts(ts=parts[[1]], beta[1], period=data$period)
detrended[[1]]
