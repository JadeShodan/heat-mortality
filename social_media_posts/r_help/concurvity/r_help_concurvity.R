library(readr)
library(mgcv)

df <- read_rds("data_crossvalidated_post2.rds")

# Create matrices for lagged weather variables (6 day lags)
lagard <- function(x,n.lag=7) {
  n <- length(x); X <- matrix(NA,n,n.lag)
  for (i in 1:n.lag) X[i:n,i] <- x[i:n-i+1]
  X
}

dat <- list(lag=matrix(0:6,nrow(df),7,byrow=TRUE), deaths=df$deaths_total, doy=df$doy, year = df$year, month = df$month, weekday = df$weekday, week = df$week, monthday = df$monthday, time = df$time, heap=df$heap, heap_bin = df$heap_bin)
dat$temp_max <- lagard(df$temp_max)
dat$precip_daily_total <- lagard( df$precip_daily_total)


knots <- list(doy=c(0.5, 366.5)) # set knots for cyclic spline for doy
m1 <- gam(deaths~te(year, doy, bs = c("cr", "cc")) + heap + te(temp_max, lag, k=c(10,4))+ te(precip_daily_total, lag, k=c(10,4)), data = dat, family = nb, method = 'REML', select = TRUE, knots = knots)

# now increase k for (year, doy)
m2 <- gam(deaths~te(year, doy, bs = c("cr", "cc"), k=c(7, 20)) + heap + te(temp_max, lag, k=c(10,4))+ te(precip_daily_total, lag, k=c(10,4)), data = dat, family = nb, method = 'REML', select = TRUE, knots = knots)

gam.check(m1, rep=1000)
gam.check(m2, rep=1000)
concurvity(m1, full=FALSE)$worst
concurvity(m2, full=FALSE)$worst