#checks for seasonality using divisibility of acf lag indices with prescribed season 'd'
acfIndex <- function(vec, n.max = 1, mod = NA) {
  # input : vector of acf values; output : data frame of the top n.max values and their indices
  stopifnot(n.max <= length(vec));
  val <- rep(NA, n.max); idx <- rep(NA, n.max)
  for (i in 1:n.max) {
    val[i] <- max(vec); idx[i] <- which.max(vec)
    vec <- vec[-idx[i]]
  }
  if (is.na(mod) == FALSE) {
    mod.vec <- idx %% mod
    return(data.frame(index = idx, value = val, remainder = mod.vec))
  }
  return(data.frame(index = idx, value = val))
}

#performs manual differencing, but same can be done with differences argument in diff()
difference <- function(dta, lag.input = 1, order = 1) {
# Performs differencing for any degree of regular differencing
  time <- dta[,1]; ts <- dta[,2]; 
  ts.out <- diff(ts, lag = lag.input, differences = order)
  return(data.frame(Date = time[(order + 1):length(time)], Activity = ts.out))
}

##Old Wrong Question 4

Q4Train <- data$Q4Train

plot(Q4Train, type = 'l', xlab = "Date", ylab = "Google Data")

Q4Train.Log <- data.frame(Date = Q4Train$Date, Activity = log(Q4Train$activity + 2))
plot(Q4Train.Log, type = 'l', xlab = "Date", ylab = "Log Google Data")
q4train.log <- Q4Train.Log$Activity

# Observe first and second differenced log data
q4firstdiff <- diff(q4train.log)
q4seconddiff <- diff(diff(q4train.log))

# Observe differenced data of orders 1,2
plot(q4firstdiff, type = 'l', xlab = "Date", ylab = "1st Diff Google Date");
plot(q4seconddiff, type = 'l', xlab = "Date", ylab = "2nd Diff Google Date")


# Observe acf of data of orders 1, 2
#par(mfrow = c(2,1))
acf(q4firstdiff, lag.max = 300)
acf(q4seconddiff, lag.max = 100)

#par(mfrow = c(2,1))
pacf(q4firstdiff, lag.max = 100)
pacf(q4seconddiff, lag.max = 100)

