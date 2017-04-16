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