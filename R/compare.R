### Functions to compare signals
# Needs tidyverse and dtw packages

# Takes two multi-dimensional signals (dataframes) and compares them. Returns a
# distance measure.
compareSignals <- function(signal1, signal2, index=T) {
  # Check if data frames have same number of columns
  if (length(sapply(list(signal1, signal2), ncol) %>% unique) != 1) {
    stop('Data frames have a different number of columns')
  }

  # Get indexes of individual signals to compare
  ifelse(index, nSigs <- 2:ncol(signal1), nSigs <- 1:ncol(signal1))

  # Make sure lengths of signals are not too far from each other
  if (which.max(c(nrow(signal1), nrow(signal2))) == 1) {
    while (nrow(signal2) < nrow(signal1)/2) signal2 <- lapply(signal2, rep, each=2) %>% as.data.frame()
  } else {
    while (nrow(signal1) < nrow(signal2)/2) signal1 <- lapply(signal1, rep, each=2) %>% as.data.frame()
  }

  # Compare each signal and average distances
  distances <- rep(0, length(nSigs))
  i <- 1
  for (s in nSigs) {
    distances[i] <- dtw(signal1[,s], signal2[,s], k=T, step=typeIIIc)$distance
    i <- i+1
  }
  mean(distances)
}

# Compares the n highest variance signals. Takes the variance measurements from
# signal 2 and compares those to the matching signals from signal1.
compareSignals_highVar <- function(signal1, signal2, nSigs, index=T) {
  # Check if data frames have same number of columns
  if (length(sapply(list(signal1, signal2), ncol) %>% unique) != 1) {
    stop('Data frames have a different number of columns')
  }

  # Get indexes of signals to compare
  maxVarIdx <- sapply(signal2, var)[ifelse(index, 2, 1):46] %>%
    order(decreasing = T) + ifelse(index, 1, 0)
  maxVarIdx <- maxVarIdx[1:nSigs]

  # Make sure lengths of signals are not too far from each other
  if (which.max(c(nrow(signal1), nrow(signal2))) == 1) {
    while (nrow(signal2) < nrow(signal1)/2) signal2 <- lapply(signal2, rep, each=2) %>% as.data.frame()
  } else {
    while (nrow(signal1) < nrow(signal2)/2) signal1 <- lapply(signal1, rep, each=2) %>% as.data.frame()
  }

  # Compare each signal and average distances
  distances <- rep(0, length(maxVarIdx))
  i <- 1
  for (s in maxVarIdx) {
    distances[i] <- dtw(signal1[,s], signal2[,s], k=T, step=typeIIIc)$distance
    i <- i+1
  }
  mean(distances)
}
