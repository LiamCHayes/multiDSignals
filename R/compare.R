### Functions to compare signals


#' Compare two n-dimesional signals
#'
#' @param signal1 A dataframe with n columns.
#' @param signal2 A dataframe with n columns.
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @importFrom dtw dtw typeIIIc
#'
#' @return A numeric representing the distance measure between the two signals.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
#'                    y = c(2,3,4,5,4,5,6,7,8,8),
#'                    z = c(1,1,2,3,2,3,4,3,4,5))
#' sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
#'                    y = c(2,3,4,5,4,5,6,7,8,8)+1,
#'                    z = c(1,1,2,3,2,3,4,3,4,5)+1)
#' compareSignals(sig1, sig2, index=FALSE)
compareSignals <- function(signal1, signal2, index=TRUE) {
  # Check if data frames have same number of columns
  if (length(sapply(list(signal1, signal2), ncol) %>% unique) != 1) {
    stop('Data frames have a different number of columns')
  }

  # Get indexes of individual signals to compare
  ifelse(index, nSigs <- 2:ncol(signal1), nSigs <- 1:ncol(signal1))

  # Make sure lengths of signals are not too far from each other
  if (which.max(c(nrow(signal1), nrow(signal2))) == 1) {
    while (nrow(signal2) <= nrow(signal1)/2) signal2 <- lapply(signal2, rep, each=2) %>% as.data.frame()
  } else {
    while (nrow(signal1) <= nrow(signal2)/2) signal1 <- lapply(signal1, rep, each=2) %>% as.data.frame()
  }

  # Compare each signal and average distances
  distances <- rep(0, length(nSigs))
  i <- 1
  for (s in nSigs) {
    distances[i] <- dtw(signal1[,s], signal2[,s], keep.internals=TRUE, step.pattern=typeIIIc)$distance
    i <- i+1
  }
  mean(distances)
}


#' Compares the m highest variance signals from two n-dimesional signals (m < n).
#'
#' @param signal1 A dataframe with n columns.
#' @param signal2 A dataframe with n columns.
#' @param nSigs Number of signals to compare. Will choose the nSigs highest variance signals from signal 2 and compare them to the corresponding signals from signal1.
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @importFrom dtw dtw typeIIIc
#' @importFrom stats var
#'
#' @return A numeric representing the distance measure between the two signals.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
#'                    y = c(2,3,4,5,4,5,6,7,8,8),
#'                    z = c(1,1,2,3,2,3,4,3,4,5))
#' sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
#'                    y = c(2,3,4,5,4,5,6,7,8,8)+1,
#'                    z = c(1,1,2,3,2,3,4,3,4,5)+1)
#' compareSignals_highVar(sig1, sig2, nSigs=2, index=FALSE)
compareSignals_highVar <- function(signal1, signal2, nSigs, index=TRUE) {
  # Check if data frames have same number of columns
  if (length(sapply(list(signal1, signal2), ncol) %>% unique) != 1) {
    stop('Data frames have a different number of columns')
  }

  # Get indexes of signals to compare
  maxVarIdx <- sapply(signal2, var)[ifelse(index, 2, 1):46] %>%
    order(decreasing = TRUE) + ifelse(index, 1, 0)
  maxVarIdx <- maxVarIdx[1:nSigs]

  # Make sure lengths of signals are not too far from each other
  if (which.max(c(nrow(signal1), nrow(signal2))) == 1) {
    while (nrow(signal2) <= nrow(signal1)/2) signal2 <- lapply(signal2, rep, each=2) %>% as.data.frame()
  } else {
    while (nrow(signal1) <= nrow(signal2)/2) signal1 <- lapply(signal1, rep, each=2) %>% as.data.frame()
  }

  # Compare each signal and average distances
  distances <- rep(0, length(maxVarIdx))
  i <- 1
  for (s in maxVarIdx) {
    distances[i] <- dtw(signal1[,s], signal2[,s], keep.internals=TRUE, step.pattern=typeIIIc)$distance
    i <- i+1
  }
  mean(distances)
}
