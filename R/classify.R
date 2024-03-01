### Functions to classify signals
# Needs functions from compare.R


#' Classify a signal.
#'
#' @param signalOptions A list of dataframes with n columns which represent the options to classify signal as.
#' @param signal The signal to classify.
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @return The index of signalOptions that signal is classified as.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10), y = c(2,3,4,5,4,5,6,7,8,8), z = c(1,1,2,3,2,3,4,3,4,5))
#' sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1, y = c(2,3,4,5,4,5,6,7,8,8)+1, z = c(1,1,2,3,2,3,4,3,4,5)+1)
#' sig3 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+2, y = c(2,3,4,5,4,5,6,7,8,8)+3, z = c(1,1,2,3,2,3,4,3,4,5)+2)
#' sigList <- list(sig1, sig2, sig3)
#' classifySignals(signalOption, sig1, index=F)
classifySignals <- function(signalOptions, signal, index=T) {
  distances <- rep(0, length(signalOptions))
  for (s in signalOptions) {
    compareSignals(s, signal, index)
  }
  which.min(distances)
}


#' Classify a signal based on the nSigs highest variance signals.
#'
#' @param signalOptions A list of dataframes with n columns which represent the options to classify signal as.
#' @param signal The signal to classify.
#' @param nSigs The number of highest variance signals to compare by.
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @return The index of signalOptions that signal is classified as.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10), y = c(2,3,4,5,4,5,6,7,8,8), z = c(1,1,2,3,2,3,4,3,4,5))
#' sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1, y = c(2,3,4,5,4,5,6,7,8,8)+1, z = c(1,1,2,3,2,3,4,3,4,5)+1)
#' sig3 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+2, y = c(2,3,4,5,4,5,6,7,8,8)+3, z = c(1,1,2,3,2,3,4,3,4,5)+2)
#' sigList <- list(sig1, sig2, sig3)
#' classifySignals_highVar(signalOption, sig1, nSigs=2, index=F)
classifySignals_highVar <- function(signalOptions, signal, nSigs, index=T) {
  distances <- rep(0, length(signalOptions))
  for (s in signalOptions) {
    compareSignals_highVar(s, signal, nSigs, index)
  }
  which.min(distances)
}
