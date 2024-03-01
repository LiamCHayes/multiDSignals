### Functions to classify signals
# Needs functions from compare.R

# Classify a signal. Out of the possible options, which option is the signal
# closest to? signalOptions is a list of dataframes and signal is a
# dataframe.
classifySignals <- function(signalOptions, signal, index=T) {
  distances <- rep(0, length(signalOptions))
  for (s in signalOptions) {
    compareSignals(s, signal, index)
  }
  which.min(distances)
}

# Classify a signal based on the n highest variance signals. Out of the possible
# options, which option is the signal closest to? signalOptions is a list of
# dataframes and signal is a dataframe.
classifySignals_highVar <- function(signalOptions, signal, nSigs, index=T) {
  distances <- rep(0, length(signalOptions))
  for (s in signalOptions) {
    compareSignals_highVar(s, signal, nSigs, index)
  }
  which.min(distances)
}
