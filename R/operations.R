### Functions to operate on signals
# Needs tidyverse and dtw

# Takes a list of dataframes of signals and returns the elementwise average of
# the signals. All dataframes must have the same number of columns but can have
# a different number of rows. If index is true, it means the list of dataframes
# has an index in the first row of each dataframe. Returns 1 dataframe with
# averages of all the signals.
avgSignals <- function(signal_list, index=T) {
  # Check if data frames have same number of columns
  if (length(sapply(signal_list, ncol) %>% unique) != 1) {
    stop('Data frames have a different number of columns')
  }

  # Get column indexes to average over
  ncols <- ncol(signal_list[[1]])
  ifelse(index, col_idx <- seq(2,ncols), col_idx <- seq(1,ncols))

  # Average over dfs
  avg <- list()
  for (idx in col_idx) {
    sigs <- lapply(signal_list, pull, idx)
    avgVec <- sigs[[1]]
    for (i in 2:length(sigs)) {
      # Align vectors with dtw
      if (which.max(c(length(avgVec), length(sigs[[i]]))) == 1) {
        while (length(sigs[[i]]) < length(avgVec)/2) sigs[[i]] <- rep(sigs[[i]], each=2)
      } else {
        while (length(avgVec) < length(sigs[[i]])/2) avgVec <- rep(avgVec, each=2)
      }
      alignment <- dtw(avgVec, sigs[[i]], k=T, step=typeIIIc)
      new_vec2 <- rep(0, length(alignment$index2))
      new_vec2[alignment$index1] <- sigs[[i]][alignment$index2]

      # Average with existing avgVec
      avgVec <- (avgVec + new_vec2) / 2
    }
    avg <- c(avg, list(avgVec))
  }
  avg <- avg %>% as.data.frame()
  if (index) {
    avg <- avg %>%
      add_column(index = 1:nrow(avg)) %>%
      relocate(index)
  }
  colnames(avg) <- colnames(signal_list[[1]])
  avg
}
