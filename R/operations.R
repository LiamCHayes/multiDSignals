### Functions to operate on signals
# Needs tidyverse and dtw


#' Takes the average signal of a list of n-dimensional signals.
#'
#' @param signal_list A list of dataframes with n columns that represent the signals to average.
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @return A single dataframe that represents the average signal.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
#'                    y = c(2,3,4,5,4,5,6,7,8,8),
#'                    z = c(1,1,2,3,2,3,4,3,4,5))
#' sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
#'                    y = c(2,3,4,5,4,5,6,7,8,8)+1,
#'                    z = c(1,1,2,3,2,3,4,3,4,5)+1)
#' sig3 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+2,
#'                    y = c(2,3,4,5,4,5,6,7,8,8)+3,
#'                    z = c(1,1,2,3,2,3,4,3,4,5)+2)
#' sigList <- list(sig1, sig2, sig3)
#' avgSignals(sigList, index=F)
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
