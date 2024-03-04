### Functions to smooth signals


#' Smooth a signal with a moving average function
#'
#' @param signal A dataframe with n columns.
#' @param window The size of the moving average window. Each point i has a window of (i-window, i+window).
#' @param index Boolean which is true if there is an index column in the dataframes (index column must be the first column).
#'
#' @importFrom dplyr relocate
#' @importFrom tibble add_column
#'
#' @return A dataframe with n columns representing the smoothed signal.
#' @export
#'
#' @examples
#' sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
#'                    y = c(2,3,4,5,4,5,6,7,8,8),
#'                    z = c(1,1,2,3,2,3,4,3,4,5))
#' movingAverage(sig1, 2, index=FALSE)
movingAverage <- function(signal, window, index=TRUE) {
  if (index) idx <- 2:ncol(signal)
  else idx <- 1:ncol(signal)
  smoothed <- list()
  for (s in idx) {
    sm <- rep(0, nrow(signal))
    for (i in 1:nrow(signal)) {
      if (i < window || i > nrow(signal)-window) sm[i] <- signal[[s]][i]
      else sm[i] <- mean(signal[[s]][(i-window):(i+window)])
    }
    smoothed <- c(smoothed, list(sm))
  }
  smoothed <- smoothed %>% as.data.frame()
  if (index) {
    smoothed <- smoothed %>%
      add_column(index = 1:nrow(smoothed)) %>%
      relocate(index)
  }
  colnames(smoothed) <- colnames(signal)
  smoothed
}





