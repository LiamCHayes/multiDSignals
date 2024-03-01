
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiDSignals

<!-- badges: start -->
<!-- badges: end -->

The goal of multiDSignals is to facilitate working with
multi-dimensional signals. Includes operations, comparing, smoothing,
aligning, and classifying functions.

## Installation

You can install the development version of multiDSignals like so:

``` r
# install.packages("devtools")
devtools::install_github("LiamCHayes/multiDSignals")
```

## Example

``` r
# Setup
devtools::install_github("LiamCHayes/multiDSignals")
#> Downloading GitHub repo LiamCHayes/multiDSignals@HEAD
#> Skipping 4 packages not available: tibble, magrittr, dtw, dplyr
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\lchco\AppData\Local\Temp\RtmpGCgXkv\remotes28384439462a\LiamCHayes-multiDSignals-d81a229/DESCRIPTION' ...  ✔  checking for file 'C:\Users\lchco\AppData\Local\Temp\RtmpGCgXkv\remotes28384439462a\LiamCHayes-multiDSignals-d81a229/DESCRIPTION'
#>       ─  preparing 'multiDSignals':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building 'multiDSignals_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/lchco/AppData/Local/R/win-library/4.3'
#> (as 'lib' is unspecified)
library(multiDSignals)
```

Compare multi-dimensional signals with a single function:

``` r
sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
                  y = c(2,3,4,5,4,5,6,7,8,8),
                  z = c(1,1,2,3,2,3,4,3,4,5))
sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
                  y = c(2,3,4,5,4,5,6,7,8,8)+1,
                  z = c(1,1,2,3,2,3,4,3,4,5)+1)

# Compare all dimensions from sig1 and sig2
compareSignals(sig1, sig2, index=FALSE)
#> [1] 4

# Compare the 2 highest variance signals from sig1 and sig2
compareSignals_highVar(sig1, sig2, nSigs=2, index=FALSE)
#> [1] 3.5
```

Classify signals like so:

``` r
sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
                   y = c(2,3,4,5,4,5,6,7,8,8),
                   z = c(1,1,2,3,2,3,4,3,4,5))
sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
                   y = c(2,3,4,5,4,5,6,7,8,8)+1,
                   z = c(1,1,2,3,2,3,4,3,4,5)+1)
sig3 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+2,
                   y = c(2,3,4,5,4,5,6,7,8,8)+3,
                   z = c(1,1,2,3,2,3,4,3,4,5)+2)
sigList <- list(sig1, sig2, sig3)

# Classify sig1 using all dimensions
classifySignals(sigList, sig1, index=FALSE)
#> [1] 1

# Classify sig1 using the 2 highest variance dimensions
classifySignals_highVar(sigList, sig1, nSigs=2, index=F)
#> [1] 1
```

You can also perform operations on multi-dimensional signals:

``` r
sig1 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10),
                   y = c(2,3,4,5,4,5,6,7,8,8),
                   z = c(1,1,2,3,2,3,4,3,4,5))
sig2 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+1,
                   y = c(2,3,4,5,4,5,6,7,8,8)+1,
                   z = c(1,1,2,3,2,3,4,3,4,5)+1)
sig3 <- data.frame(x = c(1,2,3,4,5,6,7,8,9,10)+2,
                   y = c(2,3,4,5,4,5,6,7,8,8)+3,
                   z = c(1,1,2,3,2,3,4,3,4,5)+2)
sigList <- list(sig1, sig2, sig3)

# Get the average of sig1, sig2, and sig3
avgSignals(sigList, index=FALSE)
#>        x    y    z
#> 1   2.25 3.75 2.25
#> 2   3.25 4.75 2.25
#> 3   3.50 5.00 2.50
#> 4   4.50 6.00 3.50
#> 5   5.00 5.75 3.25
#> 6   6.00 6.00 3.50
#> 7   7.00 6.50 4.00
#> 8   8.00 7.50 4.25
#> 9   9.50 9.00 4.50
#> 10 11.25 9.75 6.25
```
