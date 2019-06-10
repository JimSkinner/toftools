# toftools

An R package for automating common tasks with ToF data

## Installation

You can install the released version of toftools from [github](https://github.com/JimSkinner/toftools) with:

``` r
devtools::install_github("toftools")
```

## Example

Use 'analyse' to make a html document from a tof file:

``` r
library(toftools)
analyse("path/to/tof/file.txt")

# Alternatively, use analyse_dir to analyse() every tof file in a directory
analyse_dir("path/to/tof")
```

