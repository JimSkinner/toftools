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

For GCIMS data, use 'analyse_GCIMS':
``` r
analyse_GCIMS("path/to/directory")
```
there the directory is expected to contain pre-processed GC-IMS data (as output by the pre-processing pipeline). The first letter of the filenames will be taken as the class of interest, and there should ony be 2 unique first letters.