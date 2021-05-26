<img src= "hexSticker/hexSticker_flibm.png" width="150">

FLIBM is individual-based model for the simulation of fish stock dynamics. Results of the simulation are recorded in FLR objects ([Fisheries Library for R](http://www.flr-project.org)) (*e.g.* `FLQuant`, `FLStock`). The IBM framework generates age- and length-based data, allowing for integration in a range of stock assessment approaches with differing data requirements. 

Current configuration allows for the specification of individual variation according to growth and maturity, but the IBM can be expanded to include other variables, e.g. area- and sex-specific differences (`area` and `unit` variables, respectively).

For additional information on FLIBM's use and capabilities, the project's repository contains several tutorials (e.g. [*FLIBM manual*](https://raw.githack.com/marchtaylor/FLIBM/master/doc/FLIBM_Manual.html), [*Assessment with FLIBM objects*](https://raw.githack.com/marchtaylor/FLIBM/master/doc/Assessment_with_FLIBM_objects.html))

For version release notes, see [news.md](https://github.com/marchtaylor/FLIBM/blob/master/doc/news.md)

To install the development version, you can use:
```
devtools::install_github("marchtaylor/FLIBM", INSTALL_opts=c("--no-multiarch"))
```

WARNING: FLIBM requires a 64 bit installation of R. Installation from source 
in R for Windows should be carried out using --no-multiarch for a 64-bit-only 
installation.

Installation via `devtools::install_github` may fail with warnings. Another option is to try a downloading the most recent release, and installing manually:

```
myurl <- "https://github.com/marchtaylor/FLIBM/archive/v0.3.3.tar.gz"
z <- tempfile()
download.file(myurl, z, mode="wb")
install.packages(z, INSTALL_opts=c("--no-multiarch"))
file.remove(z) # cleanup
```
