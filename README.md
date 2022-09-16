
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lab3

<!-- badges: start -->
<!-- badges: end -->

The goal of lab3 is to write a package that is version controlled on
github including 2 functions

## Installation

You can install the development version of lab3 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fatemeh879/lab3")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` euclidean

library(lab3)
## Euclidean algorithem example
euclidean(123612, 13892347912)
euclidean(100, 1000)

## Dijkstra algorithem example

wiki_graph <-
data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
