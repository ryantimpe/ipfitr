## Introduction

[Demo](http://www.ryantimpe.com/files/Intro_to_ipfitr.html)

In market sizing and forecasting, we're often tasked with estimating large data tables with limited data or only tables of subtotals. For example, one might know the number of people employed in the a country by age range, as well as the gender breakdown of total employment without age. Using iterative proportion fitting with these two data series, one can estimate the number of people employed by age range AND gender.

[Iterative proportion fitting](https://en.wikipedia.org/wiki/Iterative_proportional_fitting) (IPF) has been used in market analysis for over 30 years. This package integrates the methodology, along with some more expansions of this technique, into the [tidyverse](https://github.com/tidyverse/tidyverse) in R.

The ipfitr package for R allows the user to input multiple high-level summary/aggregate data tables and create a single, full-dimension data table estimating individual cell values using iterative proportion fitting.

Here's the link to a [webpage with a demo of key features](http://www.ryantimpe.com/files/Intro_to_ipfitr.html). See my [GitHub](https://github.com/ryantimpe/ipfitr) for the script and latest updates. 


## Getting Started

The ipfitr package performs iterative proportion fitting on a seed datatable, continuously scaling values to each target until the seed values sum to every supplied target. If no seed is supplied, the function begins with a seed of 1 for every value. 

Changing the seed will result in different final results (see Examples 1a and 2 in [the demo](http://www.ryantimpe.com/files/Intro_to_ipfitr.html)). For this reason, it's best to include as much information as you can about the relative values of the cells in the seed.

### Prerequisites

This package is built using the [tidyverse](https://github.com/tidyverse/tidyverse) packages. 

The functions in ipfitr can be run independently, but are more useful as part of a %>% workflow.

### Installing

``` r
install.packages(devtools)
devtools::install_github("ryantimpe/ipfitr")
```

## Deployment

## Built With

* [R](https://www.r-project.org/) - R
* [tidyverse](https://github.com/tidyverse/tidyverse) - Tidyverse packages + ggplot2

## Versioning

For the versions available, see the [tags on this repository](https://github.com/ryantimpe/ipfitr/tags). 

## Authors

* **Ryan Timpe** - *Initial work* - [ryantimpe](https://github.com/ryantimpe)
* International Planning & Research - [iprcorp.com](https://iprcorp.com/)

See also the list of [contributors](https://github.com/ryantimpe/ipfitr/contributors) who participated in this project.

## License

No license.


