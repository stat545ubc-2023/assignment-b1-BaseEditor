Assignment-b1-v3
================
2023-11-03

*Stats 545B Assignment B-1*

The code and explanations following are intended to fulfil requirements
for Stats545B assignment B-1.

*Packages to load*

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.3.2

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(dplyr)
library(gapminder)
library(digest)
library(palmerpenguins)
library(commonmark)
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 4.3.2

    ## Loading required package: usethis
    ## 
    ## Attaching package: 'devtools'
    ## 
    ## The following object is masked from 'package:testthat':
    ## 
    ##     test_file

``` r
library(roxygen2)
```

    ## Warning: package 'roxygen2' was built under R version 4.3.2

*Exercise 1: Make a Function, & Exercise 2: Document your Function*

This is a simple function designed to quickly return some simple summary
statistics.

``` r
#' Generate Summary Statistics
#'
#' @description This function allows you to produce simple summary statistics for numeric variables. The summary statistics returned are range, mean, and median.
#'
#' @param x A numeric variable of interest
#'
#' @return Range, mean, and median
#'
#' @examples
#' summary_stats(penguins$bill_length_mm)
#' summary_stats(gapminder$lifeExp)
summary_stats <- function(x){
  if(!is.numeric(x)) {
    stop('Input is not of class numeric: Input must be of class numeric')}
  range <- max(x) - min(x)
  mean <- mean(x)
  median <- median(x)
  return(c(range, mean, median))
}
```

*Exercise 3: Include Examples*

Example 1:

Here is an example using the palmerspenguins::penguins dataset. Here, I
filter penguins to only include the adelie species, then drop na values.
Finally, I put the new dataset through the summary_stats function to
return summary statistics about the adelie species’ bill length.

``` r
adelie <- penguins %>%
  filter(species == "Adelie") %>%
  drop_na()

summary_stats(adelie$bill_length_mm)
```

    ## [1] 13.90000 38.82397 38.85000

Example 2:

Here is an example using the gapminder::gapminder dataset. In this
example, I filter gapminder to only include European countries, then
input this dataset into the function to return summary statistics for
life expectancy in Europe.

``` r
gap_Europe <- gapminder %>%
  filter(continent == "Europe")

summary_stats(gap_Europe$lifeExp)
```

    ## [1] 38.17200 71.90369 72.24100

*Exercise 4: Test the Function*

These are the tests for my summary_statistics function.

``` r
test_that("Testing summary statistics function",{
  expect_equal(summary_stats(adelie$bill_length_mm), c(max(adelie$bill_length_mm) - min(adelie$bill_length_mm), mean(adelie$bill_length_mm), median(adelie$bill_length_mm))) 
          })
```

    ## Test passed 😸

``` r
test_that("Testing summary statistics no error",{
  expect_no_error(summary_stats(gapminder$lifeExp)) 
          })
```

    ## Test passed 🥳

``` r
test_that("Testing summary statistics error message",{
  expect_error(summary_stats(gapminder$country), "Input is not of class numeric: Input must be of class numeric") 
          })
```

    ## Test passed 😀

``` r
test_that("Testing summary statistics return length",{
  expect_length(summary_stats(gapminder$lifeExp), 3) 
          })
```

    ## Test passed 🎊
