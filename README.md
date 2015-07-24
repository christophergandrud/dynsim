dynsim
======

### Dynamic Simulations of Autoregressive Relationships

##### Version 1.2

[![Build Status](https://travis-ci.org/christophergandrud/dynsim.png)](https://travis-ci.org/christophergandrud/dynsim)

##### [Christopher Gandrud](http://christophergandrud.blogspot.com/p/biocontact.html),
Laron K Williams, and Guy D Whitten

## About

The **dynsim** package implements Williams and Whitten's
([2011](http://www.stata-journal.com/article.html?article=st0242), [2012](http://web.missouri.edu/~williamslaro/Williams%20and%20Whitten%202012.pdf))

method for dynamic simulations of autoregressive relationships in R.

## Process

There are four basic steps to use **dynsim** to create dynamic simulations of
autoregressive relationships:

1. *Estimate* your linear model using `lm` or similar functions.

2. *Set up starting values* for simulation scenarios and (optionally) shock
values at particular iterations (e.g. points in forecasted time).

3. *Simulate* these scenarios based on the estimated model using the `dynsim`
function.

4. *Plot* the simulation results with the `dynsimGG` function.

## Examples

For examples please visit
[http://christophergandrud.github.io/dynsim/](http://christophergandrud.github.io/dynsim/).

## Install

**dynsim** is available on
[CRAN](http://cran.r-project.org/package=dynsim)

You can also easily install the latest development version with the
[devtools](http://cran.r-project.org/package=devtools) package:

```{S}
devtools::install_github("christophergandrud/dynsim")
```
