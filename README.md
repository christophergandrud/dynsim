dynsim
======

### An R implementation of dynamic simulations of autoregressive relationships

##### Version 0.2.3

##### Laron K Williams, Guy D Whitten, and [Christopher Gandrud](http://christophergandrud.blogspot.com/p/biocontact.html)

## About 

The **dynsim** package underdevelopment implements Williams and Whitten's ([2011](http://www.stata-journal.com/article.html?article=st0242), [2012](http://web.missouri.edu/~williamslaro/Williams%20and%20Whitten%202012.pdf)) method for dynamic simulations of autoregressive relationships in R.

## Process 

There are four basic steps to use **dynsim** to create dynamic simulations of autoregressive relationships:

1. *Estimate* your linear model using `zelig` from the [Zelig](http://gking.harvard.edu/zelig) package.

2. *Set up starting values* for simulation scenarios and (optionally) shock values at particular iterations (e.g. points in forecasted time).

3. *Simulate* these scenarios based on the estimated model using the `dynsim` function.

4. *Plot* the simulation results with the `dynsimGG` function.

## Examples 

For examples please visit [http://christophergandrud.github.io/dynsim/](http://christophergandrud.github.io/dynsim/).

## Install

**dynsim** is available on [CRAN](http://cran.r-project.org/web/packages/dynsim/index.html)

You can also easily install the latest development version with the [devtools](http://cran.r-project.org/web/packages/devtools/index.html) package:

```
devtools::install_github("dynsim", "christophergandrud")
```
