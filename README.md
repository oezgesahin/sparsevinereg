
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparsevinereg: High-dimensional sparse vine copula regression

<!-- badges: start -->

<!-- badges: end -->

An R package that selects variables from high-dimensional continuous
data to make vine copula based (quantile) univariate predictions.

It depends on [vinereg](https://github.com/tnagler/vinereg) and
[kde1d](https://github.com/tnagler/kde1d).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("oezgesahin/sparsevinereg")
```

## Package overview

Below is an overview of some functions and features.

### Simulate data with 5 relevant and 5 irrelevant variables

``` r
library(sparsevinereg)
set.seed(11)
x <- matrix(rnorm(5000), 500, 10)
y <- x[,1] -2*x[,2] + 3*x[,3] + 5*x[,4] - 4*x[,5]

# response is in the first column of the data
data <- data.frame(y = y, x = x)
```

### fit a sparse vine copula based regression model

``` r
fit <- sparsevinereg(data)
```

### fit a sparse vine copula based regression model based on partial correlations

``` r
fit_ParCor <- sparsevinereg(data, varsel='ParCor')
```

### print the main statistics of the model

``` r
print(fit)
#> method = resid   vine = Dvine   vars_indx = 4 5 3 2 1
```

### make predictions with the model at the quantile level 0.90

``` r
upper_pred <- predict(fit, data, 0.90)
```

### make predictions with the model at the mean

``` r
mean_pred <- predict(fit, data, NA)
```

## Contact

Please contact <ozgesahin-94@hotmail.com> if you have any questions.

## References

Sahin, O., and Czado, C. (2022). High-dimensional sparse vine copula
regression with application to genomic prediction. arXiv preprint
arXiv:2208.12383. [preprint](https://arxiv.org/pdf/2208.12383.pdf).
