
## Edit stan models using R

This package facilitates editing of stan model code using R. This
includes adding/removing blocks, extracting variable names, and
inserting/removing lines.

Development of this package was inspired by the `<bi_model>` class in
the [rbi](https://github.com/sbfnk/rbi) package.

## Installation

The development version can be installed using `pak`

``` r
pak::pak("epiforecasts/stanedit")
```

## Example

First we load a stan model and prepares it for editing using the
functionality in this package.

``` r
library("stanedit")
model_file_name <- system.file(package = "stanedit", "regression.stan")
reg <- stanedit(filename = model_file_name)
reg
```

    ## data {
    ##   int<lower=0> N;
    ##   vector[N] x;
    ##   vector[N] y;
    ## }
    ## parameters {
    ##   real alpha;
    ##   real beta;
    ##   real<lower=0> sigma;
    ## }
    ## model {
    ##   y ~ normal(alpha + beta * x, sigma);
    ## }

Let’s say we want to add standard normal priors for alpha and beta. We
can do this using

``` r
insert_lines(reg,
  lines = c(
    "alpha ~ std_normal();",
    "beta ~ std_normal();"
  ),
  at_end_of = "model"
)
```

    ## data {
    ##   int<lower=0> N;
    ##   vector[N] x;
    ##   vector[N] y;
    ## }
    ## parameters {
    ##   real alpha;
    ##   real beta;
    ##   real<lower=0> sigma;
    ## }
    ## model {
    ##   y ~ normal(alpha + beta * x, sigma);
    ##   alpha ~ std_normal();
    ##   beta ~ std_normal();
    ## }

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

<a href="https://github.com/epiforecasts/stanedit/commits?author=sbfnk">sbfnk</a>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
