// from: Stan Users' Guide
// Copyright (c) 2018--2022, Stan Development Team and their Assignees.
// https://github.com/stan-dev/docs/blob/master/src/stan-users-guide/regression.qmd
// distributed under the CC BY-ND 4.0 license

data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
}
