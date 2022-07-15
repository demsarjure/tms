data {
  int<lower=1> n; // total number of data points
  int y[n];       // target variable
}

parameters {
  real lambda; // mu
}

model {
  // model
  y ~ poisson(lambda);
}
