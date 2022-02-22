data {
  int<lower=1> n; // number of outcomes
  int y[n];       // outcomes
}

parameters {
  real<lower=0,upper=1> theta; // theta parameter
}

model {
  // uniform prior from 0 to 1
  theta ~ beta(1, 1);

  // the model
  y ~ bernoulli(theta);
}
