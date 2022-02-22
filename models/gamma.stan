data {
  int<lower=1> n; // total number of data points
  vector[n] y;    // target variable
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  // model
  y ~ gamma(alpha, beta);
}

generated quantities {
  real<lower=0> mean_isi;
  
  mean_isi = alpha/beta;
}