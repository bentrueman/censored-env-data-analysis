// modified from: Kucukelbir, A., Tran, D., Ranganath, R., Gelman, A., & Blei, D. M. (2017).
//Automatic differentiation variational inference. Journal of machine learning research.
data {
  int<lower=0> N; // number of data points in dataset
  int<lower=0> D; // dimension
  int<lower=0> M; // maximum dimension of latent space to consider
  array[N] vector[D] y; // the data
}
parameters {
  vector<lower=0>[D] mu; // mean vector
  matrix[M, N] z; // latent variables
  matrix[D, M] w; // weight parameters
  real<lower=0> sigma; // standard deviation parameter
  vector<lower=0>[M] alpha; // hyper-parameters on weights
}
model {
  // priors
  for (d in 1:D)
    mu[d] ~ lognormal(2.5, 1);
  to_vector(z) ~ normal(0, 1);
  for (d in 1:D)
    w[d] ~ normal(0, sigma * alpha);
  sigma ~ lognormal(0, 1);
  alpha ~ inv_gamma(1, 1);
  // likelihood
  for (n in 1:N)
    y[n] ~ normal(w * col(z, n) + mu, sigma);
}
