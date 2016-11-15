functions {
  row_vector col_sums(matrix X) {
    row_vector[cols(X)] s;
    s = rep_row_vector(1, rows(X)) * X;
    return s; }

  vector row_sums(matrix X) {
    vector[rows(X)] s;  
    s = X * rep_vector(1, cols(X)) ;
    return s; }
}

data {
  int L; int K; int N_area; int N_car; # L is number of samples, K is number of factors
  int i_car[L];
  int i_area[L];
  int n[L];
  # int n_area[N_area];
  # int n_car[N_car];
}

parameters {
  matrix<lower=0>[N_car, K] lambda_car;
  matrix<lower=0>[N_area, K] lambda_area;
  real<lower=0> theta;
  vector<lower=0>[K] sigma_lcar;
  vector<lower=0>[K] logmu_larea; 
  vector<lower=0>[K] sigma_larea;
}

model {
  # Scalar parameter priors are left improper.
  for (k in 1:K) lambda_car[:, k] ~ lognormal(0, sigma_lcar[k]); # exp(0) to fix scale indeterminacy
  for (k in 1:K) lambda_area[:, k] ~ lognormal(logmu_larea[k], sigma_larea[k]);
  n ~ neg_binomial_2(row_sums(lambda_car[i_car, :] .* lambda_area[i_area, :]), theta);
}
