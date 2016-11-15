data {
  int L; int N_area; int N_car;
  int i_car[L];
  int i_area[L];
  int n[L];
  # int n_area[N_area];
  # int n_car[N_car];
}
parameters {
  vector<lower=0>[N_car] lambda_car;
  vector<lower=0>[N_area] lambda_area;
  real<lower=0> theta;
}
model {
  # Add some priors here
  n ~ neg_binomial_2(lambda_car[i_car] .* lambda_area[i_area], theta);
}
