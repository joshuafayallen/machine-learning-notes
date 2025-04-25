data {
    int<lower=0> n;
    vector[n] mpg;
    vector[n] weight;
    int <lower = 1> n_trans_type;
    array[n_trans_type] int<lower=1, upper=n_trans_type> trans_type;
}

parameters {
   real beta_w;
   vector[n_trans_type] beta_trans_raw;
   real sigma;
}

transformed parameters {
    vector[n_trans_type] beta_trans;
    beta_trans = beta_trans_raw - mean(beta_trans_raw); // enforce sum-to-zero constraint
}

model {
   // mod
   vector[n] mu;
   for (i in 1:n) {
        mu[i] = beta_w * weight[i] + beta_trans[trans_type[i]];
    }
   // prior 
   beta_trans_raw ~ normal(0,5);
   beta_w ~ normal(-10, 5);
    // likelihood 
    mpg ~ normal(mu, sigma);
}
