data {
    int<lower=0> n;
    vector[n] mpg;
    vector[n] weight;

}

parameters {
   real alpha;
   real beta_w;
   real sigma;
}

model {
   // mod
   vector[n] mu;
   mu = alpha + beta_w * weight + sigma;
   // prior 
   alpha ~ normal(20,5);
   beta_w ~ normal(-10, 5);
    // likelihood 
    mpg ~ normal(mu, sigma);
}
generated quantities {
   /* ... declarations ... statements ... */
    array[n] real y_rep ;
    y_rep = normal_rng(alpha + beta_w * weight, sigma);

}
