// The model is adapted from:
// https://arxiv.org/abs/1506.06201
//
// See also:
// http://www.maths.bath.ac.uk/~jjf23/stan/

data {
    int<lower=1> N; //number of data points
    real ibi[N]; //IBI value

    int<lower=1> K; //number of tasks
    int<lower=1, upper=K> task[N]; //task id for each data point
}

parameters {
    // Location
    real<lower=0> beta; //fixed intercept

    real w_tilde[K]; //re-parametrization of w (task)
    real<lower=0> sigma_w; //sd of w

    // Scale
    real<lower=0> sigma_e; //error sd

}

transformed parameters {

  // Define transformed parameters
  // Note: for some reason all declarations need to be
  // before the for loops!
  vector[K] w; //task effects on mean

  // Location
  // reparametrization of w
  for (k in 1:K){
    w[k] = sigma_w * w_tilde[k];
  }

}

model {
    // priors
    beta ~ normal(88, 5);
    sigma_w ~ cauchy(0, 10);

    sigma_e ~ cauchy(15, 5);

    // task effect
    w_tilde ~ normal(0, 1); // implies w ~ normal(0, sigma_w)

    // sampling distribution
    for (i in 1:N){
        ibi[i] ~ normal(beta + w[task[i]], sigma_e);
    }
}


// For use with loo -package
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(ibi[i] | beta + w[task[i]], sigma_e);
  }
}
