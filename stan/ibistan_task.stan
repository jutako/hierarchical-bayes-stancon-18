// The model is adapted from:
// https://arxiv.org/abs/1506.06201
//
// See also:
// http://www.maths.bath.ac.uk/~jjf23/stan/

data {
    int<lower=1> N; //number of data points
    real ibi[N]; //IBI value

    int<lower=1> L; //number of subjects
    int<lower=1, upper=L> subj[N]; //subject id for each data point

    int<lower=1> K; //number of tasks
    int<lower=1, upper=K> task[N]; //task id for each data point
}

parameters {
    // Location
    real<lower=0> beta; //fixed intercept

    real w_tilde[K]; //re-parametrization of w (task)
    real<lower=0> sigma_w; //sd of w

    // Scale
    real<lower=0> sd_w_tilde[K]; //re-parametrization of sd_w
    real<lower=0> sigma_sd_w; //sd of sd_w

    real<lower=0> sd_sbj_tilde[L]; //re-parametrization of sd_sbj
    real<lower=0> sigma_sd_sbj; //sd of sd_sbj

    real<lower=0> sigma_e; //error sd

}

transformed parameters {

  // Define transformed parameters
  // Note: for some reason all declarations need to be
  // before the for loops!
  vector[K] w; //task effects on mean
  vector[K] sd_w; //task effects on sd
  vector[L] sd_sbj; //subject effects on sd

  // Location
  // reparametrization of w
  for (k in 1:K){
    w[k] = sigma_w * w_tilde[k];
  }

  // Scale
  // reparametrization of sd_w
  for (k in 1:K){
    sd_w[k] = sigma_sd_w * sd_w_tilde[k];
  }

  // reparametrization of sd_sbj
  for (l in 1:L){
    sd_sbj[l] = sigma_sd_sbj * sd_sbj_tilde[l];
  }
}

model {
    // priors
    beta ~ normal(88, 5);
    sigma_w ~ cauchy(0, 5);

    sigma_e ~ cauchy(15, 5);
    sigma_sd_w ~ cauchy(0, 5);
    sigma_sd_sbj ~ cauchy(0, 5);

    // Reparameterization in an attempt to make sampling more efficient
    // in the "funnel" that exists in hierarchical models:
    // See:
    // Stan manual or
    // https://arxiv.org/abs/0708.3797 or
    // http://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
    // The reparametrization is non-centered due to beta and sigma_e

    // task effect
    w_tilde ~ normal(0, 1); // implies w ~ normal(0, sigma_w)
    sd_w_tilde ~ normal(0, 1); // implies sd_w ~ normal(0, sigma_sd_w)

    // subject effect
    sd_sbj_tilde ~ normal(0, 1); // implies sd_sbj ~ normal(0, sigma_sd_sbj)


    // sampling distribution
    for (i in 1:N){
        ibi[i] ~ normal(beta + w[task[i]],
                        sigma_e + sd_w[task[i]] + sd_sbj[subj[i]]);
    }
}


// For use with loo -package
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(ibi[i] | beta + w[task[i]],
                  sigma_e + sd_w[task[i]] + sd_sbj[subj[i]]);
  }
}

