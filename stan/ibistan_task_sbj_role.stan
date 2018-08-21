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

    int<lower=1> J; //number of roles
    int<lower=1, upper=J> role[N]; //role id for each data point
}

parameters {
    // Location
    real<lower=0> beta; //fixed intercept

    real w_tilde[K]; //re-parametrization of w
    real<lower=0> sigma_w; //sd of w

    real s_tilde[L]; //re-parametrization of s (subject)
    real<lower=0> sigma_s; //sd of s

    real r_tilde[J]; //re-parametrization of r (role)
    real<lower=0> sigma_r; //sd of r

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
  vector[L] s; //subject effects on mean
  vector[J] r; //role effects on mean

  vector[K] sd_w; //task effects on sd
  vector[L] sd_sbj; //subject effects on sd

  // Location
  // reparametrization of w
  for (k in 1:K){
    w[k] = sigma_w * w_tilde[k];
  }

  // reparametrization of s
  for (l in 1:L){
    s[l] = sigma_s * s_tilde[l];
  }

  // reparametrization of r
  for (j in 1:J){
    r[j] = sigma_r * r_tilde[j];
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
    beta ~ normal(88, 5); // should be around 90 for hr, xx for ibi and zero for centered data!
    sigma_w ~ cauchy(0, 5);
    sigma_s ~ cauchy(0, 10);
    sigma_r ~ cauchy(0, 5);

    sigma_e ~ cauchy(15, 5);
    sigma_sd_w ~ cauchy(0, 5);
    sigma_sd_sbj ~ cauchy(0, 5);

    // Reparameterization
    // an attempt to make sampling more efficient:
    // see Stan manual or https://arxiv.org/abs/0708.3797
    w_tilde ~ normal(0, 1); // implies w ~ normal(0, sigma_w)
    s_tilde ~ normal(0, 1); // implies s ~ normal(0, sigma_s)
    r_tilde ~ normal(0, 1); // implies r ~ normal(0, sigma_r)

    sd_w_tilde ~ normal(0, 1); // implies sd_w ~ normal(0, sigma_sd_w)
    sd_sbj_tilde ~ normal(0, 1); // implies sd_sbj ~ normal(0, sigma_sd_sbj)

    //w_tilde ~ gamma(2, 0.5); // implies w ~
    //s_tilde ~ gamma(2, 0.5); // implies s ~
    //r_tilde ~ gamma(2, 0.5); // implies r ~

    //sd_w_tilde ~ gamma(2, 0.5); // implies sd_w ~
    //sd_sbj_tilde ~ gamma(2, 0.5); // implies sd_sbj ~

    // sampling distribution
    for (i in 1:N){
        ibi[i] ~ normal(beta + w[task[i]] + s[subj[i]] + r[role[i]],
                        sigma_e + sd_w[task[i]] + sd_sbj[subj[i]]);
    }
}


// For use with loo -package
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(ibi[i] |
      beta + w[task[i]] + s[subj[i]] + r[role[i]],
      sigma_e + sd_w[task[i]] + sd_sbj[subj[i]]);
  }
}

