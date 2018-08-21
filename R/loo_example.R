# Demonstration of using LOO for model checking and comparison
#
# Note: loo uses log_lik stored during runtime to do the math.
# You need to modify your *.stan file accordingly. For example add to the end:
#
# generated quantities {
#   vector[N] log_lik;
#   for (i in 1:N) {
#     log_lik[i] = normal_lpdf(ibi[i] | beta + w[task[i]],
#                              sigma_e + sd_w[task[i]] + sd_sbj[subj[i]]);
#   }
# }
#
# Where the function generating log_lik should match your sampling distribution.
#
# See loo -package documentation at ?loo::loo() or
# http://mc-stan.org/loo/articles/loo2-with-rstan.html (a bit outdated) or
# https://github.com/avehtari/modelselection_tutorial and
# https://link.springer.com/content/pdf/10.1007%2Fs11222-016-9696-4.pdf
require(loo)

REPO_DIR <- getwd()
source(file.path(REPO_DIR, "R", "bayes_tools_SC18.R"))

ENV <- get_environment(REPO_DIR)
STAN_RESULTS_BASEDIR <- ENV$dir_stan_resbase #parent dir for results


# Note: this assumes that all of the models listed below have been fitted
# and the results are stored in the same directory "dir1"
dir1 <- list.dirs(STAN_RESULTS_BASEDIR, recursive = F)[[1]]
fit1 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan_task.rds'))
fit2 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan_task_sbj.rds'))
fit3 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan_task_sbj_role.rds'))
fit4 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan-fs_task.rds'))
fit5 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan-fs_task_sbj.rds'))
fit6 <- readRDS(file.path(STAN_RESULTS_BASEDIR, dir1, 'ibistan-fs_task_sbj_role.rds'))


# Extract pointwise log-likelihood and compute LOO
log_lik_1 <- loo::extract_log_lik(fit1)
log_lik_2 <- loo::extract_log_lik(fit2)
log_lik_3 <- loo::extract_log_lik(fit3)
log_lik_4 <- loo::extract_log_lik(fit4)
log_lik_5 <- loo::extract_log_lik(fit5)
log_lik_6 <- loo::extract_log_lik(fit6)


(loo_1 <- loo::loo(log_lik_1))
(loo_2 <- loo::loo(log_lik_2))
(loo_3 <- loo::loo(log_lik_3))
(loo_4 <- loo::loo(log_lik_4))
(loo_5 <- loo::loo(log_lik_5))
(loo_6 <- loo::loo(log_lik_6))


loo::compare(loo_1, loo_2)
loo::compare(loo_3, loo_2)

loo::compare(loo_4, loo_2)
loo::compare(loo_5, loo_2)
loo::compare(loo_6, loo_2)

loo::compare(loo_4, loo_5)
loo::compare(loo_6, loo_5)

# Positive values of elpd_diff favor model in the second argument.
# SE can be used to measure the size of the difference with respect
# estimate uncertainty.

# => model 2 is best, given that role in model 3 is does not show significant
# effect and hence simpler model should be favored


if (F){
# Info about LOO and the diagnostics that estimate the reliability
# of the LOO estimate
print(loo_1, digits = 3)

# Visual of the diagnostics
# If LOO cannot be trusted, real K-fold crossvalidation is needed.
plot(loo_1)

# Comparing two models
comp <- loo::compare(loo_1, loo_2)
print(comp)
}

