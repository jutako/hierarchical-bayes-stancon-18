# A script to generate data that follows the model
require(tidyverse)

REPO_DIR <- getwd()
source(file.path(REPO_DIR, "R", "bayes_tools_SC18.R"))


ENV <- get_environment(REPO_DIR)
INPUT_DATA_DIR <- ENV$dir_input_data
dir.create(INPUT_DATA_DIR, recursive = T, showWarnings = F)

set.seed(42)

## Parameters
n_subj <- 4
n_task <- 3
n_role <- 2
N <- 200
# 60709 / (22*13) = 212 is the average number of observations
# per subject-task -pair in the actual training data

mu_0 <- 88
theta_task <- 5
mu_task <- rnorm(n_task, mean = 0, sd = theta_task)
theta_subj <- 5
mu_subj <- rnorm(n_subj, mean = 0, sd = theta_subj)
theta_role <- 0.1
mu_role <- rnorm(n_role, mean = 0, sd = theta_role)

sigma_0 = 8
omega_task <- 5
sigma_task <- abs(rnorm(n_task, mean = 0, sd = omega_task))
omega_subj <- 5
sigma_subj <- abs(rnorm(n_subj, mean = 0, sd = omega_subj))

sbj_role_assignment <- tibble(
  sbj_idx = 1:n_subj,
  role_idx = rep(1:n_role, length.out = n_subj)
)


## Create a table of subjects, tasks and roles
params <- tibble(
  sbj_idx = as.integer(gl(n_subj, n_task)),
  task_idx = rep(seq(1, n_task), n_subj)
)

params <- left_join(params, sbj_role_assignment, by = 'sbj_idx')


## Assing model parameters for all rows of params
params <- params %>%
  mutate( mu_task = mu_task[task_idx],
          sigma_task = sigma_task[task_idx],
          mu_subj = mu_subj[sbj_idx],
          sigma_subj = sigma_subj[sbj_idx],
          mu_role = mu_role[role_idx],
          mu = mu_0 + mu_task + mu_subj + mu_role,
          sigma = sigma_0 + sigma_task + sigma_subj)

params_file <- file.path(INPUT_DATA_DIR, 'ibi-syndata_parameters.rds')
saveRDS(params, file = params_file)



data <- params %>%
  group_by(sbj_idx, task_idx, role_idx) %>%
  do(hr = rnorm(N, mean = .$mu, sd = .$sigma) +
          rnorm(N, mean = 0, sd = 1)) %>%
  ungroup() %>%
  unnest() %>%
  mutate(subject = ordered(sbj_idx),
         sbjnr = sbj_idx,
         task = ordered(task_idx),
         role = ordered(role_idx))
# todo: use only sbj_idx and subject -> get rid of sbjnr


syndata_file <- file.path(INPUT_DATA_DIR, 'ibi-syndata_data.rds')
saveRDS(data, file = syndata_file)

