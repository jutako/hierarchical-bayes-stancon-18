#
# Note: run source('prepare_ibi_data_SC18.R') once prior to
# running this script.
# source('analysis/noldus_ibi/ibi_stancon2018/prepare_ibi_data_SC18.R')
require(rstan)
require(stringr)

REPO_DIR <- getwd()
source(file.path(REPO_DIR, "R", "bayes_tools_SC18.R"))

# ----------------------------------------------------------------
# Initialize user settable parameters

PARAMSET <- 'testing' # 'testing'
MODELSET <- c('ibistan_task_sbj.stan')
# MODELSET <- c('ibistan_task.stan','ibistan_task_sbj.stan',
#              'ibistan_task_sbj_role.stan')
# MODELSET <- c('ibistan-fs_task.stan','ibistan-fs_task_sbj.stan',
#               'ibistan-fs_task_sbj_role.stan')


ENV <- get_environment(REPO_DIR)
INPUT_DATA_DIR <- ENV$dir_input_data
STAN_MODEL_DIR <- ENV$dir_stan_model #*.stan files
STAN_RESULTS_BASEDIR <- ENV$dir_stan_resbase #parent dir for results

# result directory of current run
EXPORT_DIR <- file.path(STAN_RESULTS_BASEDIR,
                        sprintf('%s_%s',
                                PARAMS$target_variable,
                                strftime(Sys.time(),'%Y%m%dT%H%M%S')) )
dir.create(EXPORT_DIR, recursive = T, showWarnings = F)

PARAMS <- get_parameters(PARAMSET)
PARAMS$stan_basedir <- STAN_RESULTS_BASEDIR
PARAMS$export_dir <- EXPORT_DIR


# Set some recommended options
rstan_options(auto_write = F) # cache compiled models to save time
options(mc.cores = PARAMS$n_cores) # parallelize chains, leave one free for other stuff


# ----------------------------------------------------------------
# Load input data
ibid <- switch(PARAMS$dataset,
                synthetic = readRDS(file.path(INPUT_DATA_DIR,
                                     'ibi-syndata_data.rds')) )


# ----------------------------------------------------------------
# Estimate Stan models

# A helper function to estimate the model
run_model <- function(ibid, stan_file, params){

  # ----------------------------------------------------------------
  ## RStan helper file locations
  indata_filename <- str_replace(basename(stan_file), '\\.stan', '_input-data.R') #name of result file
  indata_file <- file.path(params$export_dir, indata_filename)

  fit_filename <- str_replace(basename(stan_file), '\\.stan', '.rds') #name of result file
  fit_file <- file.path(params$export_dir, fit_filename)

  # ----------------------------------------------------------------
  ## Define input data
  ibi = ibid[[params$target_variable]]
  N = nrow(ibid)

  L = length(unique(ibid$sbj_idx))
  subj = ibid$sbj_idx

  K = length(unique(ibid$task_idx))
  task = ibid$task_idx

  if (PARAMS$dataset == 'synthetic'){
    stan_rdump(c('N','ibi','L','subj','K','task'),
               file = indata_file)
  } else {
    J = length(unique(ibid$role_idx))
    role = ibid$role_idx

    stan_rdump(c('N','ibi','L','subj','J','role','K','task'),
               file = indata_file)
  }

  ibi_data <- read_rdump(indata_file)


  # ----------------------------------------------------------------
  ## Run rstan
  start_time <- Sys.time()
  fit <- rstan::stan(stan_file,
                     data = ibi_data,
                     iter = params$iterations,
                     seed = params$seed,  #to gain reproducibility
                     chains = params$chains,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 15))
  end_time <- Sys.time()
  end_time - start_time

  attr(fit, 'user_params') <- params
  attr(fit, 'computation_time') <- end_time - start_time
  saveRDS(fit, fit_file)
}



# Loop over different models one by one, sequentially
for (cfile in MODELSET){
  model_file <- file.path(STAN_MODEL_DIR, cfile)
  run_model(ibid, model_file, PARAMS)
  rm(model_file)
}
