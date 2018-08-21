
#' Set computing environment
#' Single point of setting directory structure
#'
#' @param repo_root Root directory of the cloned repo
#'
#' @return A list of directories to use in scripts
get_environment <- function(repo_root){

  env <- list(
    dir_rcode = file.path(REPO_DIR, 'R'),
    dir_stan_model = file.path(repo_root, 'stan'),
    dir_stan_resbase = file.path(repo_root, 'results'),
    dir_input_data = file.path(repo_root, 'data')
  )

  env
}


## Set parameters for testing or production purposes
get_parameters <- function(paramset){

  # dataset: testing, training, synthetic
  stancon2018 <- list(
    dataset = 'training',
    target_variable = 'hr',
    n_cores = 4,
    chains = 4,
    iterations = 1e3,
    seed = 42)

  testing <- list(
    dataset = 'synthetic',
    target_variable = 'hr',
    n_cores = 2,
    chains = 4,
    iterations = 1e2,
    seed = 42)

  params <- switch(paramset,
         stancon2018 = stancon2018,
         testing = testing)

  if(is.null(params)){
    cat('Unknown input value. Input \'paramset\' needs to be either \'stancon2018\' or \'testing\'.')
  }

  params
}


## Load ibi data and create index variables needed with Stan
# also centering included for now (todo: move somewhere else?)
load_prepare <- function(ibidfile,
                         task_levels = c('initiation','testing','sensorfault','faultresolved',
                                          'testingends', 'normalrun',
                                          'firealarm','fireconfirmed','shutdown','de-energized',
                                          'leakdetected','leaksolved','questionnaires'),
                         role_levels = c('APO','SS','RO','TO')){

  ibid_train <- readRDS(ibidfile) %>%
    filter(task != 'unkown') %>%
    mutate(subject = ordered(subject),
           sbj_idx = as.integer(subject),
           role = ordered(role,
                          levels = role_levels),
           role_idx = as.integer(role),
           crew_idx = as.integer(str_sub(crew, 2)),
           task = ordered(task,
                          levels = task_levels),
           task_idx = as.integer(task),
           roletask = ordered(paste0(role, '_', task)),
           roletask_idx = as.integer(roletask))


  # center data since training data is not exactly centered
  # todo: move this higher up in the pipe
  center <- function(ds){
    ds$ibi.c <- ds$ibi - mean(ds$ibi)
    ds$hr.c <- ds$hr - mean(ds$hr)
    ds
  }
  ibid_train <- ibid_train %>%
    group_by(subject) %>%
    do(center(.))

  ibid_train
}


## Make a balanced pilot subset of ibi data
subset_testing <- function(ibid_train){

  ## Subset for testing purposes
  assign_pilots <- function(ds){
    idx <- 1:nrow(ds)
    set.seed(42) #for predictable results
    idx <- sample(idx, floor(0.1*nrow(ds)), replace = F)
    ds$ispilot[idx] <- T
    ds
  }
  ibid_pilot <- ibid_train %>%
    mutate(ispilot = F) %>%
    group_by(subject, task) %>%
    do(assign_pilots(.)) %>%
    ungroup()

  ibid_pilot <- ibid_pilot %>% filter(ispilot)
  ibid_pilot
}


## Return a tibble with names and metadata for the Stan model parameters
get_parameter_names <- function(ibid){

  task_labels1 <- ibid %>%
    group_by(task, task_idx) %>%
    count() %>%
    ungroup() %>%
    arrange(task_idx) %>%
    mutate(stan_name = sprintf('w[%d]', task_idx),
           label = paste0(task,'_mean'),
           group = 'task_mean')

  subj_labels1 <- ibid %>%
    group_by(sbjnr, sbj_idx) %>%
    count() %>%
    ungroup() %>%
    arrange(sbj_idx) %>%
    mutate(stan_name = sprintf('s[%d]', sbj_idx),
           label = paste0(sbjnr,'_mean'),
           group = 'subj_mean')

  role_labels <- ibid %>%
    group_by(role, role_idx) %>%
    count() %>%
    ungroup() %>%
    arrange(role_idx) %>%
    mutate(stan_name = sprintf('r[%d]', role_idx),
           label = paste0(role,'_mean'),
           group = 'role_mean')


  task_labels2 <- ibid %>%
    group_by(task, task_idx) %>%
    count() %>%
    ungroup() %>%
    arrange(task_idx) %>%
    mutate(stan_name = sprintf('sd_w_tilde[%d]', task_idx),
           label = paste0(task,'_sd'),
           group = 'task_sd')

  subj_labels2 <- ibid %>%
    group_by(sbjnr, sbj_idx) %>%
    count() %>%
    ungroup() %>%
    arrange(sbj_idx) %>%
    mutate(stan_name = sprintf('sd_sbj_tilde[%d]', sbj_idx),
           label = paste0(sbjnr,'_sd'),
           group = 'subj_sd')

  misc_labels <- data.frame(
    stan_name = c('beta','sigma_e'),
    label = c('mean_baseline', 'sd_baseline'),
    group = 'baselines')


  labs <- rbind(as.data.frame(task_labels1 %>%
                                select(stan_name, label, group)),
                as.data.frame(task_labels2 %>%
                                select(stan_name, label, group)),
                as.data.frame(subj_labels1 %>%
                                select(stan_name, label, group)),
                as.data.frame(subj_labels2 %>%
                                select(stan_name, label, group)),
                as.data.frame(role_labels %>%
                                select(stan_name, label, group)),
                misc_labels)
  labs <- as.tibble(labs) %>%
    mutate(group = ordered(group,
                           levels = c("baselines",
                                      "task_mean","subj_mean","role_mean",
                                      "task_sd","subj_sd")))

  labs
}


# task factor levels and labels in correct order
get_task_order <- function(type){
  list(
    levels = sprintf(
      c("initiation_%s", "testing_%s", "sensorfault_%s",
              "faultresolved_%s",  "testingends_%s", "normalrun_%s",
              "firealarm_%s", "fireconfirmed_%s", "shutdown_%s",
              "de-energized_%s", "leakdetected_%s","leaksolved_%s",
              "questionnaires_%s"), type),
    labels = c("initiation", "testing", "sensorfault",
               "faultresolved",  "testingends", "normalrun",
               "firealarm", "fireconfirmed", "shutdown",
               "de-energized", "leakdetected","leaksolved",
               "questionnaires")
  )
}
