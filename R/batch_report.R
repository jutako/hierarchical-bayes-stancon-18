## A script to run summary report for a set of rds files containing
## fitted RStan models.
require(tidyverse)

REPO_DIR <- getwd()
source(file.path(REPO_DIR, "R", "bayes_tools_SC18.R"))


#----------------------------------------------------------------------
## Options
ENV <- get_environment(REPO_DIR)
INPUT_DATA_DIR <- ENV$dir_input_data
STAN_MODEL_DIR <- ENV$dir_stan_model #*.stan files
STAN_RESULTS_BASEDIR <- ENV$dir_stan_resbase #parent dir for results
REPORT_DIR <- ENV$dir_rcode
REPORT_FILE <- 'stanfit_report.Rmd'


# List of directories to analyze
dir_lst <- list.dirs(STAN_RESULTS_BASEDIR, recursive = F)

file_lst <- c(
  'ibistan-fs_task_sbj_role.rds',
  'ibistan-fs_task_sbj.rds',
  'ibistan-fs_task.rds',
  'ibistan_task_sbj_role.rds',
  'ibistan_task_sbj.rds',
  'ibistan_task.rds',
  'ibistan_task_fixedscale.rds'
)


#----------------------------------------------------------------------
## Helper functions
# Extract file body
filebody <- function(fname){
  str_split(basename(fname), '\\.', simplify = T)[[1]]
}


# Render report to html format
render_html <- function(report_file, outpath_report, inparams){

  out_file <- file.path(outpath_report,
                        sprintf('stanfitrep_%s.html',
                                filebody(basename(inparams$stan_fit_rds)) ))
  rmarkdown::render(report_file,
                    output_format = 'html_document',
                    params = inparams,
                    output_file = out_file)

}


#-------------------------------------------------------------------------------------------
## Stan fit reports

# A helper function to make rendering tibble compatible
render_wrapper <- function(input_data_dir, stan_fit_rds){

  input_data_rds <- file.path(INPUT_DATA_DIR, 'ibi-syndata_data.rds')

  inparams <- list(input_data_rds = input_data_rds,
                   stan_fit_rds = stan_fit_rds)
  render_html(file.path(REPORT_DIR, REPORT_FILE),
              dirname(stan_fit_rds),
              inparams)
  tibble(success = T)

}


# Define input files
infile_ds <- tibble(fname = rep(file_lst, length(dir_lst)),
                    fdir = gl(length(dir_lst), length(file_lst), labels = dir_lst))
infile_ds <- infile_ds %>%
  mutate(file = file.path(fdir, fname),
         exists = file.exists(file))
infile_ds


# Render reports for directory-file combinations that exist
infile_ds %>%
  filter(exists) %>%
  group_by(file) %>%
  do(render_wrapper(input_data_dir, .$file))

