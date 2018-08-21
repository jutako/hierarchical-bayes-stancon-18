require(tidyverse)
require(ggplot2)
require(cowplot)
require(rstan)

REPO_DIR <- getwd()
source(file.path(REPO_DIR, "R", "bayes_tools_SC18.R"))


ENV <- get_environment(REPO_DIR)
INPUT_DATA_DIR <- ENV$dir_input_data
STAN_RESULTS_BASEDIR <- ENV$dir_stan_resbase #parent dir for results

COLORS = c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#7f7f7f")

PUBPLOT_DIR <- file.path(STAN_RESULTS_BASEDIR,'pubplot')
dir.create(PUBPLOT_DIR, showWarnings = F, recursive = T)

file_lst <- c(
  'ibistan_task_sbj_role.rds',
  'ibistan_task_sbj.rds',
  'ibistan_task.rds',
  'ibistan-fs_task_sbj_role.rds',
  'ibistan-fs_task_sbj.rds',
  'ibistan-fs_task.rds'
)
dir_lst <- list.dirs(STAN_RESULTS_BASEDIR, recursive = F)[[1]]


## Make tibble of available fit files
infile_ds <- tibble(fname = rep(file_lst, length(dir_lst)),
                    fdir = gl(length(dir_lst), length(file_lst), labels = dir_lst))
infile_ds <- infile_ds %>%
  mutate(file = file.path(fdir, fname),
         exists = file.exists(file),
         branch = str_split(fname, '_', n = 2, simplify = T)[,1]) %>%
  separate(fname, c('tmp1','tmp2'), sep = '_', remove = F, extra = 'merge') %>%
  separate(tmp2, c('model','tmp3'), sep = '\\.', remove = F) %>%
  select(-starts_with('tmp')) %>%
  mutate(model = paste0(model,
                        str_replace_all(branch, "ibistan", "")),
         model = ordered(model, levels =
                c("task", "task_sbj", "task_sbj_role",
                  "task-fs", "task_sbj-fs", "task_sbj_role-fs")) ) %>%
  filter(exists) #exclude rds files that were not found
infile_ds



# A function to extract posterior HDI from stan fit
get_summary_qi <- function(stanfit_file){
  #stanfit_file <- infile_ds$file[1]

  ibid_file <- file.path(INPUT_DATA_DIR, 'ibi-syndata_data.rds')
  ibid <- readRDS(ibid_file)
  param_names <- get_parameter_names(ibid)

  fit <- readRDS(stanfit_file)
  ds <- as.data.frame(rstan::summary(fit)$summary[,c('mean','2.5%','97.5%')])
  ds$stan_name <- rownames(ds)
  ds <- as.tibble(ds) %>%
    rename("qi.low" = "2.5%", "qi.high" = "97.5%")

  ds <- left_join(ds, param_names, by = 'stan_name')
  ds
}

# Extract posterior intervals for all files
tmpd <- infile_ds %>%
  group_by(file) %>%
  mutate(pdata = map(file, get_summary_qi)) %>%
  ungroup() %>%
  unnest()
tmpd


# A function to generate standard comparison plot
my_pointrange <- function(pd){

  p <-  ggplot(data = pd)+
    geom_pointrange(aes(x = label, y = mean, ymin = qi.low, ymax = qi.high,
                        colour = model),
                    position = position_dodge(width = 0.8),
                    size = 0.2) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    #scale_color_brewer(type = 'qual', palette = 'Set2') +
    scale_color_manual(values = COLORS) +
    theme_bw(base_family = 'sans') +
    theme(axis.text.x  = element_text(angle=-45, vjust = 1, hjust = 0))
  p

}

# A helper for saving
my_saveplot <- function(p, savename,
                        savedir = PUBPLOT_DIR,
                        width = 180, height = 100){
  cowplot::ggsave(filename = file.path(savedir, savename),
                  plot = p, units = 'mm',
                  width = width, height = height)
}


# Individual plots
task_fac <- get_task_order('mean')
pd <- tmpd %>%
  filter(group == 'task_mean') %>%
  mutate(label = ordered(label,
                         levels = task_fac$levels,
                         labels = task_fac$labels))
p <-  my_pointrange(pd) +
  labs(x = 'Task', y = 'Posterior value (bpm)',
       title = 'Size of task effect on mean')
my_saveplot(p, 'posterior_task-mean.pdf')


pd <- tmpd %>%
  filter(group == 'subj_mean')
p <-  my_pointrange(pd) +
  labs(x = 'Subject', y = 'Posterior value (bpm)',
       title = 'Size of subject effect on mean')
my_saveplot(p, 'posterior_subj-mean.pdf')


pd <- tmpd %>%
  filter(group == 'role_mean')
p <-  my_pointrange(pd) +
  labs(x = 'Role', y = 'Posterior value (bpm)',
       title = 'Size of role effect on mean')
my_saveplot(p, 'posterior_role-mean.pdf')


task_fac <- get_task_order('sd')
pd <- tmpd %>%
  filter(group == 'task_sd') %>%
  mutate(label = ordered(label,
                         levels = task_fac$levels,
                         labels = task_fac$labels))
p <-  my_pointrange(pd) +
  labs(x = 'Task', y = 'Posterior value (bpm)',
       title = 'Size of task effect on sd')
my_saveplot(p, 'posterior_task-sd.pdf')


pd <- tmpd %>%
  filter(group == 'subj_sd')
p <-  my_pointrange(pd) +
  labs(x = 'Subject', y = 'Posterior value (bpm)',
       title = 'Size of subject effect on sd')
my_saveplot(p, 'posterior_subj-sd.pdf')


## Baselines
pd <- tmpd %>%
  filter(group == 'baselines')
p <-  my_pointrange(pd) +
  #facet_grid(label ~ ., scales = 'free_y') +
  facet_wrap(~ label, nrow = 1, scales = 'free_y') +
  labs(x = 'Baseline', y = 'Posterior value (bpm)',
       title = 'Size of baseline effect')
p
my_saveplot(p, 'posterior_baselines.pdf', width = 150, height = 150)


pd <- tmpd %>%
  filter(group == 'baselines', label == 'mean_baseline')
p1 <-  my_pointrange(pd) +
  theme(axis.text.x=element_blank(),
        legend.position="none") +
  labs(x = 'Mean effect baseline', y = 'Posterior value (bpm)')
p1

pd <- tmpd %>%
  filter(group == 'baselines', label == 'sd_baseline')
p2 <-  my_pointrange(pd) +
  theme( axis.text.x=element_blank()) +
  labs(x = 'SD effect baseline', y = 'Posterior value (bpm)')
p2

p <- cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(1,1.5))
p
my_saveplot(p, 'posterior_baselines_2.pdf')


# ======================================================================
## Computation times

# A function to extract computation time
get_summary_qi <- function(stanfit_file){
  fit <- readRDS(stanfit_file)
  ds <- tibble(ctime = as.numeric(attr(fit, 'computation_time'),
                                  units = "hours"))
  ds
}

# Extract posterior intervals for all files
ctd <- infile_ds %>%
  group_by(file) %>%
  mutate(pdata = map(file, get_summary_qi)) %>%
  ungroup() %>%
  unnest()
ctd

p <- ggplot(data = ctd) +
  geom_point(aes(x = model, y = ctime, color = model), size = 2) +
  #geom_hline(aes(yintercept = 2), linetype = 2) +
  scale_y_log10(breaks = c(1:10, 12, 24, 36, 48)) +
  scale_color_manual(values = COLORS) +
  theme_bw(base_family = 'sans') +
  theme(legend.position="none") +
  labs(x = 'Model', y = 'Computation time (h)')
p
my_saveplot(p, 'computation_times.pdf', width = 150, height = 100)


