library(lme4)
library(car)
library(dplyr)
library(emmeans)
library(parallel)

setwd("~/Documents/Bioinformatics/Projects/Lahaina/lahaina_m_and_m/")


# read in otu table
otu_raw = read.csv("data/microbe/lahaina_otu_table.csv")

##################
# log transform #
#################


# get the data ready

# copy raw data.frame
otu_df = otu_raw

# set rownames
row.names(otu_df) = otu_df$OTU

# drop 'OTU' id column
otu_df = otu_df[ , colnames(otu_df) != "OTU"]

# now, all of the columns are numeric/integer!

# make it a bit smaller for the exampl
#otu_df = otu_df[1:20 , ]

########################
# option 1: data.frame #
########################

# ad 1 to each row and then log transform
dat= apply(otu_df, 2, function(a_row) log(a_row + 1 ))

# transpose
t_dat = as.data.frame(t(dat))

# get vector of otu names
otu_names = colnames(t_dat)


# add fake columns for classes
t_dat$sample_type = sample(c("control", "treatment", "blank"), size = 137, replace = T)
t_dat$time = sample(1:10, size = 137, replace = T)

# make an empty list to store model outputs
mod_out_list = list()

# for testing
# an_otu = otu_names[1]

# function to run models
run_lmer_anova_emmeans = function(an_otu) {
  
  # make a formula with the otu name and the model variables
  mod_formula = as.formula(paste0(an_otu, " ~ sample_type + (1|time)"))
  
  # run the model
  mod = lmer(mod_formula, data = t_dat)
  
  # run type II anova using car package
  mod_anova = Anova(mod)
  
  # output dataframe
  mod_df = as.data.frame(mod_anova)
  mod_df$otu = an_otu
  
  # fix names
  mod_df = rename(mod_df,  mod_pval = `Pr(>Chisq)`)
  
  
  ###############
  # RUN POSTHOC #
  ###############
  # run post hoc test on time period
  
  emm_out = emmeans(mod, "sample_type")
  post_hoc = as.data.frame(contrast(emm_out, "pairwise"))
  
  t_post_hoc = as.data.frame(t(post_hoc[, "p.value"]))
  colnames(t_post_hoc) = post_hoc$contrast
  
  # clean up column names
  colnames(t_post_hoc) = gsub("(.+) - (.+)", "\\1vs\\2_pval",
                              colnames(t_post_hoc))
  t_post_hoc$otu = an_otu
  
  ########
  # OUT #
  #######
  # spit out:
  # global model significance (p val, r^2,)
  # post hoc test (difference between time periods)
  
  compiled_anova_emmeans = merge(mod_df, t_post_hoc, by = "otu")
  
  return(compiled_anova_emmeans)
  
}


# version 1: run model one at a time
mods = lapply(otu_names, run_lmer_anova_emmeans)

# version 2: run models in parallel (8 cores) with error handling

nCores <- 8
cl <- makeCluster(nCores) 

mods = mclapply(otu_names, function(x) tryCatch(run_lmer_anova_emmeans(x),
                                              error = function(e) NULL))


# compile outputs into a data.frame
mod_out_df = do.call("rbind", mods)

# correct pvalues
mod_out_df$corrected_model_p = p.adjust(mod_out_df$mod_pval, method = "BH")



