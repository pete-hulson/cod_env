# script to run temperature links for goa pcod ----
#by Krista, Nov 2024, borrowing heavily from Pete's script
#compare CFSR, HYCOM, GAK1

#BEFORE RUNNING
#CHECK YOUR r4ss VERSION
#this script written with r4ss 1.50.0 for SS3 release 3.30.23
#using r4ss 1.49 or older will result in errors in 'read and evaluate results' section

# load necessary packages ----
## cran packages ----
pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "tictoc")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)

## load functions ----
source_files <- list.files(here::here("R"), pattern = "*.R$")
purrr::map(here::here("R", source_files), source)


# code example to run a model ----
# this example is to run the base 24 assessment model upon cloning the repository

# first, downlad the ss3 exe file with r4ss function to the model folder
r4ss::get_ss3_exe(dir = here::here('base_mdl'))

# second, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('base_mdl'))
exename <- "ss3"

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'base_mdl',
              exename)



# CFSR link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_cfsr'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_cfsr'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_cfsr', datafilename))

# and then, change the env data

# get cfsr data
cfsr <- vroom::vroom(here::here('data', 'raw_cfsr.csv'))

# compute env index
cfsr %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(month == 6) %>% 
  tidytable::select(year, temp = '0_20') %>% 
  tidytable::mutate(avg = as.numeric(cfsr %>% 
                                       dplyr::rename_all(tolower) %>% 
                                       tidytable::filter(month == 6,
                                                         year >= 1982,
                                                         year <= 2012) %>% 
                                       tidytable::select(year, temp = '0_20') %>% 
                                       tidytable::summarise(avg_temp = mean(temp)))) %>% 
  tidytable::mutate(value = round(temp - avg, digits = 5)) %>% 
  tidytable::select(year, value) -> env_indx

# format for ss3 data file
ss3indx <- ss3_envlnk(env_indx,
                      var = 1)

# replace the env data in the ss3 dat file with this new data
datafile$envdat <- ss3indx

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'llq_cfsr'), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_cfsr'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_cfsr',
              exename)


# HYCOM link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_hycom')) #

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_hycom'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_hycom', datafilename))

# and then, change the env data

# get hycom data
temp_dat <- vroom::vroom(here::here('data', 'scaled_temp_by_month.csv'))
temp_dat <- temp_dat %>% rename(hycom='mean_monthly',
                                        gak='mean_gak_monthly_temp',
                                        cfsr='cfsr_temp')

jun_50_hycom <- temp_dat[which(temp_dat$Month==6&temp_dat$depth=="50m"),names(temp_dat) %in% 
                        c("Year","hycom")]
jun_50_hycom <- na.omit(jun_50_hycom)
# hycom is already monthly mean and z-scored
#here I am using 50m depth and June mean



env_indx_hycom <- jun_50_hycom

#rename to work with function
env_indx_hycom <- env_indx_hycom %>% rename(year='Year',
                                      value='hycom')

# format for ss3 data file
ss3indx_hycom <- ss3_envlnk(env_indx_hycom,
                      var = 1)

# replace the env data in the ss3 dat file with this new data
#stitch hycom (starts in 1995) to cfsr before 1994
datafile$envdat[which(datafile$year>1994)] <- ss3indx_hycom

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'llq_hycom'), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_hycom'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_hycom',
              exename)




# GAK link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_gak'))#

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_gak'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_gak', datafilename))

# and then, change the env data

jun_50_gak <- temp_dat[which(temp_dat$Month==6&temp_dat$depth=="50m"),names(temp_dat) %in% 
                           c("Year","gak")]
jun_50_gak <- na.omit(jun_50_gak)
# gak is already monthly mean and z-scored
#here I am using 50m depth and June mean



env_indx_gak <- jun_50_gak

#rename to work with function
env_indx_gak <- env_indx_gak %>% rename(year='Year',
                                            value='gak')

# format for ss3 data file
ss3indx_gak <- ss3_envlnk(env_indx_gak,
                            var = 1)

# replace the env data in the ss3 dat file with this new data
#stitch gak (starts CONSISTENTLY in 2004) to cfsr before 2004
datafile$envdat[which(datafile$year>2003)] <- ss3indx_gak[which(ss3indx_gak$year>2003),] #UPDATE YEAR HERE
#FOR GAK there are missing years, need to decide how to handle

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'llq_gak'), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_gak'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_gak',
              exename)








#read and evaluate results----

# get model output for base model and alt model
outputH <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                       here::here('rsch', 'llq_hycom')))

#test <- r4ss::SS_output(dir = here::here('base_mdl'),
printstats = FALSE)

#r4ss::SS_output(dir = here::here('rsch', 'llq'))

# run function to summarize output into a list, a big list...
summ_outH <- r4ss::SSsummarize(outputH)

# example to look at likelihoods
summ_outH$likelihoods

# example to look at parameter estimates
summ_outH$pars

# example function to plot output
r4ss::SSplotComparisons(summ_outH)