# example script to run environmental links for goa pcod ----


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
exename <- ss3_exename(dir = here::here('base_mdl'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'base_mdl',
              exename)



# code example to investigate link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq', datafilename))

# and then, change the env data
# here i add in code that uses the cfsr data, you'd change this to reflect whatever data you'll use

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
                       here::here(here::here('rsch', 'llq'), datafilename), 
                       overwrite = TRUE)
  
# almost to end, get your operating system's exe name to be able to run model
exename <- ss3_exename(dir = here::here('rsch', 'llq'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq',
              exename)

  
  
# code example to investigate link with growth ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'grwth'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'grwth'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'grwth', datafilename))

# here, i'm using cfsr as with llq, but, this is an example of having more than 1 environmental index
# so, i'm just renaming the above one as another, but you'd possibly supply something different here
env_indx2 <- env_indx
# format for ss3 data file, and note that i've changed in the var argument to '2', to denote it's a different index
ss3indx2 <- ss3_envlnk(env_indx2,
                       var = 2)

# replace the env data in the ss3 dat file with this new data
datafile$envdat <- rbind(datafile$envdat,
                         ss3indx2)

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'grwth'), datafilename), 
                       overwrite = TRUE)


# next, read in ctl file to be able to change the parameters that you link to growth
# define ctl file name
ctlfilename <- list.files(here::here('rsch', 'grwth'), pattern = '.ctl')
# get datafile input
ctlfile <- r4ss::SS_readctl_3.30(here::here('rsch', 'grwth', ctlfilename))

# let's play with Lmin, there are 2 important steps, and here's an example to do that
# and, check out https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#tvOrder 
# for a description on how to define ss3 parameters and what links are available

# step 1: define the environmental link type in the MG_params matrix, 100s digit is type of link, 1s digit is which env variable in data file
ctlfile$MG_parms$`env_var&link`[which(rownames(ctlfile$MG_parms) == 'L_at_Amin_Fem_GP_1')] <- 102

# step 2, define the specifics for the link parameter
# the vector being defined is: lower bound, upper bound, initial value, prior, prio sd, prior type, and parameter phase
# if adding more than 1 parameter, need to keep this in order of parameter
ctlfile$MG_parms_tv <- rbind(ctlfile$MG_parms_tv,
                             L_at_Amin_Fem_GP_1_ENV_add = c(-10, 10, 0, 0, 0, 0, 6))

# and, write the new ctl file
r4ss::SS_writectl_3.30(ctllist = ctlfile,
                       outfile = here::here('rsch', 'grwth', ctlfilename),
                       verbose = TRUE,
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
exename <- ss3_exename(dir = here::here('rsch', 'grwth'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/grwth',
              exename)

  
  
# code example to read and evaluate results, using growth run as example ----

# get model output for base model and alt model (growth model)
output <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                       here::here('rsch', 'grwth')))

# run function to summarize output into a list, a big list...
summ_out <- r4ss::SSsummarize(output)

# example to look at likelihoods
summ_out$likelihoods

# example to look at parameter estimates
summ_out$pars

 # example function to plot output
r4ss::SSplotComparisons(summ_out)
