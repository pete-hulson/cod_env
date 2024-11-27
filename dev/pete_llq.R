# to look at longline catchability stuff



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


# code example to investigate link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('base_mdl'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('base_mdl', datafilename))

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


# get sabbie index
sabs <- vroom::vroom(here::here('data', 'sab_indx.csv'))

ss3indx <- rbind(ss3indx,
                 ss3_envlnk(sabs %>% 
                              tidytable::filter(year >= 2015) %>% 
                              tidytable::rename(value = sab_indx),
                            var = 2))

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

# get results
base <- r4ss::SS_output(dir = here::here('rsch', 'llq'))

# next, read in ctl file to be able to change the parameters that you link to growth
# define ctl file name
ctlfilename <- list.files(here::here('rsch', 'llq'), pattern = '.ctl')
# get datafile input
ctlfile <- r4ss::SS_readctl_3.30(here::here('rsch', 'llq', ctlfilename))

# replace env link with sabbie index
ctlfile$Q_parms$`env_var&link`[which(rownames(ctlfile$Q_parms) == 'LnQ_base_LLSrv(5)')] <- 102

# and, write the new ctl file
r4ss::SS_writectl_3.30(ctllist = ctlfile,
                       outfile = here::here('rsch', 'llq', ctlfilename),
                       verbose = TRUE,
                       overwrite = TRUE)

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq',
              exename)


# get results
new <- r4ss::SS_output(dir = here::here('rsch', 'llq'))



summ_out <- r4ss::SSsummarize(list(base, new))

summ_out$indices


names(summ_out)
names(new)

dir.create(here::here('rsch', 'llq', 'plots'))
r4ss::SSplotComparisons(summ_out,
                        print = TRUE,
                        plotdir = here::here('rsch', 'llq', 'plots'))

r4ss::SSplotComparisons(summ_out,
                        subplots = 13,
                        indexfleets = 5)

summ_out$likelihoods
summ_out$likelihoods_by_fleet



















