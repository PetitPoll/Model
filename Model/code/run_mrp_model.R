### Run MRP model on Australian voting intention data
### Petit Poll, 2016


# set wd and load all require functions and packages
setwd("")
source("Model/code/setup.R")


# get pop weights ---------------------------------------------------------

# weights by age sex and education
wts <- read.csv("")

# load data ---------------------------------------------------------------

load("") #an .Rda file of the data

#specify names of all columns with intention data
intent.colnames <- c()

# get data into format for model
df <- cleanDemoVars(Data)
# need to specify whether you are imputing data, and if so, which assumptions you are using
# need to specify whether you are using multinomial model
# if using binomial, need to specify which party you are interested in
intents <- cleanIntentVars(Data, intent.colnames, 
						   impute = T, impute.from.joined = F, impute.partial = T, 
						   greens.is.mp = T, multinomial = T, party.vote = "labor")
df <- cbind(df, intents)

# Run model ---------------------------------------------------------------

# need to specify whether using multinomial or binomial model
# need to specify whether using informative priors
# if using binomial, need to specify which party you are interested in

runMCMC(df, intent.colnames, multinomial = F, informative.priors = T, save.results = T, alpha.level = 0.2, party.vote = "labor")




