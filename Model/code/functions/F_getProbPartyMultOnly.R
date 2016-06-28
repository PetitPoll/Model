getProbPartyMultOnly <- function(mcmc.array, #mcmc output rda, dimensions = iters x chains x npars
                         party, #LNP = 1, labor = 2, greens = 3
                         sex, # 1 = Female, 2 = Male
                         educ,
                         age.group, # 1 - 4,
                         #last, #party last voted for # greens labor lnp other
                         week.number
){
  ## calculates probability of voting lnp for a particular subpopulation
  ## returns vector of posterior draws of probability. 
  
  beta0 <- mcmc.array[,,paste0("beta0.tk[", week.number, ",", party,"]")]
  b.sex <- mcmc.array[,,paste0("b.sex.tk[",sex, ",", week.number, ",", party,"]")]
  b.age <- mcmc.array[,,paste0("b.age.tk[",age.group, ",", week.number, ",", party,"]")]
  b.educ <- mcmc.array[,,paste0("b.educ.tk[",educ, ",", week.number, ",", party,"]")]
  #b.last <- mcmc.array[,,paste0("b.party.tk[",last, ",", week.number, ",", party,"]")]
  
  #logit.p.mp <- alpha0 + a.sex + a.age + a.educ #+ a.last
  # probability of voting for lnp given voted for major party (logit)
  log.p.y.k <- beta0 + b.sex + b.age  + b.educ #+ b.last
  
  parties <- 1:4
  other.parties <- !(parties %in% party)
  
  other.party.res <- c()
  
  for (i in parties[other.parties]){
    beta0 <- mcmc.array[,,paste0("beta0.tk[", week.number, ",", i,"]")]
    b.sex <- mcmc.array[,,paste0("b.sex.tk[",sex, ",", week.number, ",", i,"]")]
    b.age <- mcmc.array[,,paste0("b.age.tk[",age.group, ",", week.number, ",", i,"]")]
    b.educ <- mcmc.array[,,paste0("b.educ.tk[",educ, ",", week.number, ",", i,"]")]
    #b.last <- mcmc.array[,,paste0("b.party.tk[",educ, ",", week.number, ",", i,"]")]
    
    other.party.res<- abind(other.party.res, exp(beta0 + b.sex + b.age  + b.educ), along = 3)
  }
  
  all.party.res <- abind(other.party.res, exp(log.p.y.k), along = 3)
  
  denom <- apply(all.party.res, 1:2, sum)
  
  p.y.party <- exp(log.p.y.k)/denom
  
  # probability of voting lnp 
  p.y <- p.y.party
  return(as.vector(p.y))
}