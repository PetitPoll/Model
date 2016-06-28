getProbLNP <- function(mcmc.array, #mcmc output rda, dimensions = iters x chains x npars
                       sex, # 1 = Female, 2 = Male
                       educ,
                       age.group, # 1 - 4,
                       week.number
){
  ## calculates probability of voting lnp for a particular subpopulation
  ## returns vector of posterior draws of probability. 
  
  alpha0 <- mcmc.array[,,paste0("alpha0.t[", week.number, "]")]
  beta0 <- mcmc.array[,,paste0("beta0.t[", week.number, "]")]
  a.sex <- mcmc.array[,,paste0("a.sex.t[",sex, ",", week.number, "]")]
  a.age <- mcmc.array[,,paste0("a.age.t[",age.group, ",", week.number, "]")]
  a.educ <- mcmc.array[,,paste0("a.educ.t[",educ, ",", week.number, "]")]
  b.sex <- mcmc.array[,,paste0("b.sex.t[",sex, ",", week.number, "]")]
  b.age <- mcmc.array[,,paste0("b.age.t[",age.group, ",", week.number, "]")]
  b.educ <- mcmc.array[,,paste0("b.educ.t[",educ, ",", week.number, "]")]
  
  logit.p.mp <- alpha0 + a.sex + a.age + a.educ #+ a.last
  # probability of voting for lnp given voted for major party (logit)
  logit.p.y <- beta0 + b.sex + b.age  + b.educ #+ b.last
  # probability of voting lnp 
  p.y <- invlogit(logit.p.mp)*invlogit(logit.p.y)
  return(as.vector(p.y))
}