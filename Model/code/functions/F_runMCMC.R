runMCMC <- function(df, # data frame with all demo and intention data (cleaned)
                    intent.colnames, 
                    party.vote = "lnp",
					          multinomial = F,
					          informative.priors = F,
                    niters= 3000,
                    seed = 4114,
                    alpha.level = 0.05,
                    res.filename = paste0("res",Sys.Date(),".csv"),
                    save.results = T,
                    save.post.samples = F, 
                    save.meta = T){
  ### function that runs model to estimate parameters from multilevel model and then aggregates to give two-party probability
  set.seed(seed)
  
  # for the JAGS model, all data needs to be in matrix form with dim i x t
  no.weeks <- length(intent.colnames)
  mp.it <- c()
  y.jt <- c()
  for(week in 1:no.weeks){
  	mp.it <- cbind(mp.it, eval(parse(text = paste0("df$mp",week,".i"))))
  	y.jt <- cbind(y.jt, eval(parse(text = paste0("df$y",week,".i"))))
  }
  
  #list of inputs for the model
  jags.data <- list(mp.it = mp.it, n = nrow(df), y.jt = y.jt, m = nrow(df), T = no.weeks,
                  sex.i = matrix(rep(df$sex.i, no.weeks), ncol = no.weeks), 
                  age.i = matrix(rep(df$age, no.weeks), ncol = no.weeks), 
                  educ.i = matrix(rep(df$educ, no.weeks), ncol = no.weeks),
                  sex.j = matrix(rep(df$sex.i, no.weeks), ncol = no.weeks), 
                  age.j = matrix(rep(df$age, no.weeks), ncol = no.weeks), 
                  educ.j = matrix(rep(df$educ, no.weeks), ncol = no.weeks),
                  n.sex = 2, n.age = 4,  n.educ = 4)
   model.filename <- "Model/code/models/model_time.txt"     
   
   # parameters to save
   parnames <- c( "alpha0.t", "a.sex.t", "a.age.t", "a.educ.t",  
                  "beta0.t", "b.sex.t", "b.age.t", "b.educ.t")
  
  if(informative.priors==T){
  	# if using informative priors, need to add in info about coefficients
  	
  	coeff.priors <- read.csv("Model/coefficient_priors.csv")
  	
  	if(party.vote == "lnp") col.index <- 2
  	if(party.vote == "labor") col.index <- 3
  	if(party.vote == "greens") col.index <- 4
  	
  a.age <- coeff.priors[grep("a.age", coeff.priors$par), col.index]
	b.age <- coeff.priors[grep("b.age", coeff.priors$par), col.index]
	a.educ <- coeff.priors[grep("a.educ", coeff.priors$par), col.index]
	b.educ <- coeff.priors[grep("b.educ", coeff.priors$par), col.index]
	a.sex <- coeff.priors[grep("a.sex", coeff.priors$par), col.index]
	b.sex <- coeff.priors[grep("b.sex", coeff.priors$par), col.index]
	beta0 <- coeff.priors[grep("beta0", coeff.priors$par), col.index]
	alpha0 <- coeff.priors[grep("alpha0", coeff.priors$par), col.index]
  	
  	jags.data <- list(mp.it = mp.it, n = nrow(df), y.jt = y.jt, m = nrow(df), T = no.weeks,
                  sex.i = matrix(rep(df$sex.i, no.weeks), ncol = no.weeks), 
                  age.i = matrix(rep(df$age, no.weeks), ncol = no.weeks), 
                  educ.i = matrix(rep(df$educ, no.weeks), ncol = no.weeks),
                  sex.j = matrix(rep(df$sex.i, no.weeks), ncol = no.weeks), 
                  age.j = matrix(rep(df$age, no.weeks), ncol = no.weeks), 
                  educ.j = matrix(rep(df$educ, no.weeks), ncol = no.weeks),
                  alpha0 = alpha0, beta0= beta0,
                  a.age = a.age, a.sex = a.sex, a.educ = a.educ,
                  b.age = b.age, b.sex = b.sex, b.educ = b.educ,
                  n.sex = 2, n.age = 4,  n.educ = 4)
    
    model.filename <- "Model/code/models/model_infpriors.txt"              
  }
  
  if(multinomial==T){
    jags.data <- list(y.jt = y.jt, m = nrow(df), T = no.weeks,
                      sex.j = matrix(rep(df$sex.i, no.weeks), ncol = no.weeks), 
                      age.j = matrix(rep(df$age, no.weeks), ncol = no.weeks), 
                      educ.j = matrix(rep(df$educ, no.weeks), ncol = no.weeks),
                      n.sex = 2, n.age = 4,  n.educ = 4)
    model.filename <- "Model/code/models/model_multinomial.txt"
    
    # parameters to save
    parnames <- c( "beta0.tk", "b.sex.tk", "b.age.tk", "b.educ.tk")
  } 
   
  # run the mcmc    
  cat("Running model! \n")
  mod <- jags(data = jags.data, 
                parameters.to.save=parnames, n.iter = niters,
                model.file = model.filename)
  cat("Model finished. \n")
  mcmc.array <- mod$BUGSoutput$sims.array
  if(save.post.samples ==T) save(mcmc.array, file=paste0("Model/output/posterior samples/post.samples.Rda"))
    
  # calculate probability of voting for party of interest for each subgroup, then weight and add
  cat("Calculating results. \n")
  if(multinomial==F){
    res <- c()
    for (week in 1:no.weeks){
      p.national <- c()
      for(s in 1:2){
        for(a in 1:4){
          for(e in 1:4){
            sex <- s
            age.group <- a
            educ <- e
            temp <- getProbLNP(mcmc.array = mcmc.array, sex = sex, educ = educ, week.number = week,
                               age.group = age.group)*wts$prop[wts$age.group==age.group&wts$sex==sex&wts$educ.group==educ]
            p.national <- cbind(p.national, temp)
          } # end educ loop
        } # end age loop
      } # end sex loop
      res <- rbind(res, c(median(rowSums(p.national)),quantile(rowSums(p.national), 1-alpha.level/2), quantile(rowSums(p.national),alpha.level/2)))
    } # end week loop
    colnames(res) <- c("Median", "Upper", "Lower" )
  }
  
  if(multinomial==T){
    res.list <- list()
    for(p in 1:4){
      party <- p
      res <- c()
      alpha.level = 0.2
      for (week in 1:no.weeks){
        p.national <- c()
        for(s in 1:2){
          for(a in 1:4){
            for(e in 1:4){
              sex <- s
              age.group <- a
              educ <- e
              #last <- p
              temp <- getProbPartyMultOnly(mcmc.array = mcmc.array, sex = sex, educ = educ, week.number = week, party = party,
                                           age.group = age.group)*wts$prop[wts$age.group==age.group&
                                                                             wts$sex==sex&
                                                                             wts$educ.group==educ]
              p.national <- cbind(p.national, temp)
            }
            
          }
        }
        res <- rbind(res, c(median(rowSums(p.national)),quantile(rowSums(p.national), 1-alpha.level/2), quantile(rowSums(p.national),alpha.level/2)))
      }
      res.list[[p]] <- res
    }
    names(res.list) <- c("lnp", "labor", "greens", "other")
    res <- do.call(rbind, lapply(res.list, data.frame, stringsAsFactors=FALSE))
    res$party <- c(rep("lnp", no.weeks), rep("labor", no.weeks), rep("greens", no.weeks), rep("other", no.weeks))
    res$week <- rep(1:7, no.weeks)
    rownames(res) <- NULL
    colnames(res)[1:3] <- c("Median", "Upper", "Lower")
  }
  cat("Saving results. \n")
  if(save.results==T) write.csv(res, file = paste0("Model/output/", res.filename), row.names = F)
  print(res)
  if(save.meta == T){
    meta.info = list(seed=seed, alpha = alpha.level, niter = niters, date = Sys.time())
    save(meta.info, file = "Model/output/meta/meta.info.rda")
  }
}