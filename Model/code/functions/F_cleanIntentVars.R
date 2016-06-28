## Function to clean the voter intentions to get into a form to input into the model

cleanIntentVars <- function(Data, # raw data set
                            intent.colnames, # names of colnames where intention is listed
                            greens.is.mp = T, #treat greens as a major party?
                            impute = F, #whether or not to impute intention based on  last vote and party affiliation
                            impute.from.joined=F, #if imputing, whether or not to impute on from when they joined onwards
                            impute.partial = F, # if imputing, impute responses only for people who have at least responded once
                            multinomial = F, # whether or not using multinomial model
                            party.vote = "lnp" # for binomial model, which party is of interest? must be string (lnp, labor or greens)
                            ){
  temp <- data.frame(no = 1:nrow(Data))
  intent.col.numbers <- which(colnames(Data)%in%intent.colnames)
  
  # if imputing intention based on last vote and party affiliation
  if(impute==T){
    df<- cleanDemoVars(Data)
    df$constant <- 0
    for (j in 1:nrow(df)){
      pt <- df$party[j]
      if(!is.na(df$last[j])){
        if(as.character(df$last[j])==as.character(pt)){
          df$constant[j]<- 1
        } 
      }
    }
    
    #work out when they signed up
    if(impute.from.joined){
      df$joined <- NA
      for (i in 1:length(intent.col.numbers)){
        temp.cols <- intent.col.numbers[1:i]
        for (j in 1:nrow(df)){
          if(is.na(df$joined)[j]){
            if(i==length(intent.col.numbers)) df$joined[j] <- length(intent.col.numbers)
            if(sum(is.na(Data[j,temp.cols]))<length(temp.cols)) df$joined[j] <- i
          }
        }
      }
    } # end if (impute.from.joined)
    
    if(impute.partial){
      df$non.responses <- rowSums(is.na(Data[, intent.colnames]))
    }
    
  } # end if (impute)
  
  for(i in 1:length(intent.colnames)){
    intents <- Data[,colnames(Data)==intent.colnames[i]]
    # assume first listed is major party if selected more than one
    intents <- gsub(",.*", "", intents)
    
    intents[intents == "Labor"] <- "labor"
    intents[intents == "LNP"] <- "lnp"
    intents[intents == "Greens"] <- "greens"
    intents[intents == "Other"] <- "other"
    intents[intents == "Not sure"] <- NA
    
    
    if(impute==T){
      if(impute.from.joined&!impute.partial){
        intents[df$constant==1&df$joined <=i&is.na(intents)] <- df$party[df$constant==1&df$joined <=i&is.na(intents)]
      }
      if(impute.partial&!impute.from.joined){
        intents[df$constant==1&is.na(intents)&df$non.responses<length(intent.colnames)] <- df$party[df$constant==1&is.na(intents)&df$non.responses<length(intent.colnames)]
      }
      if(!impute.from.joined&!impute.partial){
        intents[df$constant==1&is.na(intents)] <- df$party[df$constant==1&is.na(intents)]
      }
      if(impute.from.joined&impute.partial){
        intents[df$constant==1&is.na(intents)&df$non.responses<length(intent.colnames)&df$joined <=i] <- df$party[df$constant==1&is.na(intents)&df$non.responses<length(intent.colnames)&df$joined <=i]
      }
    } # end if (impute)
    
    temp$temp <- intents
    # y.i =1 if intend to vote for lnp, zero otherwise. 
    temp$y.i <- NA
    if(multinomial==F){
      temp$y.i[!is.na(temp$temp)] <- 0
      temp$y.i[temp$temp==party.vote] <- 1
    }
    if(multinomial==T){
      temp$y.i[!is.na(temp$temp)] <- 4
      temp$y.i[temp$temp=="lnp"] <- 1
      temp$y.i[temp$temp=="labor"] <- 2
      temp$y.i[temp$temp=="greens"] <- 3
    }

    # mp.i = 1 if voted for labor or lnp, zero otherwise. 
    temp$mp.i <- NA
    temp$mp.i[!is.na(temp$temp)] <- 0
    if(greens.is.mp==T) temp$mp.i[temp$temp=="lnp"|temp$temp=="labor"|temp$temp=="greens"] <- 1
    if(greens.is.mp==F) temp$mp.i[temp$temp=="lnp"|temp$temp=="labor"] <- 1
    
    colnames(temp)[which(colnames(temp)=="temp")] <- paste0("intent.week.", i)
    colnames(temp)[which(colnames(temp)=="y.i")] <- paste0("y",i,".i")
    colnames(temp)[which(colnames(temp)=="mp.i")] <- paste0("mp",i,".i")
    
  } # end loop of weeks
  temp$no <- NULL
  return(temp)
}