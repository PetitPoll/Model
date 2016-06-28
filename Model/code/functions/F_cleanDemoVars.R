cleanDemoVars <- function(Data){
  # clean background variables
  df <- data.frame(sex = Data$Gender)
  df$last <- "other"
  df$last[Data$LastElection=="Did not vote in 2013"] <- NA
  df$last[Data$LastElection=="Not sure"] <- NA
  df$last[Data$LastElection=="LNP"] <- "lnp"
  df$last[Data$LastElection=="Labor"] <- "labor"
  df$last[Data$LastElection=="Greens"] <- "green"
  
  df$educ <- NA
  df$educ[Data$Education == "High school graduate"] <- 1
  df$educ[Data$Education == "Postgraduate degree (Masters/PhD)"] <- 4
  df$educ[Data$Education == "TAFE or other Certificate"] <- 2
  df$educ[Data$Education == "Undergraduate university degree"] <- 3
  
  df$party <- NA
  df$party[!is.na(Data$Malleability)] <- "other"
  df$party[Data$Malleability=="Liberal/National voter"] <- "lnp"
  df$party[Data$Malleability=="Labor voter"] <- "labor"
  df$party[Data$Malleability=="Swing voter"] <- "swing"
  df$party[Data$Malleability=="Greens voter"] <- "greens"
  
  df$age <- NA
  df$age[Data$Age=="18-29"] <- 1
  df$age[Data$Age=="30-44"] <- 2
  df$age[Data$Age=="30-45"] <- 2
  df$age[Data$Age=="45-64"] <- 3
  df$age[Data$Age=="45-65"] <- 3
  df$age[Data$Age=="65+"] <- 4
  
  df$sex.i <- NA
  df$sex.i[df$sex=="Male"] <- 1
  df$sex.i[df$sex=="Female"] <- 2
  
  df$enrolled <- Data$Enrolled
  df$state <- Data$State
  #clean
  df$state <- gsub(",.*", "", df$state)
  
  df$view <- Data$Viewpoint
  
  df <- df[!is.na(df$age)&!is.na(df$sex.i),]
  df$party.i <- as.numeric(as.factor(df$party))
  df$last.i <- as.numeric(as.factor(df$last))
  
  return(df)
}