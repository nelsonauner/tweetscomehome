mc_simulation <- function(yVar,myData,timeVar,treatVar,treatDuration,effectSize,effectStd,treatMo,nRuns) {
  #what are the steps that we need to take? A date length function and effect
  #We would expect to see a jump in a random seg2 size....or in seg2 count?
  require(plm);require(parallel)
  ### EVERYTHING BELOW HERE IS DATA-SET WIDE ###
  #Anyays: make a list of how many months are in the data and a list of these months
  len <- length(TimePeriods <- sort(unique(i_data$date))) #TO DO: allow generic timevar
  i_data$treatEffect <- FALSE #initialize the treatment effect. 
  #Break apart the model so we can add in a treatment effect. 
  lhs <- (splitted <- strsplit(hey,'~')[[1]])[1] # TODO: change 'hey'
  rhs <- splitted[2]
  #We also need to choose who we're going to be treating...do we treat the same in each simulation? 
  #FUNCTION TO ADD: ONLY TREAT THE MIDDLE BEARS
  
  ### EVERYTHING BELOW HERE IS SIMULATION-LEVEL SCOPE. 
  #The above variables will be found by the below function, but perhaps bad form...
  run_batch <- function(X) {
    #pick a series of months to perform on: 
    treatmentStart <- sample(1:(len-TreatDuration),1) #We'll operate on this month
    treatPeriods <- TimePeriods[treatmentStart:(treatmentStart+treatDuration)]
    #if (n_segs != "all") {
      segs <- unique(i_data$treatVar)
    treat_seg <- with(i_data,sample(1:length(segs),round(length(segs)/2)))  #TODO: segs)/2 goes to some serious error checking
    i_data$treat_effect[i_data$timeVar %in% treatPeriods & i_data$treatVar %in% treat_seg] <- TRUE
    #}
    #else {i_data$treat_effect[i_data$date %in% treat_mo] = TRUE}
    #run_regression
    da_model <- plm(as.formula(
      paste("I(",lhs,
            "+ log(1+treat_effect*effect_size)) ~ treat_effect")
      data=i_data,
      random.method='amemiya',
      index=c('mo','prd','seg','issuer','acquirer')
      )
    #
    #accuracy_lm <- lm(as.formula(
    #  paste("log(gdv) ~ treat_effect +",
    #        model_input)),
    #  data=i_data)
    
    return(da_model$coefficients[2])
  }
  #Run the simulation run_batch n_runs times, return a vector (c)
  return(
    unlist(mclapply(as.list(rep(1,n_runs)),FUN=run_batch))
  )
  #ONLY TRUE FOR A MODEL WITH INTERCEPT
  ##   paste()))
  #Now, we send run two datasets through the wringer and report results: 
}




