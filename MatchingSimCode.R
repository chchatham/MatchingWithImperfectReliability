Simulation_Results <- NULL #to store and plot results of each of the num_sims
num_sims <- 25 #how many intervals of reliability to test, in 0,1 range
SampleSizeperCond <- 10000 #Number of samples per condition
MeanCondDiff <- 1 # Mean diff between conditions
caliper <- .001 #caliper - the max allowable difference btwn obs before we reject them as a potential match 

for(sim in 1:num_sims){ #perform a new simulation for every level of reliability
  REL_M <- sim/num_sims 

  #Create two conditions with different means
  Cond1_latent <- rnorm(SampleSizeperCond,0,1)
  Cond2_latent <- rnorm(SampleSizeperCond,MeanCondDiff,1)
  
  #Add measurement error to each condition
  Cond1_manifest <- Cond1_latent * REL_M + sqrt(1 - REL_M^2) * rnorm(SampleSizeperCond,0,1)
  Cond2_manifest <- Cond2_latent * REL_M + sqrt(1 - REL_M^2) * rnorm(SampleSizeperCond,0,1)

  #Find the observations that match between the two conditions, using the caliper to determine matches
  num_matches <- 0
  matched_list <- NULL
  matching_vector <- logical(length = SampleSizeperCond)
  Cond1_matched <- logical(length = SampleSizeperCond)
  Cond2_matched <- logical(length = SampleSizeperCond)
  for(i in 1:SampleSizeperCond){
    matching_vector <- abs(Cond2_manifest-Cond1_manifest[i])<caliper & !Cond2_matched
    if(length(Cond2_manifest[matching_vector])>0){ #is there a match?
      Cond1_matched[i] = TRUE #then note that this Cond1 observation has a match in Cond2 
      idx <- min(which(matching_vector == TRUE)) #find the idx of the first Cond2 item which matches
      Cond2_matched[idx] = TRUE
    }
  }
  
  Simulation_Results$latentdiff[sim] <- mean(Cond2_latent[Cond2_matched]) - mean(Cond1_latent[Cond1_matched])
  Simulation_Results$manifestdiff[sim] <- mean(Cond2_manifest[Cond2_matched]) - mean(Cond1_manifest[Cond1_matched])
  Simulation_Results$reliability[sim] <- cor(Cond1_latent,Cond1_manifest)
  print(REL_M)
}

plot(Simulation_Results$reliability^2, Simulation_Results$latentdiff, main="", xlab="Manifest/Latent Correlation", ylab="Persistent latent difference after matching")