

library(moments)  
library(tidyverse)
library(dplyr)


# Define the function to generate item difficulties considering uncertainty
simu_diff<- function(difficulty_data, num_items = 10, num_replications = 100) {
  
  
  # Placeholder for generated difficulty parameters
  simulated_difficulties = data.frame()
  
  # Loop over the number of replications
  for (replication in 1:num_replications) {
    # Randomly select a dataset
    selected_dataset <- sample(difficulty_data$dataset, size = 1, replace = TRUE)
    
    # Filter item difficulties and standard errors for the selected dataset
    dataset_difficulties <- difficulty_data %>%
      filter(dataset == selected_dataset) %>%
      select(difficulty, SE)

    # Generate mixture distribution by adding normal noise around each difficulty
    sampled_difficulties <- unlist(
      lapply(1:nrow(dataset_difficulties), function(i) {
        rnorm(1000, mean = dataset_difficulties$difficulty[i], sd = dataset_difficulties$SE[i])
      })
    )
    
    # Estimate density distribution of the generated mixture
    density_obj <- density(sampled_difficulties, n = 10000)  # Generate density with finer resolution

    # Compute the cumulative density function (CDF)
    cdf_values <- cumsum(density_obj$y) / sum(density_obj$y)

    # Create inverse CDF function
    inverse_cdf <- approxfun(cdf_values, density_obj$x)

    # Sample from the continuous density distribution using inverse CDF
    sampled_final_difficulties <- inverse_cdf(runif(num_items))

    # Store the sampled difficulties for this replication
    simulated_difficulties <- bind_rows(simulated_difficulties, 
                                        data.frame(replication = replication, 
                                                   sampled_difficulties = sampled_final_difficulties))    
  }
  
  return(simulated_difficulties)
}

