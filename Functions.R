# Set working directory and source libraries 
source("Libraries.R")

# Function to find edge cases and output potential close matches
# This function identifies unique values in df1 that are not found in df2
# It then finds the value in df2 that most closely matches the value from df1
# The output pattern is: $<name from df1> : <closest match from df2>
find_edge_cases_and_close_matches <- function(df1, col1, df2, col2) {
  
  # Standardize the formatting of the names in df1 and df2
  df1[[col1]] <- gsub("\\.\\s+", ".", df1[[col1]])
  df2[[col2]] <- gsub("\\.\\s+", ".", df2[[col2]])
  
  # Identify edge cases: unique values in df1 not found in df2
  edge_cases <- df1[[col1]][!df1[[col1]] %in% df2[[col2]]]
  unique_edge_cases <- unique(edge_cases)
  sorted_unique_edge_cases <- sort(unique_edge_cases)
  
  # Print the number of edge cases found
  print(paste("Number of edge cases found:", length(unique_edge_cases)))
  
  # Initialize an empty list to store the results
  results <- list()
  
  # For each name in the edge cases
  for (name in sorted_unique_edge_cases) {
    # Calculate the string distance between the name and all names in df2
    distances <- stringdist::stringdistmatrix(name, df2[[col2]], method = "jw")
    
    # Get the indices of the names in df2 with the smallest distance to the name
    indices <- which(distances == min(distances), arr.ind = TRUE)[, 2]
    
    # Get the unique closest matches in df2
    unique_matches <- unique(df2[[col2]][indices])
    
    # Store the unique closest matches and the minimum distance in the results list
    results[[name]] <- list("matches" = unique_matches, "min_distance" = min(distances))
  }
  
  # Sort the results by the minimum string distance
  results <- results[order(sapply(results, function(x) x$min_distance))]
  
  # Return the results list
  return(results)
}

# Sample usage (commented out)
# Apply 'find_edge_cases_and_close_matches' function to identify edge cases and close matches

# For 'winner' column
# edge_winners <- find_edge_cases_and_close_matches(tour_data, "winner", match_data, "winner")

# For 'loser' column
# edge_losers <- find_edge_cases_and_close_matches(tour_data, "loser", match_data, "loser") 

# For 'tournament' column
# edge_tournament_names <- find_edge_cases_and_close_matches(tour_data, "tournament", match_data, "tournament")

# Print the results in a readable format
# Replace 'edge_winners' with your desired vector (edge_losers or edge_tournament_names)
#for (name in names(edge_winners)) {
#  cat(paste0("Name: ", name, "\nClosest match: ", edge_winners[[name]]$matches, "\n\n"))
#}

