library(httr)
library(jsonlite)

api_token <- "API_TOKEN"
survey_id <- "SURVEY_ID"
datacenter_id <- "DATACENTER_ID"

# Retrieves survey flow
get_survey_flow <- function(api_token, survey_id, datacenter_id) {
  base_url <- paste0("https://", datacenter_id, ".qualtrics.com/API/v3/survey-definitions/", survey_id, "/flow")
  response <- GET(base_url, add_headers(`X-API-TOKEN` = api_token))
  content(response)
}

# Update the survey flow with new content
update_survey_flow <- function(api_token, survey_id, modified_flow_data, datacenter_id) {
  base_url <- paste0("https://", datacenter_id, ".qualtrics.com/API/v3/survey-definitions/", survey_id, "/flow")
  json_payload <- toJSON(modified_flow_data, auto_unbox = TRUE)
  response <- PUT(base_url,
                  add_headers(`X-API-TOKEN` = api_token, 
                              `Content-Type` = "application/json"),
                  body = json_payload)
  cat("JSON Payload for PUT Request:\n")
  print(json_payload)

  content(response)
}

# Updates the embedded data in the flow with new probabilities
update_flow_with_probabilities <- function(flow, new_probabilities) {
  for(i in seq_along(new_probabilities)) {
    if(length(flow[[2]]$EmbeddedData) >= i) {
      flow[[2]]$EmbeddedData[[i]]$Value <- as.character(new_probabilities[i])
    }
  }
  return(flow)
}

# From run_simulated_qualtrics
new_probabilities <- c(0.959, 0.964, 0.964, 0.965, 0.976, 0.976, 1.000)

# Retrieve current survey flow (full structure)
current_flow <- get_survey_flow(api_token, survey_id, datacenter_id)

# Extract just the flow part for modification
current_flow_data <- current_flow$result$Flow

# Update the flow data with new probabilities
modified_flow_data <- update_flow_with_probabilities(current_flow_data, new_probabilities)

# Reconstruct the full survey configuration with the modified flow part
current_flow$result$Flow <- modified_flow_data

# Convert the entire modified survey configuration to JSON
json_payload <- toJSON(current_flow$result, auto_unbox = TRUE)

# Make the PUT request to update the survey flow
update_response <- update_survey_flow(api_token, survey_id, current_flow$result, datacenter_id)

# Check the response
print(update_response)


