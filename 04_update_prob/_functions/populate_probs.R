library(httr)
library(jsonlite)
config <- config::get()

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
    add_headers(
      `X-API-TOKEN` = api_token,
      `Content-Type` = "application/json"
    ),
    body = json_payload
  )
  cat("JSON Payload for PUT Request:\n")
  # print(json_payload)

  content(response)
}

# Updates the embedded data in the flow with new probabilities
update_flow_with_probabilities <- function(flow, flow_id, new_probabilities,
                                           new_prob_varnames) {
  # identify position in flow based on Flow ID
  # this may not be consistent across surveys
  list_pos <- which(unlist(map(flow, ~ .[["FlowID"]] == flow_id)))

  for (pi in new_prob_varnames) {
    # identify new probability by name
    new_prob <- new_probabilities[[pi]]
    # identify old probability by name
    embd_data_pos <- which(unlist(map(
      flow[[list_pos]]$EmbeddedData,
      ~ .[["Description"]] == pi
    )))
    if (length(embd_data_pos) == 0) {
      message(str_glue("{pi} not in data"))
      next
    }
    orig_prob <- flow[[list_pos]]$EmbeddedData[[embd_data_pos]]$Value

    # replace original probability with new probability
    stopifnot(flow[[list_pos]]$EmbeddedData[[embd_data_pos]]$Description == pi)
    print(str_glue(
      "For {pi}, ",
      "replacing old probability {orig_prob} ",
      "with new probability {new_prob}"
    ))
    flow[[list_pos]]$EmbeddedData[[embd_data_pos]]$Value <- new_prob
  }
  return(flow)
}
