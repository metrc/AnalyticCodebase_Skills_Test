

#' Screened
#'
#' @description To determine if a participant is screened we just need to consider if the CRF00 is complete
#' and for that we only need data from the baseline event.
#'
#' @return standard constructor output
#' @export
#'
#' @examples
#' \dontrun{
#' snow_screened()
#' }
snow_screened<-function(){
  data <- get_data(c("study_id", "redcap_event_name", "crf00_complete"))

  output <- data %>%
    filter(redcap_event_name=="baseline_arm_1" & crf00_complete==2) %>%
    mutate(screened=TRUE) %>%
    select(study_id, screened)

  output <- full_join(output, data %>%
                        select(study_id) %>%
                        distinct(),by="study_id") %>%
    mutate(screened = ifelse(is.na(screened),FALSE,screened))

  return(output)
}


#' Snow facilitycode
#'
#' @description To determine facilitycode, we just need to consider the screening form
#' and for that we only need data from the baseline event.
#'
#' @return standard constructor output
#' @export
#'
#' @examples
#' \dontrun{
#' snow_facilitycode()
#' }
snow_facilitycode <-function(){
  data <- get_data(c("study_id", "redcap_event_name", "facilitycode"))
  screened <- get_construct_output("screened")

  facilitycode <- right_join(data %>%
                               filter(redcap_event_name=="baseline_arm_1"),get_construct_output("screened"), by="study_id") %>%
    select(study_id, facilitycode)

  return(facilitycode)
}