#' pasteur enrolled_consider_covid
#'
#' @description enrolled function imports the consented, adjudicated_ineligible, pending_adjudication, 
#' has_early_discontinuation, and not_added_during_covid and combines them into a boolean for whether each ID was
#' enrolled excluding those enrolled during the height of covid.
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_enrolled_consider_covid()
#' }
pasteur_enrolled_consider_covid <- function(){
  df <- get_construct_outputs(c('consented', 'adjudicated_ineligible', 'pending_adjudication', 'has_early_discontinuation', 'not_added_during_covid'))
  
  df <- df %>%
    mutate(enrolled = consented & not_added_during_covid & !adjudicated_ineligible & !pending_adjudication & !has_early_discontinuation) %>% 
    select(study_id, enrolled)
  
  return(df)
}




#' pasteur injury_type
#'
#' @description injury type is determined by checking crf00 for the fracture types and recording ankle or plateau for each
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_injury_type()
#' }
pasteur_injury_type <- function(){
  data <- get_data(c("study_id","redcap_event_name","inc02___4","v2_inc02___4",
                     "inc02___3","v2_inc02___3","inc02___2","v2_inc02___2","inc02___1","v2_inc02___1"))
  screened <- get_construct_output("screened")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(injury_type = ifelse(inc02___4==1 | v2_inc02___4==1,"plateau",
                                ifelse(inc02___3 == 1 | v2_inc02___3 == 1 | inc02___2 == 1 | 
                                         v2_inc02___2 == 1 | inc02___1 == 1 | v2_inc02___1==1,"ankle",NA))) %>% 
    select(study_id, injury_type)
  
  df <- left_join(screened, df) %>%
    select(-screened)
    
  return(df)
}



#' pasteur refused
#'
#' @description this column is determined by checking crf00 for whether the participant refused to consent or not
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_refused()
#' }
pasteur_refused <- function(){
  data <- get_data(c("study_id","redcap_event_name","elac06"))
  screened <- get_construct_output("screened")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(refused = ifelse(elac06==0 & !is.na(elac06),TRUE,FALSE)) %>% 
    select(study_id, refused)
  
  df <- left_join(screened, df) %>%
    select(-screened)
  
  return(df)
}


#' pasteur consented
#'
#' @description this column is determined by checking crf00 for whether the participant consented in both locations in the data
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_consented()
#' }
pasteur_consented <- function(){
  data <- get_data(c("study_id","redcap_event_name","elac06","elac12y"))
  screened <- get_construct_output("screened")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(consented = ifelse(elac06==1 & !is.na(elac06) 
                              & elac12y==1 & !is.na(elac12y),TRUE,FALSE)) %>% 
    select(study_id, consented)
  
  df <- left_join(screened, df) %>%
    select(-screened)
  
  return(df)
}




#' pasteur eligible
#'
#' @description this column is determined by checking crf00 for whether the participant is eligible for the study
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_eligible()
#' }
pasteur_eligible <- function(){
  data <- get_data(c("study_id","redcap_event_name","elac01"))
  screened <- get_construct_output("screened")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(eligible = ifelse(elac01==1,TRUE,FALSE)) %>% 
    select(study_id, eligible)
  
  df <- left_join(screened, df) %>%
    select(-screened)
  
  return(df)
}



#' pasteur not_consented
#'
#' @description this column is determined by comparing the refused, eligible, and consented columns in the analytic set
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_not_consented()
#' }
pasteur_not_consented <- function(){
  
  screened <- get_construct_outputs(c("eligible","refused","consented"))
  
  screened <- screened %>% 
    mutate(not_consented = ifelse(eligible & !refused & !consented,TRUE,FALSE)) %>% 
    select(study_id, not_consented)
  
  return(screened)
}



#' pasteur early_withdraw_reason
#'
#' @description this column is determined by checking af00 for whether the participant discontinued in the study
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_early_withdraw_reason()
#' }
pasteur_early_withdraw_reason <- function(){
  data <- get_data(c("study_id","redcap_event_name","edf_dis_reason"))
  not_con <- get_construct_output("not_consented")
  
  df <- data %>%
    filter(redcap_event_name=="admin_arm_1") %>% 
    mutate(early_withdraw_reason = ifelse(edf_dis_reason	== 1, "Inappropriate Enrollment", 
                                   ifelse(edf_dis_reason	==	2, "Late Ineligibility",
                                          ifelse(edf_dis_reason	==	3, "Late Refusal",NA)))) %>% 
    filter(!is.na(early_withdraw_reason)) %>% 
    select(study_id, early_withdraw_reason)

  df <- left_join(not_con, df) %>%
    mutate(early_withdraw_reason = ifelse(not_consented,NA,early_withdraw_reason)) %>% 
    select(-not_consented)
  
  return(df)
}



#' pasteur early_withdraw
#'
#' @description this column is determined by reviewing the early_withdraw_reason column and if there is a reason then there was an early withdraw
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_early_withdraw()
#' }
pasteur_early_withdraw <- function(){
  withdraw <- get_construct_output("early_withdraw_reason")
  
  withdraw <- withdraw %>% 
    mutate(early_withdraw = ifelse(is.na(early_withdraw_reason),NA,TRUE)) %>% 
    select(-early_withdraw_reason)
  
  return(withdraw)
}

#' pasteur randomized
#'
#' @description this column is determined by checking multiple crfs for whether 
#' the participant was actually randomized into the study
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_randomized()
#' }
pasteur_randomized <- function(){
  data <- get_data(c("study_id","redcap_event_name","elig_complete",
                     "elig_randomization","elac12y","elac06","crf00_complete"))
  screened <- get_construct_output("screened")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(randomized = elig_complete == 2 & elig_randomization == 1 & 
             elac12y == 1 & elac06 == 1 & crf00_complete == 2) %>% 
    select(study_id, randomized)
  
  df <- left_join(screened, df) %>%
    select(-screened)
  
  return(df)
}




#' pasteur enrolled
#'
#' @description this column is determined by comparing the randomized to the early_withdraw columns in the analytic set
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_enrolled()
#' }
pasteur_enrolled <- function(){
  
  df <- get_construct_outputs(c("randomized","early_withdraw"))
  
  df <- df %>%
    mutate(enrolled = ifelse(randomized & (!early_withdraw |is.na(early_withdraw)),TRUE,FALSE)) %>% 
    select(study_id, enrolled)
  
  return(df)
}




#' pasteur pending_verification
#'
#' @description this column is determined by checking af00 for whether the participant discontinued in the study
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_pending_verification()
#' }
pasteur_pending_verification <- function(){
  data <- get_data(c("study_id","redcap_event_name","elac06","elac12y","elig_complete"))
  enrolled <- get_construct_output("enrolled")
  
  df <- data %>%
    filter(redcap_event_name=="baseline_arm_1") %>% 
    mutate(pending_verification = elig_complete ==0 & elac12y == 1 & elac06 == 1) %>% 
    select(study_id, pending_verification)
  
  df <- left_join(enrolled, df) %>%
    mutate(pending_verification = ifelse(enrolled,FALSE,pending_verification)) %>% 
    mutate(pending_verification = ifelse(is.na(pending_verification),FALSE, pending_verification)) %>% 
    select(-enrolled)
  
  return(df)
}




#' pasteur definitive_fixation_date
#'
#' @description definitive_fixation_date function gets the surgery data from the CRF07 Definitive Fixation Form
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_definitive_fixation_date()
#' }
pasteur_definitive_fixation_date <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'df_proc_dt'))
  
  study_ids_df <- data %>% 
    select(study_id) %>% 
    distinct()
  
  definitive_fixation_date <- data %>%
    filter(redcap_event_name == 'baseline_arm_1')%>%
    rename(definitive_fixation_date=df_proc_dt) %>% 
    select(study_id, definitive_fixation_date)
  
  definitive_fixation_date <- left_join(study_ids_df, definitive_fixation_date)
  
  return(definitive_fixation_date)
}



#' pasteur six_week_radiographs_taken
#'
#' @description six_week_radiographs_taken function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_radiographs_taken()
#' }
pasteur_six_week_radiographs_taken <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_radiographs'))
  
  study_ids_df <- data %>% 
    select(study_id) %>% 
    distinct()
  
  six_week_radiographs_taken <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_radiographs_taken=cfu_radiographs==1) %>% 
    select(study_id, six_week_radiographs_taken)
  
  six_week_radiographs_taken <- left_join(study_ids_df, six_week_radiographs_taken)
  
  return(six_week_radiographs_taken)
}




#' pasteur six_week_ankle_coronal_plane_displacement
#'
#' @description six_week_ankle_coronal_plane_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_ankle_coronal_plane_displacement()
#' }
pasteur_six_week_ankle_coronal_plane_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_corplane'))
  injury_type <- get_construct_output('injury_type')
  six_week_ankle_coronal_plane_displacement <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_ankle_coronal_plane_displacement=as.numeric(cfu_xray_ank_corplane))
  six_week_ankle_coronal_plane_displacement <- full_join(injury_type, six_week_ankle_coronal_plane_displacement) %>% mutate(six_week_ankle_coronal_plane_displacement=ifelse(injury_type=="ankle",six_week_ankle_coronal_plane_displacement,NA)) %>%
    select(study_id, six_week_ankle_coronal_plane_displacement)
  
  return(six_week_ankle_coronal_plane_displacement)
}



#' pasteur six_week_ankle_talar_tilt_degrees
#'
#' @description six_week_ankle_talar_tilt_degrees function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_ankle_talar_tilt_degrees()
#' }
pasteur_six_week_ankle_talar_tilt_degrees <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt'))
  injury_type <- get_construct_output('injury_type')
  six_week_ankle_talar_tilt_degrees <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_ankle_talar_tilt_degrees=as.numeric(cfu_xray_ank_talartilt))
  six_week_ankle_talar_tilt_degrees <- full_join(injury_type, six_week_ankle_talar_tilt_degrees) %>% mutate(six_week_ankle_talar_tilt_degrees=ifelse(injury_type=="ankle",six_week_ankle_talar_tilt_degrees,NA)) %>%
    select(study_id, six_week_ankle_talar_tilt_degrees)
  
  return(six_week_ankle_talar_tilt_degrees)
}



#' pasteur six_week_ankle_talar_tilt_varus_or_valgus
#'
#' @description six_week_ankle_talar_tilt_varus_or_valgus function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_ankle_talar_tilt_varus_or_valgus()
#' }
pasteur_six_week_ankle_talar_tilt_varus_or_valgus <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt_sp'))
  injury_type <- get_construct_output('injury_type')
  six_week_ankle_talar_tilt_varus_or_valgus <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_ankle_talar_tilt_varus_or_valgus=ifelse(cfu_xray_ank_talartilt_sp=="1","Varus",ifelse(cfu_xray_ank_talartilt_sp=="2","Valgus",NA)))
  six_week_ankle_talar_tilt_varus_or_valgus <- full_join(injury_type, six_week_ankle_talar_tilt_varus_or_valgus) %>% mutate(six_week_ankle_talar_tilt_varus_or_valgus=ifelse(injury_type=="ankle",six_week_ankle_talar_tilt_varus_or_valgus,NA)) %>%
    select(study_id, six_week_ankle_talar_tilt_varus_or_valgus)
  
  return(six_week_ankle_talar_tilt_varus_or_valgus)
}



#' pasteur six_week_ankle_sagital_displacement
#'
#' @description six_week_ankle_sagital_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_ankle_sagital_displacement()
#' }
pasteur_six_week_ankle_sagital_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_sagdis'))
  injury_type <- get_construct_output('injury_type')
  six_week_ankle_sagital_displacement <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_ankle_sagital_displacement=as.numeric(cfu_xray_ank_sagdis))
  six_week_ankle_sagital_displacement <- full_join(injury_type, six_week_ankle_sagital_displacement) %>% mutate(six_week_ankle_sagital_displacement=ifelse(injury_type=="ankle",six_week_ankle_sagital_displacement,NA)) %>%
    select(study_id, six_week_ankle_sagital_displacement)
  
  return(six_week_ankle_sagital_displacement)
}



#' pasteur six_week_plateau_condylar_width
#'
#' @description six_week_plateau_condylar_width function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_condylar_width()
#' }
pasteur_six_week_plateau_condylar_width <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_con'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_condylar_width <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_condylar_width=as.numeric(cfu_xray_tib_con))
  six_week_plateau_condylar_width <- full_join(injury_type, six_week_plateau_condylar_width) %>% mutate(six_week_plateau_condylar_width=ifelse(injury_type=="plateau",six_week_plateau_condylar_width,NA)) %>%
    select(study_id, six_week_plateau_condylar_width)
  
  return(six_week_plateau_condylar_width)
}



#' pasteur six_week_plateau_medial_and_lateral_difference
#'
#' @description six_week_plateau_medial_and_lateral_difference function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_medial_and_lateral_difference()
#' }
pasteur_six_week_plateau_medial_and_lateral_difference <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_med_lat','cfu_xray_tib_med_lat_sp'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_medial_and_lateral_difference <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_medial_and_lateral_difference=paste(ifelse(cfu_xray_tib_med_lat_sp=="1","Medial",ifelse(cfu_xray_tib_med_lat_sp=="2","Lateral",NA)),as.numeric(cfu_xray_tib_med_lat)))
  six_week_plateau_medial_and_lateral_difference <- full_join(injury_type, six_week_plateau_medial_and_lateral_difference) %>% mutate(six_week_plateau_medial_and_lateral_difference=ifelse(injury_type=="plateau",six_week_plateau_medial_and_lateral_difference,NA)) %>%
    select(study_id, six_week_plateau_medial_and_lateral_difference)
  
  return(six_week_plateau_medial_and_lateral_difference)
}



#' pasteur six_week_plateau_angle_between_femur_and_tibia
#'
#' @description six_week_plateau_angle_between_femur_and_tibia function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_angle_between_femur_and_tibia()
#' }
pasteur_six_week_plateau_angle_between_femur_and_tibia <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_fem_tib'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_angle_between_femur_and_tibia <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_angle_between_femur_and_tibia=as.numeric(cfu_xray_tib_fem_tib))
  six_week_plateau_angle_between_femur_and_tibia <- full_join(injury_type, six_week_plateau_angle_between_femur_and_tibia) %>% mutate(six_week_plateau_angle_between_femur_and_tibia=ifelse(injury_type=="plateau",six_week_plateau_angle_between_femur_and_tibia,NA)) %>%
    select(study_id, six_week_plateau_angle_between_femur_and_tibia)
  
  return(six_week_plateau_angle_between_femur_and_tibia)
}



#' pasteur six_week_plateau_medial_proximal_tibia_angle
#'
#' @description six_week_plateau_medial_proximal_tibia_angle function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_medial_proximal_tibia_angle()
#' }
pasteur_six_week_plateau_medial_proximal_tibia_angle <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_ang'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_medial_proximal_tibia_angle <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_medial_proximal_tibia_angle=as.numeric(cfu_xray_tib_ang))
  six_week_plateau_medial_proximal_tibia_angle <- full_join(injury_type, six_week_plateau_medial_proximal_tibia_angle) %>% mutate(six_week_plateau_medial_proximal_tibia_angle=ifelse(injury_type=="plateau",six_week_plateau_medial_proximal_tibia_angle,NA)) %>%
    select(study_id, six_week_plateau_medial_proximal_tibia_angle)
  
  return(six_week_plateau_medial_proximal_tibia_angle)
}



#' pasteur six_week_plateau_sagittal_plane_alignment
#'
#' @description six_week_plateau_sagittal_plane_alignment function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_sagittal_plane_alignment()
#' }
pasteur_six_week_plateau_sagittal_plane_alignment <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_sag_plane','cfu_xray_sag_plane_sp'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_sagittal_plane_alignment <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1' & !is.na(cfu_xray_sag_plane_sp))%>%
    mutate(six_week_plateau_sagittal_plane_alignment=paste(ifelse(cfu_xray_sag_plane_sp=="1","Posterior",ifelse(cfu_xray_sag_plane_sp=="2","Anterior",NA)),as.numeric(cfu_xray_sag_plane)))
  six_week_plateau_sagittal_plane_alignment <- full_join(injury_type, six_week_plateau_sagittal_plane_alignment) %>% mutate(six_week_plateau_sagittal_plane_alignment=ifelse(injury_type=="plateau",six_week_plateau_sagittal_plane_alignment,NA)) %>%
    select(study_id, six_week_plateau_sagittal_plane_alignment)
  
  return(six_week_plateau_sagittal_plane_alignment)
}



#' pasteur six_week_plateau_articular_step_off_lateral
#'
#' @description six_week_plateau_articular_step_off_lateral function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_articular_step_off_lateral()
#' }
pasteur_six_week_plateau_articular_step_off_lateral <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_lat'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_articular_step_off_lateral <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_articular_step_off_lateral=cfu_xray_tib_art_lat=="1")
  six_week_plateau_articular_step_off_lateral <- full_join(injury_type, six_week_plateau_articular_step_off_lateral) %>% mutate(six_week_plateau_articular_step_off_lateral=ifelse(injury_type=="plateau",six_week_plateau_articular_step_off_lateral,NA)) %>%
    select(study_id, six_week_plateau_articular_step_off_lateral)
  
  return(six_week_plateau_articular_step_off_lateral)
}



#' pasteur six_week_plateau_articular_step_off_medial
#'
#' @description six_week_plateau_articular_step_off_medial function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_articular_step_off_medial()
#' }
pasteur_six_week_plateau_articular_step_off_medial <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_ap'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_articular_step_off_medial <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_articular_step_off_medial=cfu_xray_tib_art_ap=="1")
  six_week_plateau_articular_step_off_medial <- full_join(injury_type, six_week_plateau_articular_step_off_medial) %>% mutate(six_week_plateau_articular_step_off_medial=ifelse(injury_type=="plateau",six_week_plateau_articular_step_off_medial,NA)) %>%
    select(study_id, six_week_plateau_articular_step_off_medial)
  
  return(six_week_plateau_articular_step_off_medial)
}



#' pasteur six_week_plateau_patella_centered
#'
#' @description six_week_plateau_patella_centered function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_patella_centered()
#' }
pasteur_six_week_plateau_patella_centered <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_pat'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_patella_centered <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_patella_centered=cfu_xray_tib_pat=="1")
  six_week_plateau_patella_centered <- full_join(injury_type, six_week_plateau_patella_centered) %>% mutate(six_week_plateau_patella_centered=ifelse(injury_type=="plateau",six_week_plateau_patella_centered,NA)) %>%
    select(study_id, six_week_plateau_patella_centered)
  
  return(six_week_plateau_patella_centered)
}



#' pasteur six_week_plateau_tib_fib_overlap
#'
#' @description six_week_plateau_tib_fib_overlap function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_week_plateau_tib_fib_overlap()
#' }
pasteur_six_week_plateau_tib_fib_overlap <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_fib_overlap'))
  injury_type <- get_construct_output('injury_type')
  six_week_plateau_tib_fib_overlap <- data %>%
    filter(redcap_event_name == '6_week_followup_arm_1')%>%
    mutate(six_week_plateau_tib_fib_overlap=as.numeric(cfu_xray_fib_overlap))
  six_week_plateau_tib_fib_overlap <- full_join(injury_type, six_week_plateau_tib_fib_overlap) %>% mutate(six_week_plateau_tib_fib_overlap=ifelse(injury_type=="plateau",six_week_plateau_tib_fib_overlap,NA)) %>%
    select(study_id, six_week_plateau_tib_fib_overlap)
  
  return(six_week_plateau_tib_fib_overlap)
}



#' pasteur three_month_radiographs_taken
#'
#' @description three_month_radiographs_taken function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_radiographs_taken()
#' }
pasteur_three_month_radiographs_taken <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_radiographs'))
 
  study_ids_df <- data %>% 
    select(study_id) %>% 
    distinct()
  
  three_month_radiographs_taken <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_radiographs_taken=cfu_radiographs==1) %>% 
    select(study_id, three_month_radiographs_taken)
  
  three_month_radiographs_taken <- left_join(study_ids_df, three_month_radiographs_taken)
  
  return(three_month_radiographs_taken)
}



#' pasteur three_month_ankle_coronal_plane_displacement
#'
#' @description three_month_ankle_coronal_plane_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_ankle_coronal_plane_displacement()
#' }
pasteur_three_month_ankle_coronal_plane_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_corplane'))
  injury_type <- get_construct_output('injury_type')
  three_month_ankle_coronal_plane_displacement <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_ankle_coronal_plane_displacement=as.numeric(cfu_xray_ank_corplane))
  three_month_ankle_coronal_plane_displacement <- full_join(injury_type, three_month_ankle_coronal_plane_displacement) %>% mutate(three_month_ankle_coronal_plane_displacement=ifelse(injury_type=="ankle",three_month_ankle_coronal_plane_displacement,NA)) %>%
    select(study_id, three_month_ankle_coronal_plane_displacement)
  
  return(three_month_ankle_coronal_plane_displacement)
}



#' pasteur three_month_ankle_talar_tilt_degrees
#'
#' @description three_month_ankle_talar_tilt_degrees function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_ankle_talar_tilt_degrees()
#' }
pasteur_three_month_ankle_talar_tilt_degrees <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt'))
  injury_type <- get_construct_output('injury_type')
  three_month_ankle_talar_tilt_degrees <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_ankle_talar_tilt_degrees=as.numeric(cfu_xray_ank_talartilt))
  three_month_ankle_talar_tilt_degrees <- full_join(injury_type, three_month_ankle_talar_tilt_degrees) %>% mutate(three_month_ankle_talar_tilt_degrees=ifelse(injury_type=="ankle",three_month_ankle_talar_tilt_degrees,NA)) %>%
    select(study_id, three_month_ankle_talar_tilt_degrees)
  
  return(three_month_ankle_talar_tilt_degrees)
}



#' pasteur three_month_ankle_talar_tilt_varus_or_valgus
#'
#' @description three_month_ankle_talar_tilt_varus_or_valgus function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_ankle_talar_tilt_varus_or_valgus()
#' }
pasteur_three_month_ankle_talar_tilt_varus_or_valgus <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt_sp'))
  injury_type <- get_construct_output('injury_type')
  three_month_ankle_talar_tilt_varus_or_valgus <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_ankle_talar_tilt_varus_or_valgus=ifelse(cfu_xray_ank_talartilt_sp=="1","Varus",ifelse(cfu_xray_ank_talartilt_sp=="2","Valgus",NA)))
  three_month_ankle_talar_tilt_varus_or_valgus <- full_join(injury_type, three_month_ankle_talar_tilt_varus_or_valgus) %>% mutate(three_month_ankle_talar_tilt_varus_or_valgus=ifelse(injury_type=="ankle",three_month_ankle_talar_tilt_varus_or_valgus,NA)) %>%
    select(study_id, three_month_ankle_talar_tilt_varus_or_valgus)
  
  return(three_month_ankle_talar_tilt_varus_or_valgus)
}



#' pasteur three_month_ankle_sagital_displacement
#'
#' @description three_month_ankle_sagital_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_ankle_sagital_displacement()
#' }
pasteur_three_month_ankle_sagital_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_sagdis'))
  injury_type <- get_construct_output('injury_type')
  three_month_ankle_sagital_displacement <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_ankle_sagital_displacement=as.numeric(cfu_xray_ank_sagdis))
  three_month_ankle_sagital_displacement <- full_join(injury_type, three_month_ankle_sagital_displacement) %>% mutate(three_month_ankle_sagital_displacement=ifelse(injury_type=="ankle",three_month_ankle_sagital_displacement,NA)) %>%
    select(study_id, three_month_ankle_sagital_displacement)
  
  return(three_month_ankle_sagital_displacement)
}



#' pasteur three_month_plateau_condylar_width
#'
#' @description three_month_plateau_condylar_width function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_condylar_width()
#' }
pasteur_three_month_plateau_condylar_width <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_con'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_condylar_width <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_condylar_width=as.numeric(cfu_xray_tib_con))
  three_month_plateau_condylar_width <- full_join(injury_type, three_month_plateau_condylar_width) %>% mutate(three_month_plateau_condylar_width=ifelse(injury_type=="plateau",three_month_plateau_condylar_width,NA)) %>%
    select(study_id, three_month_plateau_condylar_width)
  
  return(three_month_plateau_condylar_width)
}



#' pasteur three_month_plateau_medial_and_lateral_difference
#'
#' @description three_month_plateau_medial_and_lateral_difference function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_medial_and_lateral_difference()
#' }
pasteur_three_month_plateau_medial_and_lateral_difference <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_med_lat','cfu_xray_tib_med_lat_sp'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_medial_and_lateral_difference <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_medial_and_lateral_difference=paste(ifelse(cfu_xray_tib_med_lat_sp=="1","Medial",ifelse(cfu_xray_tib_med_lat_sp=="2","Lateral",NA)),as.numeric(cfu_xray_tib_med_lat)))
  three_month_plateau_medial_and_lateral_difference <- full_join(injury_type, three_month_plateau_medial_and_lateral_difference) %>% mutate(three_month_plateau_medial_and_lateral_difference=ifelse(injury_type=="plateau",three_month_plateau_medial_and_lateral_difference,NA)) %>%
    select(study_id, three_month_plateau_medial_and_lateral_difference)
  
  return(three_month_plateau_medial_and_lateral_difference)
}



#' pasteur three_month_plateau_angle_between_femur_and_tibia
#'
#' @description three_month_plateau_angle_between_femur_and_tibia function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_angle_between_femur_and_tibia()
#' }
pasteur_three_month_plateau_angle_between_femur_and_tibia <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_fem_tib'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_angle_between_femur_and_tibia <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_angle_between_femur_and_tibia=as.numeric(cfu_xray_tib_fem_tib))
  three_month_plateau_angle_between_femur_and_tibia <- full_join(injury_type, three_month_plateau_angle_between_femur_and_tibia) %>% mutate(three_month_plateau_angle_between_femur_and_tibia=ifelse(injury_type=="plateau",three_month_plateau_angle_between_femur_and_tibia,NA)) %>%
    select(study_id, three_month_plateau_angle_between_femur_and_tibia)
  
  return(three_month_plateau_angle_between_femur_and_tibia)
}



#' pasteur three_month_plateau_medial_proximal_tibia_angle
#'
#' @description three_month_plateau_medial_proximal_tibia_angle function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_medial_proximal_tibia_angle()
#' }
pasteur_three_month_plateau_medial_proximal_tibia_angle <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_ang'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_medial_proximal_tibia_angle <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_medial_proximal_tibia_angle=as.numeric(cfu_xray_tib_ang))
  three_month_plateau_medial_proximal_tibia_angle <- full_join(injury_type, three_month_plateau_medial_proximal_tibia_angle) %>% mutate(three_month_plateau_medial_proximal_tibia_angle=ifelse(injury_type=="plateau",three_month_plateau_medial_proximal_tibia_angle,NA)) %>%
    select(study_id, three_month_plateau_medial_proximal_tibia_angle)
  
  return(three_month_plateau_medial_proximal_tibia_angle)
}



#' pasteur three_month_plateau_sagittal_plane_alignment
#'
#' @description three_month_plateau_sagittal_plane_alignment function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_sagittal_plane_alignment()
#' }
pasteur_three_month_plateau_sagittal_plane_alignment <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_sag_plane','cfu_xray_sag_plane_sp'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_sagittal_plane_alignment <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1' & !is.na(cfu_xray_sag_plane_sp))%>%
    mutate(three_month_plateau_sagittal_plane_alignment=paste(ifelse(cfu_xray_sag_plane_sp=="1","Posterior",ifelse(cfu_xray_sag_plane_sp=="2","Anterior",NA)),as.numeric(cfu_xray_sag_plane)))
  three_month_plateau_sagittal_plane_alignment <- full_join(injury_type, three_month_plateau_sagittal_plane_alignment) %>% mutate(three_month_plateau_sagittal_plane_alignment=ifelse(injury_type=="plateau",three_month_plateau_sagittal_plane_alignment,NA)) %>%
    select(study_id, three_month_plateau_sagittal_plane_alignment)
  
  return(three_month_plateau_sagittal_plane_alignment)
}



#' pasteur three_month_plateau_articular_step_off_lateral
#'
#' @description three_month_plateau_articular_step_off_lateral function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_articular_step_off_lateral()
#' }
pasteur_three_month_plateau_articular_step_off_lateral <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_lat'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_articular_step_off_lateral <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_articular_step_off_lateral=cfu_xray_tib_art_lat=="1")
  three_month_plateau_articular_step_off_lateral <- full_join(injury_type, three_month_plateau_articular_step_off_lateral) %>% mutate(three_month_plateau_articular_step_off_lateral=ifelse(injury_type=="plateau",three_month_plateau_articular_step_off_lateral,NA)) %>%
    select(study_id, three_month_plateau_articular_step_off_lateral)
  
  return(three_month_plateau_articular_step_off_lateral)
}



#' pasteur three_month_plateau_articular_step_off_medial
#'
#' @description three_month_plateau_articular_step_off_medial function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_articular_step_off_medial()
#' }
pasteur_three_month_plateau_articular_step_off_medial <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_ap'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_articular_step_off_medial <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_articular_step_off_medial=cfu_xray_tib_art_ap=="1")
  three_month_plateau_articular_step_off_medial <- full_join(injury_type, three_month_plateau_articular_step_off_medial) %>% mutate(three_month_plateau_articular_step_off_medial=ifelse(injury_type=="plateau",three_month_plateau_articular_step_off_medial,NA)) %>%
    select(study_id, three_month_plateau_articular_step_off_medial)
  
  return(three_month_plateau_articular_step_off_medial)
}



#' pasteur three_month_plateau_patella_centered
#'
#' @description three_month_plateau_patella_centered function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_patella_centered()
#' }
pasteur_three_month_plateau_patella_centered <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_pat'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_patella_centered <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_patella_centered=cfu_xray_tib_pat=="1")
  three_month_plateau_patella_centered <- full_join(injury_type, three_month_plateau_patella_centered) %>% mutate(three_month_plateau_patella_centered=ifelse(injury_type=="plateau",three_month_plateau_patella_centered,NA)) %>%
    select(study_id, three_month_plateau_patella_centered)
  
  return(three_month_plateau_patella_centered)
}



#' pasteur three_month_plateau_tib_fib_overlap
#'
#' @description three_month_plateau_tib_fib_overlap function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_three_month_plateau_tib_fib_overlap()
#' }
pasteur_three_month_plateau_tib_fib_overlap <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_fib_overlap'))
  injury_type <- get_construct_output('injury_type')
  three_month_plateau_tib_fib_overlap <- data %>%
    filter(redcap_event_name == '3_month_followup_arm_1')%>%
    mutate(three_month_plateau_tib_fib_overlap=as.numeric(cfu_xray_fib_overlap))
  three_month_plateau_tib_fib_overlap <- full_join(injury_type, three_month_plateau_tib_fib_overlap) %>% mutate(three_month_plateau_tib_fib_overlap=ifelse(injury_type=="plateau",three_month_plateau_tib_fib_overlap,NA)) %>%
    select(study_id, three_month_plateau_tib_fib_overlap)
  
  return(three_month_plateau_tib_fib_overlap)
}



#' pasteur six_month_radiographs_taken
#'
#' @description six_month_radiographs_taken function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_radiographs_taken()
#' }
pasteur_six_month_radiographs_taken <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_radiographs'))
  
  study_ids_df <- data %>% 
    select(study_id) %>% 
    distinct()
  
  six_month_radiographs_taken <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_radiographs_taken=cfu_radiographs==1) %>% 
    select(study_id, six_month_radiographs_taken)
  
  six_month_radiographs_taken <- left_join(study_ids_df, six_month_radiographs_taken)
  
  return(six_month_radiographs_taken)
}



#' pasteur six_month_ankle_coronal_plane_displacement
#'
#' @description six_month_ankle_coronal_plane_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_ankle_coronal_plane_displacement()
#' }
pasteur_six_month_ankle_coronal_plane_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_corplane'))
  injury_type <- get_construct_output('injury_type')
  six_month_ankle_coronal_plane_displacement <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_ankle_coronal_plane_displacement=as.numeric(cfu_xray_ank_corplane))
  six_month_ankle_coronal_plane_displacement <- full_join(injury_type, six_month_ankle_coronal_plane_displacement) %>% mutate(six_month_ankle_coronal_plane_displacement=ifelse(injury_type=="ankle",six_month_ankle_coronal_plane_displacement,NA)) %>%
    select(study_id, six_month_ankle_coronal_plane_displacement)
  
  return(six_month_ankle_coronal_plane_displacement)
}



#' pasteur six_month_ankle_talar_tilt_degrees
#'
#' @description six_month_ankle_talar_tilt_degrees function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_ankle_talar_tilt_degrees()
#' }
pasteur_six_month_ankle_talar_tilt_degrees <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt'))
  injury_type <- get_construct_output('injury_type')
  six_month_ankle_talar_tilt_degrees <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_ankle_talar_tilt_degrees=as.numeric(cfu_xray_ank_talartilt))
  six_month_ankle_talar_tilt_degrees <- full_join(injury_type, six_month_ankle_talar_tilt_degrees) %>% mutate(six_month_ankle_talar_tilt_degrees=ifelse(injury_type=="ankle",six_month_ankle_talar_tilt_degrees,NA)) %>%
    select(study_id, six_month_ankle_talar_tilt_degrees)
  
  return(six_month_ankle_talar_tilt_degrees)
}



#' pasteur six_month_ankle_talar_tilt_varus_or_valgus
#'
#' @description six_month_ankle_talar_tilt_varus_or_valgus function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_ankle_talar_tilt_varus_or_valgus()
#' }
pasteur_six_month_ankle_talar_tilt_varus_or_valgus <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt_sp'))
  injury_type <- get_construct_output('injury_type')
  six_month_ankle_talar_tilt_varus_or_valgus <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_ankle_talar_tilt_varus_or_valgus=ifelse(cfu_xray_ank_talartilt_sp=="1","Varus",ifelse(cfu_xray_ank_talartilt_sp=="2","Valgus",NA)))
  six_month_ankle_talar_tilt_varus_or_valgus <- full_join(injury_type, six_month_ankle_talar_tilt_varus_or_valgus) %>% mutate(six_month_ankle_talar_tilt_varus_or_valgus=ifelse(injury_type=="ankle",six_month_ankle_talar_tilt_varus_or_valgus,NA)) %>%
    select(study_id, six_month_ankle_talar_tilt_varus_or_valgus)
  
  return(six_month_ankle_talar_tilt_varus_or_valgus)
}



#' pasteur six_month_ankle_sagital_displacement
#'
#' @description six_month_ankle_sagital_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_ankle_sagital_displacement()
#' }
pasteur_six_month_ankle_sagital_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_sagdis'))
  injury_type <- get_construct_output('injury_type')
  six_month_ankle_sagital_displacement <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_ankle_sagital_displacement=as.numeric(cfu_xray_ank_sagdis))
  six_month_ankle_sagital_displacement <- full_join(injury_type, six_month_ankle_sagital_displacement) %>% mutate(six_month_ankle_sagital_displacement=ifelse(injury_type=="ankle",six_month_ankle_sagital_displacement,NA)) %>%
    select(study_id, six_month_ankle_sagital_displacement)
  
  return(six_month_ankle_sagital_displacement)
}



#' pasteur six_month_plateau_condylar_width
#'
#' @description six_month_plateau_condylar_width function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_condylar_width()
#' }
pasteur_six_month_plateau_condylar_width <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_con'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_condylar_width <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_condylar_width=as.numeric(cfu_xray_tib_con))
  six_month_plateau_condylar_width <- full_join(injury_type, six_month_plateau_condylar_width) %>% mutate(six_month_plateau_condylar_width=ifelse(injury_type=="plateau",six_month_plateau_condylar_width,NA)) %>%
    select(study_id, six_month_plateau_condylar_width)
  
  return(six_month_plateau_condylar_width)
}



#' pasteur six_month_plateau_medial_and_lateral_difference
#'
#' @description six_month_plateau_medial_and_lateral_difference function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_medial_and_lateral_difference()
#' }
pasteur_six_month_plateau_medial_and_lateral_difference <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_med_lat','cfu_xray_tib_med_lat_sp'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_medial_and_lateral_difference <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_medial_and_lateral_difference=paste(ifelse(cfu_xray_tib_med_lat_sp=="1","Medial",ifelse(cfu_xray_tib_med_lat_sp=="2","Lateral",NA)),as.numeric(cfu_xray_tib_med_lat)))
  six_month_plateau_medial_and_lateral_difference <- full_join(injury_type, six_month_plateau_medial_and_lateral_difference) %>% mutate(six_month_plateau_medial_and_lateral_difference=ifelse(injury_type=="plateau",six_month_plateau_medial_and_lateral_difference,NA)) %>%
    select(study_id, six_month_plateau_medial_and_lateral_difference)
  
  return(six_month_plateau_medial_and_lateral_difference)
}



#' pasteur six_month_plateau_angle_between_femur_and_tibia
#'
#' @description six_month_plateau_angle_between_femur_and_tibia function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_angle_between_femur_and_tibia()
#' }
pasteur_six_month_plateau_angle_between_femur_and_tibia <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_fem_tib'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_angle_between_femur_and_tibia <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_angle_between_femur_and_tibia=as.numeric(cfu_xray_tib_fem_tib))
  six_month_plateau_angle_between_femur_and_tibia <- full_join(injury_type, six_month_plateau_angle_between_femur_and_tibia) %>% mutate(six_month_plateau_angle_between_femur_and_tibia=ifelse(injury_type=="plateau",six_month_plateau_angle_between_femur_and_tibia,NA)) %>%
    select(study_id, six_month_plateau_angle_between_femur_and_tibia)
  
  return(six_month_plateau_angle_between_femur_and_tibia)
}



#' pasteur six_month_plateau_medial_proximal_tibia_angle
#'
#' @description six_month_plateau_medial_proximal_tibia_angle function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_medial_proximal_tibia_angle()
#' }
pasteur_six_month_plateau_medial_proximal_tibia_angle <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_ang'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_medial_proximal_tibia_angle <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_medial_proximal_tibia_angle=as.numeric(cfu_xray_tib_ang))
  six_month_plateau_medial_proximal_tibia_angle <- full_join(injury_type, six_month_plateau_medial_proximal_tibia_angle) %>% mutate(six_month_plateau_medial_proximal_tibia_angle=ifelse(injury_type=="plateau",six_month_plateau_medial_proximal_tibia_angle,NA)) %>%
    select(study_id, six_month_plateau_medial_proximal_tibia_angle)
  
  return(six_month_plateau_medial_proximal_tibia_angle)
}



#' pasteur six_month_plateau_sagittal_plane_alignment
#'
#' @description six_month_plateau_sagittal_plane_alignment function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_sagittal_plane_alignment()
#' }
pasteur_six_month_plateau_sagittal_plane_alignment <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_sag_plane','cfu_xray_sag_plane_sp'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_sagittal_plane_alignment <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1' & !is.na(cfu_xray_sag_plane_sp))%>%
    mutate(six_month_plateau_sagittal_plane_alignment=paste(ifelse(cfu_xray_sag_plane_sp=="1","Posterior",ifelse(cfu_xray_sag_plane_sp=="2","Anterior",NA)),as.numeric(cfu_xray_sag_plane)))
  six_month_plateau_sagittal_plane_alignment <- full_join(injury_type, six_month_plateau_sagittal_plane_alignment) %>% mutate(six_month_plateau_sagittal_plane_alignment=ifelse(injury_type=="plateau",six_month_plateau_sagittal_plane_alignment,NA)) %>%
    select(study_id, six_month_plateau_sagittal_plane_alignment)
  
  return(six_month_plateau_sagittal_plane_alignment)
}



#' pasteur six_month_plateau_articular_step_off_lateral
#'
#' @description six_month_plateau_articular_step_off_lateral function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_articular_step_off_lateral()
#' }
pasteur_six_month_plateau_articular_step_off_lateral <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_lat'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_articular_step_off_lateral <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_articular_step_off_lateral=cfu_xray_tib_art_lat=="1")
  six_month_plateau_articular_step_off_lateral <- full_join(injury_type, six_month_plateau_articular_step_off_lateral) %>% mutate(six_month_plateau_articular_step_off_lateral=ifelse(injury_type=="plateau",six_month_plateau_articular_step_off_lateral,NA)) %>%
    select(study_id, six_month_plateau_articular_step_off_lateral)
  
  return(six_month_plateau_articular_step_off_lateral)
}



#' pasteur six_month_plateau_articular_step_off_medial
#'
#' @description six_month_plateau_articular_step_off_medial function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_articular_step_off_medial()
#' }
pasteur_six_month_plateau_articular_step_off_medial <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_ap'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_articular_step_off_medial <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_articular_step_off_medial=cfu_xray_tib_art_ap=="1")
  six_month_plateau_articular_step_off_medial <- full_join(injury_type, six_month_plateau_articular_step_off_medial) %>% mutate(six_month_plateau_articular_step_off_medial=ifelse(injury_type=="plateau",six_month_plateau_articular_step_off_medial,NA)) %>%
    select(study_id, six_month_plateau_articular_step_off_medial)
  
  return(six_month_plateau_articular_step_off_medial)
}



#' pasteur six_month_plateau_patella_centered
#'
#' @description six_month_plateau_patella_centered function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_patella_centered()
#' }
pasteur_six_month_plateau_patella_centered <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_pat'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_patella_centered <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_patella_centered=cfu_xray_tib_pat=="1")
  six_month_plateau_patella_centered <- full_join(injury_type, six_month_plateau_patella_centered) %>% mutate(six_month_plateau_patella_centered=ifelse(injury_type=="plateau",six_month_plateau_patella_centered,NA)) %>%
    select(study_id, six_month_plateau_patella_centered)
  
  return(six_month_plateau_patella_centered)
}



#' pasteur six_month_plateau_tib_fib_overlap
#'
#' @description six_month_plateau_tib_fib_overlap function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_six_month_plateau_tib_fib_overlap()
#' }
pasteur_six_month_plateau_tib_fib_overlap <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_fib_overlap'))
  injury_type <- get_construct_output('injury_type')
  six_month_plateau_tib_fib_overlap <- data %>%
    filter(redcap_event_name == '6_month_followup_arm_1')%>%
    mutate(six_month_plateau_tib_fib_overlap=as.numeric(cfu_xray_fib_overlap))
  six_month_plateau_tib_fib_overlap <- full_join(injury_type, six_month_plateau_tib_fib_overlap) %>% mutate(six_month_plateau_tib_fib_overlap=ifelse(injury_type=="plateau",six_month_plateau_tib_fib_overlap,NA)) %>%
    select(study_id, six_month_plateau_tib_fib_overlap)
  
  return(six_month_plateau_tib_fib_overlap)
}



#' pasteur twelve_month_radiographs_taken
#'
#' @description twelve_month_radiographs_taken function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_radiographs_taken()
#' }
pasteur_twelve_month_radiographs_taken <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_radiographs'))
  
  study_ids_df <- data %>% 
    select(study_id) %>% 
    distinct()
  
  twelve_month_radiographs_taken <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_radiographs_taken=cfu_radiographs==1) %>% 
    select(study_id, twelve_month_radiographs_taken)
  
  twelve_month_radiographs_taken <- left_join(study_ids_df, twelve_month_radiographs_taken)

return(twelve_month_radiographs_taken)
}



#' pasteur twelve_month_ankle_coronal_plane_displacement
#'
#' @description twelve_month_ankle_coronal_plane_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_ankle_coronal_plane_displacement()
#' }
pasteur_twelve_month_ankle_coronal_plane_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_corplane'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_ankle_coronal_plane_displacement <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_ankle_coronal_plane_displacement=as.numeric(cfu_xray_ank_corplane))
  twelve_month_ankle_coronal_plane_displacement <- full_join(injury_type, twelve_month_ankle_coronal_plane_displacement) %>% mutate(twelve_month_ankle_coronal_plane_displacement=ifelse(injury_type=="ankle",twelve_month_ankle_coronal_plane_displacement,NA)) %>%
    select(study_id, twelve_month_ankle_coronal_plane_displacement)
  
  return(twelve_month_ankle_coronal_plane_displacement)
}



#' pasteur twelve_month_ankle_talar_tilt_degrees
#'
#' @description twelve_month_ankle_talar_tilt_degrees function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_ankle_talar_tilt_degrees()
#' }
pasteur_twelve_month_ankle_talar_tilt_degrees <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_ankle_talar_tilt_degrees <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_ankle_talar_tilt_degrees=as.numeric(cfu_xray_ank_talartilt))
  twelve_month_ankle_talar_tilt_degrees <- full_join(injury_type, twelve_month_ankle_talar_tilt_degrees) %>% mutate(twelve_month_ankle_talar_tilt_degrees=ifelse(injury_type=="ankle",twelve_month_ankle_talar_tilt_degrees,NA)) %>%
    select(study_id, twelve_month_ankle_talar_tilt_degrees)
  
  return(twelve_month_ankle_talar_tilt_degrees)
}



#' pasteur twelve_month_ankle_talar_tilt_varus_or_valgus
#'
#' @description twelve_month_ankle_talar_tilt_varus_or_valgus function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_ankle_talar_tilt_varus_or_valgus()
#' }
pasteur_twelve_month_ankle_talar_tilt_varus_or_valgus <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_talartilt_sp'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_ankle_talar_tilt_varus_or_valgus <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_ankle_talar_tilt_varus_or_valgus=ifelse(cfu_xray_ank_talartilt_sp=="1","Varus",ifelse(cfu_xray_ank_talartilt_sp=="2","Valgus",NA)))
  twelve_month_ankle_talar_tilt_varus_or_valgus <- full_join(injury_type, twelve_month_ankle_talar_tilt_varus_or_valgus) %>% mutate(twelve_month_ankle_talar_tilt_varus_or_valgus=ifelse(injury_type=="ankle",twelve_month_ankle_talar_tilt_varus_or_valgus,NA)) %>%
    select(study_id, twelve_month_ankle_talar_tilt_varus_or_valgus)
  
  return(twelve_month_ankle_talar_tilt_varus_or_valgus)
}



#' pasteur twelve_month_ankle_sagital_displacement
#'
#' @description twelve_month_ankle_sagital_displacement function imports the injury_type construct, considers only the ankle injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_ankle_sagital_displacement()
#' }
pasteur_twelve_month_ankle_sagital_displacement <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_ank_sagdis'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_ankle_sagital_displacement <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_ankle_sagital_displacement=as.numeric(cfu_xray_ank_sagdis))
  twelve_month_ankle_sagital_displacement <- full_join(injury_type, twelve_month_ankle_sagital_displacement) %>% mutate(twelve_month_ankle_sagital_displacement=ifelse(injury_type=="ankle",twelve_month_ankle_sagital_displacement,NA)) %>%
    select(study_id, twelve_month_ankle_sagital_displacement)
  
  return(twelve_month_ankle_sagital_displacement)
}



#' pasteur twelve_month_plateau_condylar_width
#'
#' @description twelve_month_plateau_condylar_width function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_condylar_width()
#' }
pasteur_twelve_month_plateau_condylar_width <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_con'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_condylar_width <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_condylar_width=as.numeric(cfu_xray_tib_con))
  twelve_month_plateau_condylar_width <- full_join(injury_type, twelve_month_plateau_condylar_width) %>% mutate(twelve_month_plateau_condylar_width=ifelse(injury_type=="plateau",twelve_month_plateau_condylar_width,NA)) %>%
    select(study_id, twelve_month_plateau_condylar_width)
  
  return(twelve_month_plateau_condylar_width)
}



#' pasteur twelve_month_plateau_medial_and_lateral_difference
#'
#' @description twelve_month_plateau_medial_and_lateral_difference function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_medial_and_lateral_difference()
#' }
pasteur_twelve_month_plateau_medial_and_lateral_difference <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_med_lat','cfu_xray_tib_med_lat_sp'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_medial_and_lateral_difference <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_medial_and_lateral_difference=paste(ifelse(cfu_xray_tib_med_lat_sp=="1","Medial",ifelse(cfu_xray_tib_med_lat_sp=="2","Lateral",NA)),as.numeric(cfu_xray_tib_med_lat)))
  twelve_month_plateau_medial_and_lateral_difference <- full_join(injury_type, twelve_month_plateau_medial_and_lateral_difference) %>% mutate(twelve_month_plateau_medial_and_lateral_difference=ifelse(injury_type=="plateau",twelve_month_plateau_medial_and_lateral_difference,NA)) %>%
    select(study_id, twelve_month_plateau_medial_and_lateral_difference)
  
  return(twelve_month_plateau_medial_and_lateral_difference)
}



#' pasteur twelve_month_plateau_angle_between_femur_and_tibia
#'
#' @description twelve_month_plateau_angle_between_femur_and_tibia function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_angle_between_femur_and_tibia()
#' }
pasteur_twelve_month_plateau_angle_between_femur_and_tibia <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_fem_tib'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_angle_between_femur_and_tibia <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_angle_between_femur_and_tibia=as.numeric(cfu_xray_tib_fem_tib))
  twelve_month_plateau_angle_between_femur_and_tibia <- full_join(injury_type, twelve_month_plateau_angle_between_femur_and_tibia) %>% mutate(twelve_month_plateau_angle_between_femur_and_tibia=ifelse(injury_type=="plateau",twelve_month_plateau_angle_between_femur_and_tibia,NA)) %>%
    select(study_id, twelve_month_plateau_angle_between_femur_and_tibia)
  
  return(twelve_month_plateau_angle_between_femur_and_tibia)
}



#' pasteur twelve_month_plateau_medial_proximal_tibia_angle
#'
#' @description twelve_month_plateau_medial_proximal_tibia_angle function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_medial_proximal_tibia_angle()
#' }
pasteur_twelve_month_plateau_medial_proximal_tibia_angle <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_ang'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_medial_proximal_tibia_angle <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_medial_proximal_tibia_angle=as.numeric(cfu_xray_tib_ang))
  twelve_month_plateau_medial_proximal_tibia_angle <- full_join(injury_type, twelve_month_plateau_medial_proximal_tibia_angle) %>% mutate(twelve_month_plateau_medial_proximal_tibia_angle=ifelse(injury_type=="plateau",twelve_month_plateau_medial_proximal_tibia_angle,NA)) %>%
    select(study_id, twelve_month_plateau_medial_proximal_tibia_angle)
  
  return(twelve_month_plateau_medial_proximal_tibia_angle)
}



#' pasteur twelve_month_plateau_sagittal_plane_alignment
#'
#' @description twelve_month_plateau_sagittal_plane_alignment function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_sagittal_plane_alignment()
#' }
pasteur_twelve_month_plateau_sagittal_plane_alignment <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_sag_plane','cfu_xray_sag_plane_sp'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_sagittal_plane_alignment <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1' & !is.na(cfu_xray_sag_plane_sp))%>%
    mutate(twelve_month_plateau_sagittal_plane_alignment=paste(ifelse(cfu_xray_sag_plane_sp=="1","Posterior",ifelse(cfu_xray_sag_plane_sp=="2","Anterior",NA)),as.numeric(cfu_xray_sag_plane)))
  twelve_month_plateau_sagittal_plane_alignment <- full_join(injury_type, twelve_month_plateau_sagittal_plane_alignment) %>% mutate(twelve_month_plateau_sagittal_plane_alignment=ifelse(injury_type=="plateau",twelve_month_plateau_sagittal_plane_alignment,NA)) %>%
    select(study_id, twelve_month_plateau_sagittal_plane_alignment)
  
  return(twelve_month_plateau_sagittal_plane_alignment)
}



#' pasteur twelve_month_plateau_articular_step_off_lateral
#'
#' @description twelve_month_plateau_articular_step_off_lateral function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_articular_step_off_lateral()
#' }
pasteur_twelve_month_plateau_articular_step_off_lateral <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_lat'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_articular_step_off_lateral <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_articular_step_off_lateral=cfu_xray_tib_art_lat=="1")
  twelve_month_plateau_articular_step_off_lateral <- full_join(injury_type, twelve_month_plateau_articular_step_off_lateral) %>% mutate(twelve_month_plateau_articular_step_off_lateral=ifelse(injury_type=="plateau",twelve_month_plateau_articular_step_off_lateral,NA)) %>%
    select(study_id, twelve_month_plateau_articular_step_off_lateral)
  
  return(twelve_month_plateau_articular_step_off_lateral)
}



#' pasteur twelve_month_plateau_articular_step_off_medial
#'
#' @description twelve_month_plateau_articular_step_off_medial function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_articular_step_off_medial()
#' }
pasteur_twelve_month_plateau_articular_step_off_medial <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_art_ap'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_articular_step_off_medial <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_articular_step_off_medial=cfu_xray_tib_art_ap=="1")
  twelve_month_plateau_articular_step_off_medial <- full_join(injury_type, twelve_month_plateau_articular_step_off_medial) %>% mutate(twelve_month_plateau_articular_step_off_medial=ifelse(injury_type=="plateau",twelve_month_plateau_articular_step_off_medial,NA)) %>%
    select(study_id, twelve_month_plateau_articular_step_off_medial)
  
  return(twelve_month_plateau_articular_step_off_medial)
}



#' pasteur twelve_month_plateau_patella_centered
#'
#' @description twelve_month_plateau_patella_centered function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_patella_centered()
#' }
pasteur_twelve_month_plateau_patella_centered <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_tib_pat'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_patella_centered <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_patella_centered=cfu_xray_tib_pat=="1")
  twelve_month_plateau_patella_centered <- full_join(injury_type, twelve_month_plateau_patella_centered) %>% mutate(twelve_month_plateau_patella_centered=ifelse(injury_type=="plateau",twelve_month_plateau_patella_centered,NA)) %>%
    select(study_id, twelve_month_plateau_patella_centered)
  
  return(twelve_month_plateau_patella_centered)
}



#' pasteur twelve_month_plateau_tib_fib_overlap
#'
#' @description twelve_month_plateau_tib_fib_overlap function imports the injury_type construct, considers only the plateau injury participants, and then looks at a data point from a particular field in the followup from
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_twelve_month_plateau_tib_fib_overlap()
#' }
pasteur_twelve_month_plateau_tib_fib_overlap <- function(){
  data  <- get_data(c('study_id', 'redcap_event_name', 'cfu_xray_fib_overlap'))
  injury_type <- get_construct_output('injury_type')
  twelve_month_plateau_tib_fib_overlap <- data %>%
    filter(redcap_event_name == '12_month_followup_arm_1')%>%
    mutate(twelve_month_plateau_tib_fib_overlap=as.numeric(cfu_xray_fib_overlap))
  twelve_month_plateau_tib_fib_overlap <- full_join(injury_type, twelve_month_plateau_tib_fib_overlap) %>% mutate(twelve_month_plateau_tib_fib_overlap=ifelse(injury_type=="plateau",twelve_month_plateau_tib_fib_overlap,NA)) %>%
    select(study_id, twelve_month_plateau_tib_fib_overlap)
  
  return(twelve_month_plateau_tib_fib_overlap)
}



#' pasteur complication_data
#'
#' @description complication_data function in here we would write a detailed description of this function - this is part of Jyoti's homework
#' 
#' UNZIPPING INSTRUCTIONS:
#' columns:"redcap_event_name", "visit_date", "complications", 
#' "first_complication_note", "another_complication_note", "diagnosis_date", 
#' "relatedness", "severity", "treatment_related", "new_or_previous_diagnosis", "form_notes"
#' column sep:"|"
#' row sep:";new_row: "	
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pasteur_complication_data()
#' }
pasteur_complication_data <- function(){
  raw_data <- get_data(c("study_id", "redcap_event_name", "cfu_visit_dt", "cfu_visit", 
                         "cfu_comp", "cfu_comp_prompt___1", "cfu_comp_prompt___2", "cfu_comp_prompt___3", 
                         "cfu_comp_prompt___4", "cfu_comp_prompt___5", "cfu_comp_prompt___6", 
                         "cfu_comp_prompt___13", "cfu_comp_prompt___7", "cfu_comp_prompt___8", 
                         "cfu_comp_prompt___9", "cfu_comp_prompt___10", "cfu_comp_prompt___11", 
                         "cfu_comp_prompt___12", "cfu_comp_prompt___20", "cfu_comp_prompt___501", 
                         "cfu_comp_prompt___502", "cfu_comp_prompt___503", "cfu_comp_prompt___504", 
                         "cfu_comp_head1_doi", "cfu_comp_dd1", "cfu_comp_rel1", "cfu_comp_sev1", 
                         "cfu_comp_tx1", "cfu_comp_dx1", "cfu_comp_head2_doi", "cfu_comp_dd2", 
                         "cfu_comp_rel2", "cfu_comp_sev2", "cfu_comp_tx2", "cfu_comp_dx2", 
                         "cfu_comp_head3_doi", "cfu_comp_dd3", "cfu_comp_rel3", "cfu_comp_sev3", 
                         "cfu_comp_tx3", "cfu_comp_dx3", "cfu_comp_head4_doi", "cfu_comp_dd4", 
                         "cfu_comp_rel4", "cfu_comp_sev4", "cfu_comp_tx4", "cfu_comp_dx4", 
                         "cfu_comp_dd5", "cfu_comp_rel5", "cfu_comp_sev5", "cfu_comp_tx5", 
                         "cfu_comp_dx5", "cfu_comp_rad5_1", "cfu_comp_rad5_2", "cfu_comp_rad5_3", 
                         "cfu_comp_dd6", "cfu_comp_rel6", "cfu_comp_sev6", "cfu_comp_tx6", 
                         "cfu_comp_dx6", "cfu_comp_dd13", "cfu_comp_rel13", "cfu_comp_sev13", 
                         "cfu_comp_tx13", "cfu_comp_dx13", "cfu_comp_dd7", "cfu_comp_rel7", 
                         "cfu_comp_sev7", "cfu_comp_tx7", "cfu_comp_dx7", "cfu_comp_dd8", 
                         "cfu_comp_rel8", "cfu_comp_sev8", "cfu_comp_tx8", "cfu_comp_dx8", 
                         "cfu_comp_dd9", "cfu_comp_rel9", "cfu_comp_sev9", "cfu_comp_tx9", 
                         "cfu_comp_dx9", "cfu_comp_dd10", "cfu_comp_rel10", "cfu_comp_sev10", 
                         "cfu_comp_tx10", "cfu_comp_dx10", "cfu_comp_dd11", "cfu_comp_rel11", 
                         "cfu_comp_sev11", "cfu_comp_tx11", "cfu_comp_dx11", "cfu_comp_dd12", 
                         "cfu_comp_rel12", "cfu_comp_sev12", "cfu_comp_tx12", "cfu_comp_dx12", 
                         "cfu_comp_dd20", "cfu_comp_rel20", "cfu_comp_sev20", "cfu_comp_tx20", 
                         "cfu_comp_dx20", "cfu_comp_o501", "cfu_comp_dd501", "cfu_comp_rel501", 
                         "cfu_comp_sev501", "cfu_comp_tx501", "cfu_comp_dx501", "cfu_comp_o502", 
                         "cfu_comp_dd502", "cfu_comp_rel502", "cfu_comp_sev502", "cfu_comp_tx502", 
                         "cfu_comp_dx502", "cfu_comp_o503", "cfu_comp_dd503", "cfu_comp_rel503", 
                         "cfu_comp_sev503", "cfu_comp_tx503", "cfu_comp_dx503", "cfu_comp_o504", 
                         "cfu_comp_dd504", "cfu_comp_rel504", "cfu_comp_sev504", "cfu_comp_tx504", 
                         "cfu_comp_dx504", "crf09_notes"))
  
  
  complications_check_boxes_and_other_cols <- raw_data %>% 
    filter(cfu_comp_prompt___1 == "1" | cfu_comp_prompt___2 == "1" | cfu_comp_prompt___3 == "1" | 
             cfu_comp_prompt___4 == "1" | cfu_comp_prompt___5 == "1" | cfu_comp_prompt___6 == "1" | 
             cfu_comp_prompt___7 == "1" | cfu_comp_prompt___8 == "1" | cfu_comp_prompt___9 == "1" |
             cfu_comp_prompt___10 == "1" | cfu_comp_prompt___11 == "1" | cfu_comp_prompt___12 == "1" | 
             cfu_comp_prompt___13 == "1" | cfu_comp_prompt___20 == "1" | cfu_comp_prompt___501 == "1" | 
             cfu_comp_prompt___502 == "1" | cfu_comp_prompt___503 == "1" | cfu_comp_prompt___504 == "1") %>% 
    pivot_longer(starts_with("cfu_comp_prompt___"), 
                 names_to = "complications_box_number", 
                 values_to = "presence_of_complication", 
                 values_drop_na = TRUE) %>% 
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_prompt___")) %>% 
    filter(presence_of_complication == 1) %>% 
    mutate(complications = recode(complications_box_number, 
                                  "1" = "Superficial-infection", 
                                  "2" = "Deep-Infection",
                                  "3" = "Deep-Infection, Not Involving Bone", 
                                  "4" = "Deep-Infection, Septic Joint",
                                  "5" = "Non-Union", 
                                  "6" = "Malunion",
                                  "7" = "Loss of limb/amputation", 
                                  "8" = "Fixation failure",
                                  "9" = "Peri-implant Fracture", 
                                  "10" = "Reaction to Hardware",
                                  "11" = "Wound Dehiscence", 
                                  "12" = "Wound Seroma/Hematoma",
                                  "13" = "Flap failure", 
                                  "20" = "Tendon Rupture",
                                  "501" = "Other", 
                                  "502" = "Other",
                                  "503" = "Other",
                                  "504" = "Other" )) %>% 
    rename(first_complication_note = cfu_comp_o501, 
           another_complication_note =  cfu_comp_o502, 
           form_notes = crf09_notes, 
           visit_date = cfu_visit_dt) %>% 
    select(study_id, redcap_event_name, complications_box_number, 
           complications, first_complication_note, another_complication_note,
           form_notes, visit_date)
  
  
  complications_dates <- raw_data %>% 
    pivot_longer(cols = starts_with("cfu_comp_dd"), 
                 names_to = "complications_box_number", 
                 values_to = "diagnosis_date",
                 values_drop_na = TRUE) %>% 
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_dd")) %>% 
    select(study_id, redcap_event_name, complications_box_number, diagnosis_date)
  
  
  complications_relatedness <- raw_data %>% 
    pivot_longer(cols = starts_with("cfu_comp_rel"), 
                 names_to = "complications_box_number", 
                 values_to = "relatedness",
                 values_drop_na = TRUE) %>% 
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_rel")) %>% 
    mutate(relatedness = recode(relatedness, "1" = "Definitely related",
                                "2" = "Probably related",
                                "3" = "Possibly related",
                                "4" = "Unlikely related",
                                "5" = "Unrelated",
                                "6" = "Don't know")) %>% 
    select(study_id, redcap_event_name, complications_box_number, relatedness)
  
  
  complications_severity <- raw_data %>% 
    pivot_longer(cols = starts_with("cfu_comp_sev"), 
                 names_to = "complications_box_number", 
                 values_to = "severity",
                 values_drop_na = TRUE) %>%
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_sev")) %>% 
    mutate(severity = recode(severity, "1" = "Mild",
                             "2" = "Moderate",
                             "3" = "Severe and Undesirable",
                             "4" = "Life-threatening or disabling",
                             "5" = "Fatal")) %>% 
    select(study_id, redcap_event_name, complications_box_number, severity)
  
  
  complications_treatment <- raw_data %>% 
    pivot_longer(cols = starts_with("cfu_comp_tx"), 
                 names_to = "complications_box_number", 
                 values_to = "treatment_related",
                 values_drop_na = TRUE) %>% 
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_tx")) %>% 
    mutate(treatment_related = recode(treatment_related, "1" = "Operative",
                                      "2" = "Non-operative",
                                      "3" = "No treatment")) %>% 
    select(study_id, redcap_event_name, complications_box_number, treatment_related)
    
  
  complication_newness <- raw_data %>% 
    pivot_longer(cols = starts_with("cfu_comp_dx"), 
                 names_to = "complications_box_number", 
                 values_to = "new_or_previous_diagnosis",
                 values_drop_na = TRUE) %>%
    distinct(study_id, redcap_event_name, cfu_visit_dt, .keep_all = TRUE) %>% 
    mutate(complications_box_number = str_remove(complications_box_number, "^cfu_comp_dx")) %>% 
    mutate(new_or_previous_diagnosis = recode(new_or_previous_diagnosis, "1" = "New",
                              "2" = "Previous")) %>% 
    select(study_id, redcap_event_name, complications_box_number, new_or_previous_diagnosis)

  df_joined <- complications_check_boxes_and_other_cols %>% 
    full_join(complications_dates) %>% 
    full_join(complications_relatedness) %>% 
    full_join(complications_severity) %>% 
    full_join(complications_treatment) %>% 
    full_join(complication_newness)
  
  complications_zipped <- df_joined %>% 
    unite(complication_data, all_of(c("redcap_event_name", "visit_date", "complications", 
                                      "first_complication_note", "another_complication_note", "diagnosis_date", 
                                      "relatedness", "severity", "treatment_related", "new_or_previous_diagnosis", "form_notes")), sep='|') %>% 
    group_by(study_id) %>% 
    summarise(complication_data=paste(complication_data, collapse = ";new_row: "))
  
  enrolled_df <- get_construct_output("enrolled")
  
  complications_output <- left_join(enrolled_df, complications_zipped) %>% 
    mutate(complication_data = ifelse(enrolled == TRUE, complication_data, NA)) %>% 
    select(-enrolled)
  return(complications_output)
}





