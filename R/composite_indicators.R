###############################################################################

# creating composite indicators -----------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df |> 
    dplyr::mutate(
          
           i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                                 hh_size <= 6 ~ "between_4_and_6_members",
                                 hh_size <= 9 ~ "between_7_and_9_members",
                                 hh_size >= 10 ~ "10_or_more_members"),
           
           i.hoh_no = case_when(hoh_no <= 24 ~ "age_18_24",
                                        hoh_no <= 39 ~ "age_25_39",
                                        hoh_no <= 59 ~ "age_40_59",
                                        hoh_no > 59 ~ "age_60+"),
           
           i.hh_age = case_when(hh_age <= 24 ~ "age_18_24",
                               hh_age <= 39 ~ "age_25_39",
                               hh_age <= 59 ~ "age_40_59",
                               hh_age > 59 ~ "age_60+"))|> 
    select(-c(starts_with("int.")))
}




###############################################################################