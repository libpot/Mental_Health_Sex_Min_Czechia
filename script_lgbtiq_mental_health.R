data_2022w_mod <- data_2022w %>% 
  mutate(lgbtiq_category = case_when(sex %in% c("female", "male") & gender == "women" & sexual_orientation == "lesbian" ~ "L_women",
                                     sex %in% c("female", "male") & gender == "men" & sexual_orientation == "gay" ~ "G_men",
                                     sex %in% c("female", "male") & gender == "women" & sexual_orientation == "bisexual" ~ "B_women",
                                     sex %in% c("female", "male") & gender == "men" & sexual_orientation == "bisexual" ~ "B_men",
                                     sex %in% c("female", "male") & gender == "women" & sexual_orientation == "other" ~ "divers_women",
                                     sex %in% c("female", "male") & gender == "men" & sexual_orientation == "other" ~ "divers_men",
                                     sex %in% c("female", "male") & gender == "women" & sexual_orientation == "heterosexual" ~ "H_women",
                                     sex %in% c("female", "male") & gender == "men" & sexual_orientation == "heterosexual" ~ "H_men"))