# modify dq data so that it has the issue in it

base_dq_data <- base_dq_data %>%
  mutate(RelationshipToHoH = if_else(HouseholdID == "h_648417" &
                                       RelationshipToHoH == 1,
                                     NA, RelationshipToHoH)) 

