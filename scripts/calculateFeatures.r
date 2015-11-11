#Calculate the plan features

calculateFeatures <- function(plan){
  
  #colnames(plan) <- c("MLCIndex","CP","Planned","Delivered")
  plan$MLCIndex <- as.character(plan$MLCIndex)
  plan$CP <- as.numeric(plan$CP)
  plan$Planned <- as.numeric(plan$Planned)
  plan$Delivered <- as.numeric(plan$Delivered)
  # --- PlanName --- #
  # Split filename
  #plan$PlanName <- deparse(substitute(plan))
  
  # --- Error Magnitude --- #
  plan$Error <- plan$Planned - plan$Delivered
  
  # --- Velocity @ CP --- #
  #Break MLC Index into bank and index to allow easy sorting
  plan <- tidyr::separate(plan, MLCIndex, into=c("Bank","MLCIndex"), 1)
  plan$MLCIndex <- as.numeric(plan$MLCIndex)
  #levels(plan$MLCIndex) <- c(paste0("B", 1:60), paste0("A", 1:60))
  plan <- dplyr::arrange(plan, MLCIndex, desc(Bank))

  time = 0.424
  #Make a column that is the position of the leaf at the previous CP
  plan$POSShift <- c(plan$Planned[1], plan$Planned[-length(plan$Planned)])
  
  # Subtract POSShift from Planned and divide by time, if it is a first CP then Velocity must be 0 
  plan$Velocity <- ifelse(plan$CP == 1 | plan$CP == 179, 0, (plan$Planned - plan$POSShift)/time)
  
  # --- Acceleration --- #
  
  plan$VELShift <- c(plan$Velocity[1], plan$Velocity[-length(plan$Velocity)])
  plan$Acceleration <- ifelse(plan$CP == "CP1" | plan$CP == "CP179", 0, (plan$Velocity - plan$VELShift)/time)
  plan$VELNext <- c(plan$Velocity[-1], plan$Velocity[length(plan$Velocity)])
  
  # --- Push Pull --- #
  # This category defines whether the leaf is moving towards or away from the 
  # center of the MLC
  
  # The %>% is the 'pipe' operator from dplyr/magrittr.
  plan <- plan %>%
    mutate(PushPull = ifelse(Planned - POSShift < 0 & Bank == "A", "Pull",
                             ifelse(Planned - POSShift < 0 & Bank == "B", "Push",       
                                    ifelse(Planned - POSShift > 0 & Bank == "A", "Push",
                                           ifelse(Planned - POSShift > 0 & Bank == "B", "Pull", "Rest")))))
  
  # --- RestStop --- #
  # This category defines how the leaf is moving
  plan <- plan %>%
    mutate(RestStop = ifelse(abs(Velocity) > 0 & abs(VELShift) > 0 & abs(VELNext) > 0, "Moving",
                             ifelse(abs(Velocity) > 0 & VELShift == 0 & VELNext == 0, "Single CP", 
                                    ifelse(abs(Velocity) > 0 & abs(VELShift) > 0 & VELNext == 0, "Stopping",
                                           ifelse(abs(Velocity) > 0 & VELShift == 0 & abs(VELNext) > 0, "Starting", 
                                                  ifelse(Velocity == 0 & VELShift == 0 & VELNext == 0, "Rest", "Other"))))))
  
  # --- CPcat --- #
  plan <- plan %>% 
    dplyr::mutate(CPcat = ifelse(CP %in% c(1, 2, 179, 180), "First CP",
                          ifelse(CP %in% c(177, 178, 355, 356), "Last CP", "Other")))
  
  # --- Clean up --- #
  #get rid of helper columns
  plan <- dplyr::select(plan, -POSShift, -VELShift, -VELNext)
  
    # --- Save File --- #

  plan$MLCIndex <- as.numeric(plan$MLCIndex)
  plan$CP <- as.numeric(plan$CP)
  plan$Planned <- as.numeric(plan$Planned)
  plan$Delivered <- as.numeric(plan$Delivered)
  
  return(plan)
  
}