combinePlannedAndDelivered <- function(planned, delivered){
  planned$CP <- 1:356
  delivered$CP <- 1:356
  
  planned <- select(planned, -c(ID, Energy,GantryAngle,GantryDirection,BeamDose))  %>% 
    separate(ControlPoints, into=c(paste0("B", 1:60), paste0("A", 1:60)), " ") %>%
    melt(id.vars="CP", variable.name = "MLCIndex", value.name="Planned")
  delivered <- select(delivered, -c(ID, Energy,GantryAngle,GantryDirection,BeamDose))  %>% 
    separate(ControlPoints, into=c(paste0("B", 1:60), paste0("A", 1:60)), " ") %>%
    melt(id.vars="CP", variable.name = "MLCIndex", value.name="Delivered")
  
  dat <- data.frame("MLCIndex" = planned$MLCIndex, "CP"=planned$CP, "Planned"=planned$Planned, "Delivered"=delivered$Delivered, stringsAsFactors=FALSE)
  
  return(dat)
}