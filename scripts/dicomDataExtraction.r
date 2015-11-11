#DICOM Position Extraction
dicom_data<-function(patient){

    #values of interest to extract    
    gantry.angle<-extractHeader(patient$hdr, "GantryAngle")
    gantry.direction<-extractHeader(patient$hdr, "GantryRotationDirection", numeric=FALSE)
    patient.id<-extractHeader(patient$hdr, "PatientID")
    prescribed.dose<-extractHeader(patient$hdr, "TargetPrescriptionDose")
    beam.energy<-extractHeader(patient$hdr, "NominalBeamEnergy")
    num.points<-extractHeader(patient$hdr,"NumberOfControlPoints")
    beam.dose<-extractHeader(patient$hdr, "BeamDose")
    cps<-extractHeader(patient$hdr, "LeafJawPositions", numeric=FALSE)
    #Remove the 4 entries corresponding to MLC initial positions
    cps<-cps[-c(1,2, num.points[1]+3, num.points[1]+4)]
    
    beam.dose <- c(rep(beam.dose[1], num.points[1]), rep(beam.dose[2], num.points[2]))

    #Cleanup and save  
    dat <- data.frame(patient.id,beam.energy,gantry.angle, gantry.direction,beam.dose,cps)
    colnames(dat)<- c("ID", "Energy", "GantryAngle", "GantryDirection", "BeamDose", "ControlPoints") 
    return(dat)
  
}    