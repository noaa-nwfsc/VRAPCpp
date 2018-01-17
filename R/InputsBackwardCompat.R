#' @title Backwards Compatibility for inputs List
#' @description Ensure that inputs list can be used in VRAP 1.0 functions
#' @param inputs A list of the necessary input values (can be taken from .rav file).
#' @return Returns the list of all inputs with added values so that list is compatible with VRAP 1.0 functions
#' @details The VRAP 2.0 inputs list is very similar to VRAP 1.0 but has a few differences related to how ER and Pop steps are named.  This creates entries in inputs that has the same names used in VRAP 1.0 so that the inputs list in VRAP 2.0 can be passed to VRAP 1.0 functions.

InputsBackwardCompat = function(inputs){
  inputs$NumBreakPoints = 0
  inputs$BaseRegime = 1 
  inputs$TargetU = 0.67 #dummy value req for backwards compat; not used

  inputs$BufferStep = inputs$StepSize/ifelse(inputs$StepFunc=="ER",inputs$TargetU,inputs$cap)
  inputs$BufferStart = inputs$StepStart/ifelse(inputs$StepFunc=="ER",inputs$TargetU,inputs$cap) 
  inputs$BufferEnd = inputs$StepEnd/ifelse(inputs$StepFunc=="ER",inputs$TargetU,inputs$cap)
  #integer of the number of steps of ER or Pop Capacity to do  # is 1:BufMax
  inputs$BufMax = round((inputs$BufferEnd - inputs$BufferStart) / inputs$BufferStep + 1)
  return(inputs)
}
