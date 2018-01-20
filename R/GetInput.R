#' @title GetInput
#' @description Read in an input file and assign all the variables
#' @param InFile the name of the input file
#' @return Returns the list of all inputs
GetInput = function(InFile){
  require(stringr)
  filext = str_split(InFile,"[.]")[[1]]
  filext = filext[length(filext)]
  if(!(filext %in% c("rav", "rap"))) stop("GetInput VRAP 2.0 requires either a .rav or .rap input file.")
  if(filext=="rav") inputs = ReadRavFile(InFile)
  if(filext=="rap") inputs = ReadRapFile(InFile)
  inputs$InFile = InFile
  return(inputs)
}
