#' @title GetInput
#' @description Read in an input file and assign all the variables
#' @param InFile the name of the input file
#' @return Returns the list of all inputs
GetInput = function(InFile){
  require(stringr)
  postfix = str_split(InFile,"[.]")[[1]]
  postfix = postfix[length(postfix)]
  if(!(postfix %in% c("rav", "rap"))) stop("GetInput VRAP 2.0 requires either a .rav or .rap input file.")
  if(postfix=="rav") inputs = ReadRavFile(InFile)
  if(postfix=="rap") inputs = ReadRapFile(InFile)
  inputs$InFile = InFile
}
