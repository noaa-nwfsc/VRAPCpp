#' @title SetOutFileNames
#' @description Set output file names
#' @details Used by GetOutFiles and also by GetCommandLine
#' @param BaseName The basename for the .sum, .byr, and .esc output files
#' @param inputs Inputs from .rav file
#' @param PathName Path for the files
#' @return Updated inputs list with full names for the output files
SetOutFileNames = function(BaseName, inputs, PathName=NULL){
  # SET OUTPUT FILES
  inputs$OutFileSum = paste(PathName,BaseName,".sum",sep="")
  inputs$OutFileEsc = paste(PathName,BaseName,".esc",sep="")
  inputs$OutFileByr = paste(PathName,BaseName,".byr",sep="")
  inputs$OutFileLog = paste(PathName,BaseName,".log",sep="")
  inputs$InFileCopy = paste(PathName,BaseName,".rav",sep="")
  inputs$OutFileReport = paste(PathName,BaseName,".pdf",sep="")
  return(inputs)
}