#' @title Write a report
#' @description Create a pdf with basic information about the VRAP output. 
#' @details knit2pdf is used to create the pdf using Report-knitr-ER.xRnw or Report-knitr-Pop.xRnw (Sweave files) in inst/doc.
#' @param InFile the .rav input file.
#' @param OutFileBase If OutFileBase is NULL, the VRAP output files are assumed to be in the same directory as InFile and named InFile.sum, InFile.byr, InFile.esc.  Thus they have the same basename.  If this is not the case, then OutFileBase can be passed in.
#' @param show.file Whether to open the pdf after it is produced.
#' @return Nothing. The pdf is made and saved.
WriteReport=function(InFile=NULL, OutFileBase=NULL, show.file=FALSE){
  
  
  #make sure InFile is a full path name
  InFile = normalizePath(InFile, winslash="/")
  
  #READ INPUT DATA AND CALCULATE AEQs
  #direct from .rav file or simple calculation from rav file inputs
  inputs=GetInput(InFile)
  if(is.null(OutFileBase)){
    tmp=strsplit(InFile,"\\\\")[[1]]
    InFileBase=tmp[length(tmp)]
    tmp=strsplit(InFileBase,"/")[[1]]
    InFileBase=tmp[length(tmp)]   
    if(str_detect(InFileBase,"[.]")){
      OutFileBase=strsplit(InFileBase,"[.]")[[1]][1]; 
    }else{
      OutFileBase=InFileBase;
    }
  }
  
  #add the output file names to the inputs
  inputs = SetOutFileNames(OutFileBase, inputs, PathName=paste(getwd(),"/",sep=""))
  
  output.file = inputs$OutFileReport
  
  
  if(str_sub(output.file,-4)==".pdf") output.file=str_sub(inputs$OutFileReport,1,-5)
  pkgpath <- find.package("VRAP")
  if(inputs$StepFunc=="ER") path=file.path(pkgpath, "doc", "Report-knitr-ER.xRnw")  
  if(inputs$StepFunc=="POP") path=file.path(pkgpath, "doc", "Report-knitr-Pop.xRnw")  
  knit2pdf(path, output=paste(output.file,".tex",sep=""), envir=sys.nframe(), quiet=TRUE)
  if(show.file) file.show(paste(output.file,".pdf",sep=""))
  
}