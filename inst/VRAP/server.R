.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
require(VRAPS, quietly=TRUE)
require(shiny, quietly=TRUE) #0.6.0
require(shinyAce, quietly=TRUE)

## if (file.exists("parcores.R")) {
##   source("parcores.R")
## } else {
##   parcores <- function() { 1 }
## }

## flag for displaying BYr results or not
## make sure that the help_main.html file is updated
## using filter_html.R to agree with this setting
INCLUDEBYR <- FALSE

## texts of user alerts and errors
VIRUSDETECTIONMSG <- paste("<b>The uploaded file has triggered",
                           "a virus detection.</br>",
                           "Please contact a",
                           "system administrator.</b>")
UPLOADERRORMSG <- paste("<b>An error occurred during the file upload.",
                        "Please try again.</br>",
                        "If the error persists,",
                        "contact a system administrator.</b>")
WAITSCANNINGMSG <- "Scanning uploaded file - please wait."
WAITESTIMATINGMSG <- "Estimating run time - please wait."

PROCESSINGMSG <- paste("Calculating - please wait.",
                       "</br>",
                       "<em>Close browser window to halt processing</em>")

## HTML help contents
RESULTSHELP <- HTML(includeHTML("html/help_results.html"))
DOWNLOADSHELP <- HTML(includeHTML("html/help_downloads.html"))

## system command to invoke clamdscan
SCANCMD <- "clamdscan --fdpass --remove"

## TEMPORARY SETTING FOR TESTING ONLY!!
SCANCMD <- "true"

## (relative) path to demo files
DEMOFILESPATH <- "demofiles"

## outputfile prefix
OUTFILEBASENAME <- "VRAPresults"

## Attach timestamp to end of filename.
## (filename should not include extension)
appendTimestamp <- function(filename) {
  paste0(filename,"_",strftime(Sys.time(),"%Y%m%d_%H%M%S"))
}

## Ensure that the file located at path has the same extension
## as in filename.
## If filename contains no extension, do nothing.
## - Intended to make uploaded, tmp file names consistent with
## VRAPS::GetInput() extension checks.
ensureExtension <- function(filename, path) {
  if (is.null(filename) || is.null(path) ||
        (nchar(filename) == 0) || (nchar(path) == 0)) {
    return()
  }
  
  newpath <- path
  filenamePieces <- str_split(filename, "\\.")[[1]]
  if (length(filenamePieces) > 1) {
    ## filename has an extension
    targetExt <- filenamePieces[length(filenamePieces)];
    pathPieces <- str_split(path, "\\.")[[1]]
    pplen <- length(pathPieces)
    if (pplen > 1) {
      ## path has an extension
      pathExt <- pathPieces[pplen]
      if (pathExt != targetExt) {
        pathPieces[pplen] <- targetExt
        newpath <- paste0(pathPieces,collapse='.')
      }
    } else {
      newpath <- paste(path,targetExt,sep='.')
    }
  }
  if (path != newpath) {
    file.rename(path, newpath)
  }
  newpath
}

## Extract timestamp from timestamped filename.
## filename_20161230_111111[.txt]
extractTimestamp <- function(filename) {
  basename <- stringr::str_replace(filename,"\\.[^.]*$","")
  stringr::str_extract(basename,"\\d{8}_\\d{6}$")
}

## Remove files and directories generated during knitr processing.
cleanupKnitr <- function(knitrdir=".") {
  file.remove(Sys.glob(c("*.tex","*.aux","*.log")))
  unlink("figure",recursive=TRUE)
}

# adjust column alignment of escapement results
fix.esc.headers <- function(thelines) {
  # requires stringr
  escindex <- grep("_*Escapement-*", thelines)
  if (length(escindex) > 0) {
    ei <- escindex[1]
    thelines[ei] <- str_replace(thelines[ei],"_*Escapement_*",
                                "_______Escapement_______")
  }

  mmindex <- grep("Min *Avg *Max", thelines)
  if (length(mmindex) > 0 ) {
    mmi <- mmindex[1]
    thelines[mmi] <- str_replace(thelines[mmi], "Years.*",
                                 " Years     Min     Avg     Max")
  }
  
  thelines
}

## ## build a vector of demo files named by title
## demo.set <- function(dir="demofiles") {
##   demos <- list()
##   files <- Sys.glob(file.path(dir, '*'))
##   for (thefile in files) {
##     fi <- file(thefile, 'r')
##     demolines <- readLines(fi, 1)
##     while (length(demolines) > 0) {
##       demoline <- trimws(demolines[1])
##       if (nchar(demoline) > 0) {
##         dname <- strsplit(demoline, ',')[[1]][1]
##         dfile <- basename(thefile)
##         demos[dname] = dfile
##         break;
##       }
##       demolines <- readLines(fi, 1)
##     }
##     close(fi)
##   }
  
##   as.vector(demos)
## }


shinyServer( function(input, output, session) {

  ## persistent state: the app must remember these values from
  ## one user interaction to the next
  
  ## number of (logical) cores available
  ## lcores <- parcores()

  cancelRavUpload <- reactive({
    tmp <- switches$resetupload
    isolate({
      switches$resetupload <- FALSE
    })
    tmp
  })

  EXAMPLES <- demo.set()
  
  ## setByrDisplay <- function(includebyr) {
  ##   if (includebyr) {

  ##   } else {
  ##     hideTab(inputId="tabset", target="resultstabbyr")
  ##   }
  ## }

  ## setByrDisplay(INCLUDEBYR)
  
  ## file uploader
  fileUpload <- callModule(consecFileUpload, "ravupload",
                           cancelRavUpload,
                           currentFileLabel="Current rav file:",
                           accept=c(".rav",".RAV",".Rav"))
  ## output directory
  ## set in getOutputDirectory()
  outputdir <- NULL   

  getOutputDirectory <- function() {
    if (is.null(outputdir)) {
      outputdir <<- tempfile("vrapout")
      dir.create(path=outputdir)
    }
    outputdir
  }

  getUploadDirectory <- function() {
    if (is.null(fileUpload())) {
      return(NULL)
    }
    dirname(fileUpload()$datapath)
  }

  ## current input
  ## set/reset in setCurrentInput(); get in getCurrentInput
  currentinputfile <- NULL   

  setCurrentInput <- function(settofile=NULL,deleteexisting=FALSE) {
    if (deleteexisting) {
      if (!is.null(currentinputfile) &&
            file.exists(currentinputfile) &&
            !grepl(DEMOFILESPATH, currentinputfile)) {
        file.remove(currentinputfile)
      }
    }
    currentinputfile <<- settofile;
    if (!is.null(currentinputfile)) {
      setRavEditor()
    }
  }

  getCurrentInput <- function() {
    return(currentinputfile);
  }
  
  
  ## origRavValues <- NULL
  ## newRavValues <- NULL
  usemodrav <- FALSE

  ## reactive triggers; most of these act as signals
  switches <- reactiveValues(resetupload=FALSE,
                             refreshresults=FALSE,
                             downloadstatus=FALSE,
                             sumOutput=RESULTSHELP,
                             byrOutput=RESULTSHELP,
                             escOutput=RESULTSHELP,
                             downloads=DOWNLOADSHELP,
                             ravparamsokay=TRUE,
                             editedravinput=FALSE
                             )
  ## reset the file upload control by
  ##    switches$resetupload = TRUE
  ## trigger a results tab refresh by
  ##    switches$refreshresults = !switches$refreshresults

  
  ## attempt to detect user platform in order to set download
  ## file format default properly.
  if (("request" %in% names(session)) &&
        ("HTTP_USER_AGENT" %in% names(session$request))) {
    osIsWindows <- grepl("windows", session$request$HTTP_USER_AGENT,
                         ignore.case=TRUE)
  } else {
    osIsWindows <- TRUE
  }

  ## virus scanner
  scanclam <- function(path) {
    if (is.null(path)) { return("SCANNOFILE") }

    returncode <- "SCANVIRUS"
    
    cmd <- paste(SCANCMD, path)
    ec <- system(cmd)
    if (0 == ec) { return("SCANOKAY")}
    else if (1 == ec) {
      if (file.exists(path)) {
        file.remove(path)
      }
    }
    else { returncode <- "SCANERROR" }

    ## at this point, something isn't right with the upload
    ## and the file upload control needs resetting

    switches$resetupload <- TRUE
    
    return(returncode)
  }

  getDownloadTempFile <- function() {
    tempfile("dltmp")
  }

  ## output path base name (i.e., without extension)
  ## set/reset in getBaseName
  baseName <- NULL

  getBaseName <- function(delete=FALSE) {
    if (delete) {
      baseName <<- NULL
    } else if (is.null(baseName)) {
      baseName <<- appendTimestamp(OUTFILEBASENAME);
    }
    return( baseName );
  }

  outputPathBase <- function() {
    return(file.path(getOutputDirectory(), getBaseName()))
  }
  
  outputPath <- function(ext) {
    return(paste0(outputPathBase(),ext))
  }
  
  inputFileDisplayName <- function() {
    demoPrefix <- if(usemodrav) {"Edited demo :"} else {"Demo :"}
    switch(inputtype(),
           demo=paste(demoPrefix,names(EXAMPLES)[EXAMPLES==input$demofile]),
           upload=inputfile()[2],
           NULL
           )

  }

  
  ## modifies lines in summary output to replace tmp file reference
  ## with name corresponding to file the user might download
  
  vrapFilenameFilter <- function(ext='') {
    filepath <- file.path(getOutputDirectory(), paste0(getBaseName(),".sum"))
    if (file.exists(filepath)) {
      thefile <- file(filepath,"r")
      lines <- readLines(thefile)
      close(thefile)
      uploadname <- inputFileDisplayName()
      lines <- lines[!grepl("^ *Copy of Input File.*",lines,perl=TRUE)]
      lines <- sub("(\\s*Input File:\\s+)     .*$",paste0("\\1",uploadname),
                   lines,perl=TRUE)
      outfile <- file(filepath,"w")
      cat(lines, file=outfile, sep="\n")
      close(outfile)
    }
  }

  triggerShinyBusy <- reactive({
    session$sendCustomMessage('setspinner','notbusy')
    return("")
  })

  triggerShinyBusy2 <- reactive({
    input$recalcButton
    return("")
  })

  output$dummy <- renderText({
    return(triggerShinyBusy())
  })

  output$dummy2 <- renderText({
    return(triggerShinyBusy2())
  })

  inputFileExists <- reactive({
    inputfile()
    curr <- getCurrentInput()
    if (!is.null(curr) && file.exists(curr)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  outputFilesExist <- function() {
    return(3<=length(Sys.glob(outputPath(".*"))))
  }
  
  clearOutputDirectory <- function(clearall=TRUE) {
    if (clearall) {
      getBaseName(delete=TRUE)
      unlink(file.path(getOutputDirectory(),"*"))
    } else {
      unlink(file.path(getOutputDirectory(),
                       c("*.byr","*.esc","*.sum","*.pdf")))
    }
    displayResultsTabs()
  }

  displayResultsTabs <- function() {
    ## if (outputFilesExist()) {
    ##   showTab(inputId="tabset", target="resultstabesc")
    ##   showTab(inputId="tabset", target="resultstabsum")
    ## } else {
    ##   hideTab(inputId="tabset", target="resultstabesc")
    ##   hideTab(inputId="tabset", target="resultstabsum")
    ## }
  }

  session$onSessionEnded(function(){
    unlink(file.path(getOutputDirectory()), recursive=TRUE)
  })

  ## output$byrresultstab <- renderUI({
  ##     tabPanel(
  ##       title="Results (.byr)",
  ##       value="resultstabbyr",
  ##       uiOutput('contentsbyr')
  ##     )
  ## })
  
  output$contentssum <- renderUI({
    switches$sumOutput
  })

  output$contentsbyr <- renderUI({
    switches$byrOutput
  })

  output$contentsesc <- renderUI({
    switches$escOutput
  })

  downloadControls <- function() {
    sel <- "win"
    if (!is.null(osIsWindows) && !osIsWindows) { sel="unix" }

    report <- if(file.exists(outputPath(".pdf"))) {
      downloadButton('downloadRpt', 'Report')
    } else {
      NULL
    }

    rav <- if (usemodrav && file.exists(getCurrentInput())) {
      downloadButton("downloadRav", "Rav")
    } else {
      NULL
    }
    
    return(
      tagList(
        tags$hr(),
        tags$h4('Download VRAPS output files'),
        selectInput("os", "Choose OS:",
                    list("Windows" = "win", 
                         "Mac/Unix" = "unix"),
                    selected=sel),
        report,
        if (INCLUDEBYR) {
          downloadButton('downloadByr', 'Byr File')
        } else {
          NULL
        },
        downloadButton('downloadEsc', 'Esc File'),
        downloadButton('downloadSum', 'Sum File'),
        rav,
        tags$hr(),
        fluidRow(
          column(
            4,
            selectInput( "file2", "Download the example .rav files:", EXAMPLES)
          ),
          column(
            3,
            div(id="exdlspacer",HTML("&nbsp;")),
            downloadButton('downloadExample', 'Download Example File')
          )
        ),
        tags$br(),
        tags$br(),
        tags$br()
      )
    )
  }


  output$contentsdownloads <- renderUI({
    switches$downloads
  })

  ## Wrap a reactive around input$type so that certain actions will
  ## be taken whenever it changes.
  ## Don't reference input$type directly elsewhere in the code.
  
  inputtype <- reactive({
    isolate({
      clearOutputDirectory()
      setCurrentInput(NULL, deleteexisting=TRUE)
      switches$downloadstatus <- !switches$downloadstatus
      switches$sumOutput <- RESULTSHELP
      switches$byrOutput <- RESULTSHELP
      switches$escOutput <- RESULTSHELP
      switches$downloads <- DOWNLOADSHELP
      switches$resetupload <- TRUE
    })
    input$type
  })

  observe({
    req(input$recalcButton)

    ## Don't do anything until after the first button push.
    isolate({
      if (input$recalcButton == 0){
        HTML("")
      ## } else if(inputtype()=="upload" && is.null(input$file1)) {
      } else if(inputtype()=="upload" && is.null(fileUpload())) {
        HTML("Select an input file")
      } else {
        session$sendCustomMessage('setspinner','busy')
        session$sendCustomMessage('setwaitmsg', PROCESSINGMSG)

         ## must modify behavior if using edited rav file
        clearOutputDirectory(clearall=FALSE)
        
        ## Note that just by virtue of checking the value
        ## of input$recalcButton, we're now going to get
        ## called whenever it is pushed.
        ## input$file1 will be NULL initially. After the
        ## user selects and uploads a file, it will be a
        ## data frame with 'name', 'size', 'type', and
        ## 'datapath' columns. The 'datapath' column will
        ## contain the local filenames where the data can
        ## be found.


        capture.output({
          tmp <- Main.VRAP1(inputfile()[1], OutFileBase=outputPathBase(),
                      NRuns=inNRuns(),
                      forceNewRav=TRUE,
                      silent=TRUE,
                      version="C")
        })
        
        output <- VRAP:::SaveSummary(tmp$inputs,
                                     tmp$SummaryStats,
                                     tmp$staticvars)


        ## create the report file
        owd <- setwd(getOutputDirectory())
        uld <- getUploadDirectory()
        if (!is.null(uld)) {
          tmpravPath <- file.path(uld,"tmprav.rav")
          if (file.exists(tmpravPath)) {
            file.rename(tmpravPath,
                        file.path(getOutputDirectory(),"tmprav.rav"))}
        }
        VRAP:::WriteReport(file.path(getOutputDirectory(),"tmprav.rav"),
                           getBaseName())
        cleanupKnitr()
        setwd(owd)
        ## done with report file

        ## adjust file name references in summary
        vrapFilenameFilter()

        ## generate HTML versions of text outputs
        switches$sumOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".sum"))))
        
        switches$byrOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".byr")), 2))

        switches$escOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".esc")), 2))

        displayResultsTabs()

        ## turn off the spinner, set statuses, and update the
        ## download controls to reflect files present
        session$sendCustomMessage('setspinner','notbusy')
        switches$downloadstatus <- !switches$downloadstatus
        switches$downloads <<- downloadControls();

        ## Open the summary results tab
        updateTabsetPanel(session, "tabset", selected="resultstabsum")
      }
    })
  })

  getoutputfile <- function(filepath) {
    singlestring <- ""

    if (file.exists(filepath)) {
      thefile <- file(filepath,"r")
      lines <- readLines(thefile)
      if (endsWith(filepath, ".esc")) {
        lines <- fix.esc.headers(lines)
      }
      close(thefile)
      singlestring <- paste0(lines,collapse="\n")
    }

    return(singlestring)
  }

  htmlize <- function(blob, padding=0) {
    thehtml <- paste(blob, collapse="")
    thehtml <- str_replace_all(thehtml,"\r",'')
    thehtml <- str_replace_all(thehtml,"<","&lt;")
    thehtml <- str_replace_all(thehtml,">","&gt;")
    thehtml <- str_replace_all(thehtml,"\n","<br/>")
    thehtml <- str_replace_all(thehtml,"[[:space:]]", "&nbsp;")
    for (i in 1:padding) {thehtml <- paste0(thehtml,"<br/>")}
    thehtml <- paste(c("<div class='tabtext' id='outputtext'>",
                       thehtml,"</font></div>"), collapse="")
    return(thehtml)
    
  }

  output$runbutton <- renderUI({
    switches$editedravinput
    inFilePath <- inputfile()[1]
    runsString <- ""
    if (!is.null(inFilePath)) {
      tmpruns = as.numeric(input$NRuns)
      if(tmpruns<0) tmpruns = VRAP:::GetInput(inFilePath)$NRuns
      runsString <- paste0("=",tmpruns)
    }
    if (usemodrav) {
      actionButton('recalcButton',
                   HTML(paste0("<b>Run VRAPS</b> with <b>edited parameters</b> ",
                               "and NRuns",runsString)))
    } else {
      actionButton('recalcButton',
                   HTML(paste0("<b>Run VRAPS</b> with selected file and NRuns",
                               runsString)))
    }
  })

  output$timest <- renderUI({
    inFilePath <- inputfile()[1]
    
    if (is.null(inFilePath) || !file.exists(inFilePath)) return(NULL)
    
    outfile = file.path(getOutputDirectory(),"vrap_timest")
    session$sendCustomMessage('setwaitmsg', WAITESTIMATINGMSG)
    capture.output({
      mainres = Main.VRAP1(inFilePath, OutFileBase=outfile, NRuns=1,
                     forceNewRav=FALSE,
                     silent=TRUE,
                     version="C")
    })

    tefiles <- Sys.glob(paste0(outfile,"*"))
    lapply(tefiles, function(x) {file.remove(x)})
    
    isolate({
      switches$sumOutput <- RESULTSHELP
      switches$byrOutput <- RESULTSHELP
      switches$escOutput <- RESULTSHELP
      switches$downloads <- DOWNLOADSHELP
      switches$downloadstatus <- !switches$downloadstatus
    })
    
    tmpruns = as.numeric(input$NRuns)
    if(tmpruns<0) tmpruns = VRAP:::GetInput(inFilePath)$NRuns

    processing.time <- mainres$time[1]
    out.time <- mainres$output.time[1]
    timest <- round(processing.time * tmpruns + out.time)

    nrunstext <- if (as.numeric(input$NRuns == -1)) { "specified in file" }
    else { paste0("= ",input$NRuns) }

    timesttext <- paste0(timest," second");
    if (timest != 1) {timesttext <- paste0(timesttext,"s")}

    if (timest < 2) {
      timesttext <- "~ 1 second"
    }

    HTML(paste("Estimated time for", inputfile()[2], " : ",
               timesttext,"<br/><br/>"))
  })

  observe({
    if(input$msgclosebtn > 0) {
      session$sendCustomMessage('clearmsg','reset')
    }
  })

  uploadedFileInput <- reactive({
    usemodrav <<- FALSE
    fileUpload()
  })

  demoFileInput <- reactive({
    inputtype()
    input$demofile
    usemodrav <<- FALSE
    if (input$demofile == "NOSEL") {
      return(NULL)
    } else {
      inFilePath <- normalizePath(file.path(DEMOFILESPATH,input$demofile))
      inFileName <- input$demofile
      list(inFileName,inFilePath)
    }
  })

  observe({
    inputtype()
    fileUpload()
    input$demofile

    isolate({
      inputfile()
    })
  })

  inputfile <- reactive({
    switches$editedravinput
    demoFileInput()
    uploadedFileInput()
    
    inFilePath <- NULL
    inFileName <- NULL

    ## clearOutputDirectory()
    
    if (usemodrav) {
      inFilePath <- getCurrentInput()
      inFileName <- basename(inFilePath)
    } else {
      clearOutputDirectory()
      setCurrentInput(inFilePath, deleteexisting=TRUE)
      
      if (inputtype() == "upload") {
        inFileName <- uploadedFileInput()$name
        testPath <- ensureExtension(inFileName, uploadedFileInput()$datapath)
        
        if (is.null(testPath) || !file.exists(testPath)) { return(NULL) }
        
        session$sendCustomMessage('setwaitmsg', WAITSCANNINGMSG)

        switch(scanclam(testPath),
               SCANNOFILE={
                 ## ignore
               },
               SCANOKAY=isolate({
                 ## check for parameter errors
                 if(ravCheck(testPath, inFileName)) {
                   inFilePath <- testPath
                 }
               }),
               SCANVIRUS=isolate({
                 ## virus found
                 themsg <- VIRUSDETECTIONMSG
                 session$sendCustomMessage('setmsg', themsg)
               }),
               isolate({
                 ## error, default
                 themsg <- UPLOADERRORMSG
                 session$sendCustomMessage('setmsg', themsg)
               }))

        ## if the upload failed, reset the upload control
        switches$resetupload <- is.null(inFilePath)
        
      } else if (inputtype() == "demo") {
        demoFile <- demoFileInput()
        if (is.null(demoFile)) {
          inFilePath <- NULL  ## returns NULL below
          } else {
            demoCopy <- file.path(getOutputDirectory(), demoFile[[1]])
            file.copy(demoFile[[2]],demoCopy)
            inFileName <- demoFile[[1]]
            inFilePath <- demoCopy
          }
      }
      
      if (is.null(inFilePath)) { return(NULL) }
      
      setCurrentInput(inFilePath)
    }
    return(c(inFilePath, inFileName))
  })
  
  inNRuns <- reactive({
    return(as.numeric(input$NRuns))
  })

  
  ###########################################################
  ## RAV editing
  ###########################################################

  output$ravsavebutton <- renderUI({
    if (switches$ravparamsokay) {
      actionButton("saveravedits",
                   label="Save edits",
                   title="Click to save changes to RAV parameters")
    } else {
      actionButton("saveravedits",
                   label="Save edits",
                   title=paste("Correct parameters mismatches,",
                               "then click to save changes"),
                   disabled="disabled")
    }
  })

  observe({
    req(input$saveravedits > 0)
    isolate({
      saveModRav(input$raveditor)
    })
  })

  genModRavName <- function(currInputFile) {
    fileName <- basename(currInputFile)
  }

  ravCheck <- function(filepath, filename) {
    if (file.exists(filepath)) {
      resp <- ravchecker(filepath)
      if (length(resp) > 0) {
        thes <- if(length(resp) > 1) {"s"} else {""}
        msg <- paste0("Problem",thes," in ", filename,":<br/><br/>",
                     paste(resp,collapse="<br/>"))

        session$sendCustomMessage('setmsg', msg)
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else {
      warning("Attempting to check rav file ", filepath,
              ": file does not exist.")
      return(FALSE)
    }
  }

  ## On return, setCurrentInput has been called with the path to
  ## the modified rav file
  saveModRav <- function(newRav) {
    if (is.null(newRav)) {
      cat("Error: newRav is NULL\n")
      return()
    }

    testRavPath <- tempfile(pattern="testrav",fileext="rav")
    
    cat(newRav, "\n", file=testRavPath)
    if (!ravCheck(testRavPath, "edited rav file")) {
      return();
    }
    
    currInput <- inputfile()[2]

    ## remove extension
    currInput <- str_replace(currInput,"\\.[^.]*$","")

    ## look for an edit version, extracting filename as well
    nameAndVersion <-
          stringr::str_match(currInput,"([^/]+)_edit_(\\d{2})_\\d{8}_\\d{6}")

    ## if no edit version, set new version to 1
    ## o'wise, set to 1 plus current, wrapping at 100
    newName <- NULL
    if(is.na(nameAndVersion[1])) {
      newVersion <- 1
      newName <- currInput
    } else {
      newVersion <- (as.integer(nameAndVersion[3]) %% 99) + 1
      newName <- nameAndVersion[2]
    }
    verString <- sprintf("%02d",newVersion)
    
    ## create a new output directory and timestamp
    clearOutputDirectory()
    pathBase <- outputPathBase()

    ## extract that timestamp
    timestamp <- extractTimestamp(pathBase)
    
    ## create rav file name and path
    modRavName <- sprintf("%s_edit_%02d_%s.rav",newName,newVersion,timestamp)
    modRavPath <- file.path(getOutputDirectory(), modRavName)

    ## move the new rav file
    file.rename(testRavPath, modRavPath);
    setCurrentInput(modRavPath,deleteexisting=TRUE)

    usemodrav <<- TRUE
    switches$editedravinput <- !switches$editedravinput
  }

  updateNRuns <-  function(nruns, orig=FALSE) {
    if (orig) {
      ravchoice <- paste("rav value:",nruns)
      choices=c(1, 10, 100, 1000, nruns)
      ## choices=c(1, 10, 100, 1000, -1)
      names(choices) <- c("1","10","100","1000",ravchoice)
      selected <- nruns
      updateRadioButtons(session, "ravnruns", choices=choices,
                         selected=selected, inline=TRUE)
    } else {
      nruns <- min(1000,max(1,nruns))
      selected <- 10^(round(log10(nruns)))
      updateRadioButtons(session, "ravnruns", selected=selected,
                         inline=TRUE)
    }
  }

  updateNYears <- function(nyears) {
    nyears <- min(max(nyears, 10),100)
    nyears <- if(nyears < 18) {
      10
    } else {
      round(nyears / 25) * 25
    }
    updateRadioButtons(session, "ravnyears", selected=nyears)
  }

  updateTargetStep <- function(step) {
    step <- min(max(.01,step),0.2)
    step <- if (step <= 0.03) {
      .01
    } else {
      round(step / .05) * .05
    }
    updateRadioButtons(session, "ravtargetstep", selected=step)
  }
  
  setRavEditor <- function() {
    currInput <- getCurrentInput()
    if (is.null(currInput) || !file.exists(currInput)) {
      inputText <- ""
    } else {
      con <- file(currInput,open="r")
      inputText <<- paste(readLines(con),sep='',collapse='\n')
      close(con)
    }
    ## updateAceEditor(session, "raveditor", inputText)
    output$theraveditor <- renderUI(
      aceEditor(outputId="raveditor",
                cursorId="raveditorcursor",
                mode="text",
                height="450px",
                value=inputText)
    )

  }

  ###########################################################
  ## download handlers
  ###########################################################

  downloadHelper <- function(file, ext) {
    if (input$os == "win") {
      tmp.unix <- getDownloadTempFile()
      file.copy(outputPath(ext), tmp.unix)
      system(paste0("sed -e 's/$/\r/' ", tmp.unix," > ", file))
      file.remove(tmp.unix)
    }
    else {
      file.copy(outputPath(ext), file)
    }
  }

  output$downloadByr <- downloadHandler(
    filename = function() {paste0(getBaseName(),".byr")},
    content = function(file) {downloadHelper(file, ".byr")},
    contentType = "text/plain"
  )
  
  output$downloadSum <- downloadHandler(
    filename = function() {paste0(getBaseName(), ".sum")},
    content = function(file) {downloadHelper(file, ".sum")},
    contentType = "text/plain"
  )
  
  output$downloadEsc <- downloadHandler(
    filename = function() {paste0(getBaseName(),".esc")},
    content = function(file) {downloadHelper(file, ".esc")},
    contentType = "text/plain"
  )

  output$downloadRpt <- downloadHandler(
    filename = function() {paste0(getBaseName(), ".pdf")},
    content = function(file) {file.copy(outputPath(".pdf"), file)},
    contentType = "application/pdf"
  )
  
  output$downloadRav <- downloadHandler(
    filename = function() {basename(getCurrentInput())},
    content = function(file) {file.copy(getCurrentInput(), file)},
    contentType = "text/plain"
  )
  
  output$downloadExample <- downloadHandler(
    filename = function() {"example.rav" },
    content = function(file) {file.copy(file.path(DEMOFILESPATH,input$file2),file)},
    contentType = "text/plain"
  )
  
  ###########################################################
  ## Signals
  ###########################################################


  ## This bit is detected if an input file has been selected

  output$fileselected <- reactive({
    switch(inputtype(),
           upload = {return(inputFileExists())},
           demo = {return(!is.null(inputfile()[1]))}
           )
  })
  outputOptions(output, 'fileselected', suspendWhenHidden=FALSE)

  output$downloadsready <- reactive({
    switches$downloadstatus
    outputFilesExist()
  })
  outputOptions(output, 'downloadsready', suspendWhenHidden=FALSE)

  output$theraveditor <- renderUI(
              aceEditor(outputId="raveditor",
                    cursorId="raveditorcursor",
                    mode="text",
                    height="600px")
  )
})
