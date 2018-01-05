## consecFileUploadInput() will set actionButtonLabel to the value of
## its buttonLabel argument before consecFileUpload() runs, permitting
## the server-side code to use the ui-side value of buttonLabel.
actionButtonLabel <- "Upload a file"

## Lays out a simple button, filename field, and placeholder for
## the fileInput controls.
## Sets actionButtonLabel to buttonLabel argument value, thus
## "passing" that value to consecFileUpload().
consecFileUploadInput <- function(id, buttonLabel="Upload a file:") {

  ns <- NS(id)
  actionButtonLabel <<- buttonLabel
  
  tagList(
    actionButton(inputId=ns("uploadbtn"), label=buttonLabel),
    htmlOutput(outputId=ns("filename"), inline=TRUE),
    uiOutput(outputId=ns("fileinputctrl"))
  )
}

## Control responses and manipulations.
## Returns a reactive corresponding to the value of a fileInput control.
consecFileUpload <- function(input, output, session, clear,
                             accept=NULL, width=NULL,
                             currentFileLabel="Current upload: ",
                             buttonLabel=actionButtonLabel,
                             cancelLabel="Cancel",
                             attribs=list(
                               filenameLabel=c(
                                 style="padding-left:1em;font-weight:bold"))) {
  ## Replace obviously bad values with something tolerable
  if (is.null(attribs) || !is.list(attribs)) { attribs <- list() }

  ## fileUploadVisible is TRUE if fileInput is displayed, FALSE o'wise
  ## Used to determine uploadbtn action
  ## Make it reactive as convenient trigger for button label update
  switches <- reactiveValues(
    fileUploadVisible=FALSE
  )

  ## Either render or remove the fileInputControl.
  ## Putting this in its own function allows for insuring that the
  ## fileUploadVisible switch is properly set.
  renderFileInput <- function(show=TRUE) {

    ns <- session$ns

    if (show) {
      output$fileinputctrl <- renderUI({
        tagList({fileInput(inputId=ns("fileinput"),
                           label=NULL,
                           multiple=FALSE,
                           accept=accept,
                           width=width)
        })
      })
    } else {
      output$fileinputctrl <- renderUI({ tagList() })
    }
    switches$fileUploadVisible <- show
  }

  ## Set the button label to correspond to the current state.
  ## Triggered by the fileUploadVisible switch.
  observe({
    btnLabel <- if (switches$fileUploadVisible) {
      cancelLabel
    } else {
      buttonLabel
    }
    updateActionButton(session, "uploadbtn", label=btnLabel)
  })
  

  ## triggered when fileInput changes value
  ## (that is, when it uploads a file)
  observe({
    if (is.null(inputFile())) { return() }

    ## remove the fileUpload control
    renderFileInput(show=FALSE)

    ## display the name of the uploaded file
    output$filename <- renderUI({
      tagList(
        do.call("span",c(list(currentFileLabel),attribs[["filenameLabel"]])),
        do.call("span",c(list(inputFile()$name),attribs[["filename"]]))
      )
    })
  })

  ## Reset the interface to its intial state:
  ##  1. remove the uploaded file name field
  ##  2. remove the fileInput control
  observe({
    if (clear()) {
      output$filename <- renderUI({tagList()})
      renderFileInput(show=FALSE)
    }
  })
  
  ## reactive containing the current fileInput value list
  ## - use inputFile as a trigger for other reactives to insure
  ##   values are saved before the input$fileinput ctrl is removed
  inputFile <- reactive({
    if (is.null(input$fileinput)) { return(NULL) }
    input$fileinput
  })

  ## Handles upload/cancel actionButton clicks:
  ## User wants a file upload -> render a fileInput
  ## User is canceling -> remove the fileInput
  observe({
    if(input$uploadbtn > 0) {
      isolate({
        if (switches$fileUploadVisible) {
          renderFileInput(show=FALSE)
        } else {
          renderFileInput(show=TRUE)
        }
      })
    }
  })

  return(inputFile)
}

             
