.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
library(appFrame)
require(shinyAce)

source("version.R")

appTitle <- paste0("<span title='Shiny app version / Package version' ",
                   "style='font-weight:700;font-size:24px'>",
                   shiny.version)

vrapVersion <- packageVersion("VRAPS")
COLWIDTH <- 5

if (!is.null(vrapVersion) && length(vrapVersion)>0) {
  appTitle <- paste0(appTitle, " / ",vrapVersion)
} else {
  appTitle <- paste0(appTitle,"</span")
}

examplesList <- c("Select a demo"="NOSEL",EXAMPLES)

fluidPage(
  appFrameHeaderFixed(),
  titlePanel(HTML(appTitle),windowTitle="VRAPS"),
  conditionalPanel(
    condition=paste("$('html').attr('class')=='shiny-busy' || ",
                    "$('spinnerhost').hasClass('busy')",sep=""),
    div(textOutput("dummy2")),
    div(id="spinnerhost",
        class="busy",
        p(id="waitmsg", " "),
        img(src="spinner.gif",
            alt=paste("Busy, please wait",
                      "(Close browser window to stop job)"
                      )
            )
        )
  ),
  conditionalPanel(
    condition="$('#msghost').hasClass('msgdisplay')",
    div(id="msghost",
        class="nomsgdisplay",
        p(id="msghostmsg","the message"),
        actionButton("msgclosebtn", "Close")
        )
  ),
  tabsetPanel(
    id="tabset",
    tabPanel(
      title="Run VRAPS",
      value="datainputtab",
      br(),
      fluidRow(
        column(
          COLWIDTH,
          selectInput(
            "type", "Step 1: Input data",
            list("Upload rav file (click to select demo instead)" = "upload",
                 "Choose a demo file (click to upload data instead)" = "demo"),
            selected="upload",
            width="400px"
          )
        ),
        column(
          COLWIDTH,
          conditionalPanel(
            "input.type == 'upload'",
            div(id="uploadravfilediv", strong("Select rav file:")),
            consecFileUploadInput("ravupload",
                                  "Browse..."),
            br()
          ),
          conditionalPanel(
            "input.type == 'demo'",
            selectInput("demofile", "Select demo file:", examplesList))
            ## selectInput("demofile", "Select demo file:", EXAMPLES))
        )
      ),
      fluidRow(
        column(
          2*COLWIDTH,
          uiOutput('timest')
        )
      ),
      fluidRow(
        column(
          2*COLWIDTH,
          radioButtons(
            'NRuns',
            'Step 2: Choose the number of runs per simulation (NRuns)',
            c('1'=1, '10'=10, '100'=100, '1000'=1000, 'Use rav NRuns' = -1),
            selected=-1, inline=TRUE)
          )
      ),
      tags$hr(),
      fluidRow(
        column(
          2*COLWIDTH,
          conditionalPanel(
            condition="!output.fileselected",
            strong("Step 3: Run VRAPS."),
            br(),
            HTML("<em>Button appears after step 1 is completed</em>.")
          ),
          conditionalPanel(
            "output.fileselected",
            fluidRow(
              column(
                3,
                div(id="runvrapdiv", strong("Step 3: Run VRAPS."))
              ),br()),
            fluidRow(
              column(
                3,
                htmlOutput(outputId="runbutton")
              )
            )
          )
        )
      ),
      tags$br(),
      tags$br(),
      tags$br()
    ),
    tabPanel(
      title="Edit Rav file",
      value="updateravtab",
      conditionalPanel(
        condition="output.fileselected",
        wellPanel(
          uiOutput("theraveditor"),
          br(),
          fluidRow(
            div(
              id="ravsavebutton",
              actionButton(inputId="saveravedits",
                           "Save edits")
            )
          )
        )
      ),
      conditionalPanel(
        condition="!output.fileselected",
        includeHTML("html/help_ravedit.html")
      )
    ),
    tabPanel(
      title="Results (.sum)",
      value="resultstabsum",
      uiOutput('contentssum')
    ),
    ## uncomment following to display BYr results tab
    ## tabPanel(
    ##   title="Results (.byr)",
    ##   value="resultstabbyr",
    ##   uiOutput('contentsbyr')
    ## ),
    tabPanel(
      title="Results (.esc)",
      value="resultstabesc",
      uiOutput('contentsesc')
    ),
    tabPanel(
      title="Downloads",
      value="downloadstab",
      uiOutput("contentsdownloads")
    ),
    tabPanel(
      title="Help",
      value="helptab",
      includeHTML("html/help_main.html")
    ),
    tabPanel(
      title="About",
      id="aboutPanel",
      h4("About VRAPS"),
      includeHTML("html/app.html")
    ),
    selected="helptab",
    tags$head(
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setspinner',
                      function(msg) {
                         $('#spinnerhost').attr('class',msg);
                      })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setmsg',
                      function(msg) {
                         $('#msghostmsg').html(msg);
                         $('#msghost').attr('class', 'msgdisplay');
                      })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setwaitmsg',
                      function(msg) {
                         $('#waitmsg').html(msg);
                      })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('clearmsg',
                      function(msg) {
                         $('#msghostmsg').html('');
                         $('#msghost').attr('class', 'nomsgdisplay');
                      })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('resetfileupload',
                      function(msg) {
                         $('#file1_progress > .progress-bar').css('width','0%');
                      })"
             )
      ),
      tags$link(rel="stylesheet", type="text/css", href="style.css")
    )
  ),
  appFrameFooterFixed(displayAppsURL="../..")
)
