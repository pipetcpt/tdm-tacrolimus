# setup ----

library(shinydashboard)
library(shinyTime)
library(lubridate)
library(shinythemes)

tags$head(
  tags$link(rel = "stylesheet", 
            type = "text/css", 
            href = "bootstrap.css")
)

title_h5 <- function(chr="Predicted concentration"){
  h5(chr, align='center', style="color:#dbdbdb; font-weight:bold")
}

# main ----

shiny::navbarPage(
  title = "Tacrolimus TDM",
  theme = shinytheme("paper"),
  
  # Chapter 1. Patient Info `pinfo` ----
  
  tabPanel(
    #icon = icon("user-plus"), 
    icon = icon("address-card"), 
    title = "Patient Info",
    tabName="pinfo",
    h2("- Patient Information -",
       align='center',
       style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(
      width=4,
      box(title = "Patient ID",width = NULL,solidHeadner = TRUE,status = "primary",
          textInput("pid","", value = "003866", width = NULL, placeholder = NULL)),
      #tags$hr(),
      box(title = "Information", status = "primary",width = NULL,solidHeader = TRUE,
          numericInput("post_op_date", "POD, Post OP date (day)", 3, min = NA, max = NA, 
                       step = 0.5,
                       width = NULL)
      )
    ),
    column(width=4,
           box(title = "Population PK parameters", 
               status = "primary", width = NULL,
               solidHeader = TRUE,
               tableOutput('typical_values')))
  ),
  
  # Chapter 2. Dosage Regimen `dosa` ----
  
  tabPanel(
    icon = icon("eyedropper"),
    title = "Dosage regimen", 
    tabName="dosa",
    h2(" Dosage Regimen",align='center',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(width=4,
           box(width = NULL, status = "primary", 
               solidHeader = TRUE, 
               title="Upload Dosing History", 
               fileInput('file1', 'Choose File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               tags$hr(),
               selectInput("rt", "Administration Route", 
                           c("Oral administration"), selected = NULL, multiple = FALSE,
                           selectize = TRUE, width = NULL, size = NULL)) ,
           tags$hr(),
           box(width=NULL,
               background = "teal",
               "Dosing History Template (CSV)",
               tags$p(), 
               downloadButton('downloadData', 'Download'))
    ),
    column(width=4,
           box( width=NULL, status = "primary", solidHeader = TRUE, title="Dosing History", 
                tableOutput('dosing_history_contents'))),
    column(width=2)
  ),
  
  # Chapter 3. Observations `obs` ----
  
  tabPanel(
    icon = icon("flask"),
    title = "Obesrvations", 
    tabName="obs",
    h2("Observations",align='center',style="color:#dbdbdb; font-weight:bold"),
    tags$hr(),
    column(width=2),
    column(
      width=4,
      box(width = NULL, 
          status = "primary", 
          solidHeader = TRUE, 
          title="Number of Observations",
          #textInput("1")
          selectInput("Observations","",c("1" = "1","2" = "2")))
    ),
    column(
      width=4,
      conditionalPanel(
        condition = "input.Observations == 1", 
        box(width = NULL , solidHeader = TRUE, title="First Observation",
            numericInput("obsc", "Observed Concentration (mg/L)", 10, min = NA, max = NA, step = NA,
                         width = NULL),
            dateInput("obsDate", "Date", value = "2019-03-05", min = NULL, max = NULL,
                      format = "yyyy-mm-dd", startview = "month", weekstart = 1,
                      language = "en", width = NULL),
            timeInput("obsTime", "Time", value = strptime("10:30", "%R"), seconds = FALSE)
        )
      ),
      conditionalPanel(
        condition = "input.Observations == 2",
        box(width = NULL ,solidHeader = TRUE, title="First Observation",
            numericInput("obsc1", "Observed Concentration (mg/L)", 4.5, 
                         min = NA, max = NA, step = NA,
                         width = NULL),
            textInput("obsd1", "Time", value = "2017-05-06", width = NULL, placeholder = NULL),
            timeInput("obst1", "Date", value = strptime("23:00", "%R"), seconds = FALSE)
        ),
        tags$hr(),
        box(width = NULL ,solidHeader = TRUE, title="Second Observation",
            numericInput("obsc2", "Observed Concentration (mg/L)", 10, 
                         min = NA, max = NA, step = NA,
                         width = NULL),
            textInput("obsd2", "Time", value = "2017-01-01", width = NULL, placeholder = NULL),
            timeInput("obst2", "Date", value = strptime("23:30", "%R"), seconds = FALSE)
        )
      )
    ),
    column(width=2)
  ),
  
  # Chapter 4. PK profile 1 `main` ----
  tabPanel(
    icon = icon("line-chart"),
    title = "PK profile 1", 
    tabName="main", 
    column(
      width=3,
      title_h5('Predicted Concentration'),
      box(width=NULL, status = "primary", solidHeader = TRUE, title="", 
          tableOutput('output_table1_time_predicted_concentration')),
      tags$hr(), 
      title_h5('Individual PK Parameters'),
      box(width=NULL, status = "primary", solidHeader = TRUE, title="", 
          tableOutput('outputtable2'))
    ),
    column(width=9,
           box(width = NULL, status = "primary", title="",
               plotOutput("plotCONC", 
                          width = '800px', height="500px")),
           p('* Ctrough: 50 - 200 ng/mL is desired.'))
  ),
  
  # Chapter 5. PK profile 2 `main2` ----
  tabPanel(
    icon = icon("line-chart"),
    title = "PK profile 2", 
    tabName="main2",
    column(
      width=3,
      box(width = NULL, status="warning", solidHeader = TRUE, title = "",
          sliderInput("newdose", "Next dose (mg)", 3, min = 0, max = 30, step = 1,ticks=TRUE,
                      width = NULL)),
      box(width = NULL, status="warning", solidHeader = TRUE, title = "",
          sliderInput("newtau", "New dosing interval (hours)", 12, min = 4, max = 48, step = 4,
                      width = NULL)),
      tags$hr(),
      box(width=NULL, solidHeader = TRUE, title='',status="primary",
          sliderInput("ll", "trough levels (mg/L)", 8, min = 0, max = 500, step = 50,
                      width = NULL),
          sliderInput("ul", "peak levels (mg/L)", 16, min = 0, max = 500, step = 50,
                      width = NULL))
    ),
    column(width=9,
           box(width = NULL, status = "primary", title="",
               plotOutput("plotCONC2", 
                          width = '800px', height="500px")),
           includeMarkdown("reference_ranges.md"))
  )
  # End ----  
)
