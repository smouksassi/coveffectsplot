inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}


fluidPage(
  useShinyjs(),
  titlePanel("ForestPlotteR!"),
  fluidRow(
    column(2,
             tabsetPanel(
               tabPanel(
                 "Inputs", 
                 br(),
                 tags$div(
                   tags$strong("Choose csv file to upload"),
                   "or", actionLink("sample_data_btn", "use sample data")
                 ),
                 fileInput("datafile", NULL,
                           multiple = FALSE, accept = c("csv")),
                uiOutput("exposurevariables"),
                uiOutput("covariates"),
               uiOutput("covvalueorder"),
               
               textInput("yaxistitle", label = "Y axis Title", value = ""),
               textInput("xaxistitle", label = "X axis Title", value = ""),
               sliderInput("facettext", "Facet Text Size", min=8, max=32,step=1, value=22),
               sliderInput("height", "Plot Height", min=1080/4, max=1080, value=900, animate = FALSE),
               
                 hr()
               ), # tabPanel

               
               tabPanel(
                 "How To",
                 hr()
                 #includeMarkdown(file.path("text", "howto.md"))
               )# tabpanel 
               )#tabsetPanel
           ),#column3
    
    column(8, 
               uiOutput('ui_plot')
),# column6
column(2,
         tabsetPanel(
           tabPanel(
             "Plot Options",
             fluidRow(
               
               column (12,
                       hr(),
                       numericInput("sigdigits",label = "Significant Digits",value = 2,min=NA,max=NA),
                       sliderInput("textsize", "Table Text Size", min=1, max=12,step=1, value=7),
                       uiOutput("refarea"),
                       hr(),
                       
                       checkboxInput('customxticks', 'Custom X axis Ticks ?', value = FALSE),
                       
                       conditionalPanel(condition = "input.customxticks" , 
                                        textInput("xaxisbreaks",label ="X axis major Breaks",
                                                  value = as.character(paste(
                                                    0,0.25,0.5,0.8,1,1.25,1.5,1.75,2
                                                    ,sep=",") )
                                        ),
                                        textInput("xaxisminorbreaks",label ="X axis minor Breaks",
                                                  value = as.character(paste(
                                                    0.75,1.333
                                                    ,sep=",") )
                                        ),
                                        hr()
                       ),
                       checkboxInput('userxzoom', 'Custom X axis Range ?', value = FALSE),
                       conditionalPanel(condition = "input.userxzoom" , 
                                        numericInput("lowerxin",label = "Lower X Limit",value = 0,min=NA,max=NA,width='100%'),
                                        numericInput("upperxin",label = "Upper X Limit",value = 2,min=NA,max=NA,width='100%')
                       ),
                       hr(),
                              sliderInput("plottotableratio", "Plot to Table Ratio", min=1, max=5, value=4,step=0.5, animate = FALSE),
                       hr(),
                       sliderInput("ylablesize", "Y axis labels size", min=1, max=32, value=24,step=0.5),
                       sliderInput("xlablesize", "X axis labels size", min=1, max=32, value=24,step=0.5)
                       
                       )
                     
             )
           ),#tabpanel
           tabPanel(
             "Legend Options",
             checkboxInput('customlegendtitle', 'Customization of Legend items and ordering ?',value = FALSE),
             conditionalPanel(
               condition = "input.customlegendtitle",
                          
             textInput("customcolourtitle", label ="Pointinterval Legend text",
                       value="Median (points)\\n95% CI (horizontal lines)"),
             textInput("customlinetypetitle", label ="Ref Legend text",
                       value="Reference (vertical line)\\nClinically relevant limits (colored area)"),
             textInput("customfilltitle", label ="Area Legend text",
                       value="Reference (vertical line)\\nClinically relevant limits (colored area)"),
             
             selectizeInput(
               'legendordering',
               label = paste("Drag/Drop to reorder","Colour, Ref, Area Legends"),
               choices = c("pointinterval","ref","area"),
               selected = c("pointinterval","ref","area"),
               multiple=TRUE,  options = list(
                 plugins = list('drag_drop')
               )),
             checkboxInput('combineareareflegend', 'Combine Ref and Area Legends if they share the same text ?',value = FALSE)
             
             )
           )
         )  #tabsetpanel

)#closes the column 3


)# fluidrow
)#fluidpage
