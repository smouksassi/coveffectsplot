inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

fluidPage(
  useShinyjs(),
  titlePanel("ForestPlotteR!"),
  fluidRow(
    column(
      2,
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
          shinyjs::hidden(
            selectizeInput(
              'exposurevariables',
              label = "Parameter(s)",
              choices = c(),
              multiple = TRUE,
              options = list(plugins = list('remove_button', 'drag_drop')),
              width = '800px'
            ),
            checkboxInput('shapebyparamname', 'Change Symbol by Parameter(s) ?', value = TRUE),
            
            selectizeInput(
              "covariates",
              "Covariates Top to Bottom (Remove/Drag and Drop to Desired Order):",
              choices = c(),
              multiple = TRUE,
              options = list(
                placeholder = 'Please select one or more variables',
                plugins = list('remove_button', 'drag_drop')
              ),
              width = '800px'
            ),
            selectizeInput(
              'covvalueorder',
              label = paste("Drag and Drop to Desired Order within facets", "values"),
              choices = c(),
              multiple = TRUE,
              options = list(plugins = list('remove_button', 'drag_drop')),
              width = '800px'
            )
          )
        ), # tabPanel
        tabPanel("Facets",
                 selectInput(  "facetformula", "Facet Formula:",
                               choices = c("covname ~ .","covname~paramname"),
                               selected = c("covname ~ ."),
                               multiple = FALSE),
                 
                 sliderInput("facettexty", "Facet Text Size Y",
                             min = 0, max = 32, step = 1, value = 22),
                 sliderInput("facettextx", "Facet Text Size X",
                             min = 0, max = 32, step = 1, value = 22),
                 selectizeInput(  "stripplacement", "Strip Placement:",
                                  choices = c("inside","outside"),
                                  options = list(  maxItems = 1 )  ),
                 selectInput(  "facetswitch", "Facet Switch to Near Axis:",
                               choices = c("both","y","x","none"),
                               selected = c("both"),
                               multiple = FALSE),
                 selectInput(  "facetscales", "Facet Scales:",
                               choices = c("free_y","fixed","free_x","free"),
                               selected = c("free_y"),
                               multiple = FALSE),
                 selectInput('facetspace' ,'Facet Spaces:',
                             c("fixed","free_x","free_y","free") )
        ),
        tabPanel(
          "X/Y Axes",
          sliderInput("ylablesize", "Y axis labels size", min=1, max=32, value=24,step=0.5),
          sliderInput("xlablesize", "X axis labels size", min=1, max=32, value=24,step=0.5),
          checkboxInput('customxticks', 'Custom X axis Ticks ?', value = FALSE),
          conditionalPanel(
            condition = "input.customxticks" ,
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
          conditionalPanel(
            condition = "input.userxzoom" ,
            numericInput("lowerxin",label = "Lower X Limit",value = 0.01,min=NA,max=NA,width='100%'),
            numericInput("upperxin",label = "Upper X Limit",value = 2,min=NA,max=NA,width='100%')
          ),
          checkboxInput('logxscale', 'Log-scale X axis ?', value = FALSE),
          textInput("yaxistitle", label = "Y axis Title", value = ""),
          textInput("xaxistitle", label = "X axis Title", value = "")
          
        ),
        tabPanel(
          "How To",
          hr(),
          includeMarkdown(file.path("text", "howto.md"))
        ) # tabpanel
      ) # tabsetPanel
    ), # column3

    column(
      8,
      plotOutput('plot', height = "auto", width = "100%")
    ), # column6

    column(
      2,
      tabsetPanel(
        tabPanel(
          "Table/Other Options",
          fluidRow(
            column(
              12,
              hr(),
              numericInput("sigdigits",label = "Significant Digits",value = 2,min=NA,max=NA),
              sliderInput("tabletextsize", "Table Text Size", min=1, max=12,step=1, value=7),
              

              sliderInput("plottotableratio", "Plot to Table Ratio", min=1, max=5, value=4,step=0.5, animate = FALSE),

              selectInput('tableposition','Table Position:',
                          c("on the right" = "right", "below" = "below", "none" = "none") ),
              selectInput(  "showtablefacetstrips", "Show Table Facet Strip on:",
                            choices = c("none","both","y","x"),
                            selected = c("none"),
                            multiple = FALSE),
              selectInput(  "tablefacetswitch", "Table Facet Switch to Near Axis:",
                            choices = c("both","y","x","none"),
                            selected = c("both"),
                            multiple = FALSE),
              checkboxInput('showtableyaxisticklabel', 'Show Table y axis ticks/labels ?', value = FALSE),
              

              hr(),
              checkboxInput('showrefarea', 'Show Reference Area?', value = TRUE),
              conditionalPanel(condition = "input.showrefarea" ,
                               uiOutput("refarea")),
              sliderInput("height", "Plot Height", min=1080/4, max=1080, value=900, animate = FALSE),
              sliderInput("vdodgeheight", "Vertical Dodging Height", min=0.5, max=2, value=0.8, animate = FALSE)
              
              )

          )
        ),#tabpanel
        tabPanel(
          "Colour/Legend Options",
          colourpicker::colourInput("stripbackgroundfill",
                                    "Strip Background Fill:",
                                    value="#E5E5E5",
                                    showColour = "both",allowTransparent=TRUE, returnName = TRUE),
          div( actionButton("stripbackfillreset", "Reset Strip Background Fill"),
               style="text-align: right"),
          colourpicker::colourInput("colourpointrange",
                                    "Point Range Colour:",
                                    value="blue",
                                    showColour = "both",allowTransparent=TRUE, returnName = TRUE),
          div( actionButton("colourpointrangereset", "Reset Point Range Colour"),
               style="text-align: right"),

          colourpicker::colourInput("fillrefarea",
                                    "Reference Area Fill:",
                                    value= "#BEBEBE50",
                                    showColour = "both",allowTransparent=TRUE, , returnName = TRUE),
          div( actionButton("fillrefareareset", "Reset Reference Area Fill"),
               style="text-align: right"),
          sliderInput("legendspacex", "Multiplier for Space between Legends",
                      min = 0, max = 1.5, step = 0.1, value = 1),
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
              choices = c("pointinterval","ref","area","shape"),
              selected = c("pointinterval","ref","area","shape"),
              multiple=TRUE,  options = list(
                plugins = list('drag_drop')
              )),
            checkboxInput('combineareareflegend', 'Combine Ref and Area Legends if they share the same text ?',value = TRUE),
            checkboxInput('legendshapereverse', 'Reverse the order of shape legend items ?',value = FALSE)
            

          )
        )
      )  # tabsetpanel
    ) # closes the column 3
  )# fluidrow
)#fluidpage
