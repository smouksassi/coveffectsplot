function(input, output, session) {
  maindata <- reactiveVal(NULL)
  
  # If this app was launched from a function that explicitly set an initial dataset
  if (exists("coveffectsplot_initdata")) {
    maindata(get("coveffectsplot_initdata"))
  }
  
  # Set the data source
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    maindata(read.csv(file, na.strings = c("NA", ".")))
  })
  observeEvent(input$sample_data_btn, {
    data <- get_sample_data()
    maindata(data)
  })

  # Show inputs once the data source exists
  observeEvent(maindata(), once = TRUE, {
    shinyjs::show("exposurevariables")
    shinyjs::show("covariates")
    shinyjs::show("covvalueorder")
    shinyjs::show("shapebyparamname")
    shinyjs::show("vdodgeheight")
    shinyjs::show("get_code")
  })

  # Update the options in different inputs based on data
  observe({
    df <- maindata()
    shiny::req(df)
    choices <- unique(df[["paramname"]])
    updateSelectizeInput(session, "exposurevariables",
                         choices = choices, selected = choices[1])
  })
  observe({
    df <- maindata()
    shiny::req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariables))
    choices <- unique(df[["covname"]])
    updateSelectizeInput(session, "covariates",
                         choices = choices, selected = choices)
  })
  observe({
    df <- maindata()
    shiny::req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariables)) %>%
      filter(covname %in% c(input$covariates))
    choices <- as.character(unique(df[["label"]]))
    updateSelectizeInput(session, "covvalueorder",
                         choices = choices, selected = choices)
  })

  formatstats <- metaReactive2({
    shiny::req(maindata())
    validate(need(
      length(input$covariates) >= 1,
      "Please select a least one covariate or All"
    ))
    validate(need(
      length(input$covvalueorder) >= 1,
      "Please select a least one covariate/All level"
    ))
    df <- maindata()
    metaExpr({
      df$covname <- factor(df$covname)
      df$label <- factor(df$label)
      df$exposurename <- df$paramname
      sigdigits <- ..(input$sigdigits)
      summarydata <- df %>%
        group_by(paramname, covname, label) %>%
        mutate(
          MEANEXP = mid,  
          LOWCI = lower,  
          UPCI =  upper,  
          MEANLABEL = signif_pad(MEANEXP, sigdigits),  
          LOWCILABEL = signif_pad(LOWCI, sigdigits),  
          UPCILABEL = signif_pad(UPCI, sigdigits),  
          LABEL = paste0(MEANLABEL, " [", LOWCILABEL, "-", UPCILABEL, "]") 
        )
  
      summarydata$covvalue <- factor(summarydata$label)
      summarydata <- summarydata %>%
        filter(covname %in% c(..(input$covariates))) %>%
        filter(paramname %in% ..(input$exposurevariables))
      summarydata <- as.data.frame(summarydata)
      summarydata
    })
  })


  output$refarea <- renderUI({
    REF <- ifelse(is.na(input$refvalue),1,input$refvalue)
    ymin <-  0.8
    ymax <-  1.25
    ymaxmax <- REF * ymax *3
    ystep <- 0.05
    sliderInput(
      "refareain",
      "Reference Area",
      min = 0,
      max = ymaxmax,
      value = c(ymin, ymax),
      step = ystep,
      animate = FALSE
    )
  })
  
  outputOptions(output, "refarea", suspendWhenHidden=FALSE)
  
  observeEvent(input$colourpointrangereset, {
    shinyjs::reset("colourpointrange")
  })
  observeEvent(input$colourbsvrangereset, {
    shinyjs::reset("colourbsvrange")
  })
  observeEvent(input$stripbackfillreset, {
    shinyjs::reset("stripbackgroundfill")
  })
  observeEvent(input$fillrefareareset, {
    shinyjs::reset("fillrefarea")
  })
  observeEvent(input$colorrefvaluereset, {
    shinyjs::reset("colorrefvalue")
  })

  plotdataprepare  <- metaReactive2({
    shiny::req(formatstats())
    metaExpr({
      summarydata <-  ..(formatstats())
      summarydata [, "covname"] <-
        factor(summarydata [, "covname"], levels = c(..(input$covariates)))
      summarydata [, "label"]   <-
        factor(summarydata[, "label"]   , levels = c(..(input$covvalueorder)))
      summarydata <- summarydata %>%
        filter(label %in% c(..(input$covvalueorder)))
  
      summarydata [, "paramname"]   <-
        factor(summarydata[, "paramname"]   , levels = c(..(input$exposurevariables)))
  
      summarydata
    })
  })

  output$plot <- metaRender2(renderPlot, {
    shiny::req(plotdataprepare())

    major_x_ticks <- NULL
    minor_x_ticks <- NULL
    if (input$customxticks) {
      tryCatch({
         major_x_ticks <- as.numeric(unique(unlist(strsplit(input$xaxisbreaks), ",")[[1]]))
      }, warning = function(w) {}, error = function(e) {})
      tryCatch({
        minor_x_ticks <- as.numeric(unique(unlist(strsplit(input$xaxisminorbreaks), ",")[[1]]))
      }, warning = function(w) {}, error = function(e) {})
    }
    x_range <- if (input$userxzoom) c(input$lowerxin, input$upperxin) else NULL
    ref_value <- if (is.na(input$refvalue)) 1 else input$refvalue
    inputxaxistitle <- if(input$parsexaxistitle) parse(text=input$xaxistitle) else input$xaxistitle
    inputyaxistitle <- if(input$parseyaxistitle) parse(text=input$yaxistitle) else input$yaxistitle
    
    metaExpr({
      summarydata <- ..(plotdataprepare())
        
      plot <- forest_plot(
        data = summarydata,
        facet_formula = ..(input$facetformula),
        xlabel = ..(inputxaxistitle),
        ylabel = ..(inputyaxistitle),
        x_facet_text_size = ..(input$facettextx),
        y_facet_text_size = ..(input$facettexty),
        x_facet_text_angle = ..(input$facettextxangle),
        y_facet_text_angle = ..(input$facettextyangle),
        x_facet_text_vjust = ..(input$x_facet_text_vjust),
        y_facet_text_vjust = ..(input$y_facet_text_vjust),
        x_facet_text_hjust = ..(input$x_facet_text_hjust),
        y_facet_text_hjust = ..(input$y_facet_text_hjust),
        xy_facet_text_bold = ..(input$boldfacettext),
        x_label_text_size = ..(input$xlablesize),
        y_label_text_size = ..(input$ylablesize),
        table_text_size = ..(input$tabletextsize),
        base_size = ..(input$base_size),
        theme_benrich = ..(input$theme_benrich),
        table_title = escape_newline(..(input$custom_table_title)),
        table_title_size = ..(input$table_title_size),
        ref_legend_text = escape_newline(..(input$customlinetypetitle)),
        area_legend_text = escape_newline(..(input$customfilltitle)),
        interval_legend_text = escape_newline(..(input$customcolourtitle)),
        interval_bsv_text = escape_newline(..(input$custombsvtitle)),
        legend_order = ..(input$legendordering),
        combine_area_ref_legend = ..(input$combineareareflegend),
        legend_position = ..(input$legendposition),
        show_ref_area = ..(input$showrefarea),
        ref_area = ..(input$refareain),
        show_ref_value = ..(input$showrefvalue),
        ref_value = ..(ref_value),
        ref_area_col = ..(input$fillrefarea),
        ref_value_col = ..(input$colorrefvalue),
        interval_col = ..(input$colourpointrange),
        bsv_col      = ..(input$colourbsvrange),
        strip_col = ..(input$stripbackgroundfill),
        paramname_shape = ..(input$shapebyparamname),
        legend_shape_reverse = ..(input$legendshapereverse),
        facet_switch = ..(input$facetswitch),
        facet_scales = ..(input$facetscales),
        facet_space = ..(input$facetspace),
        strip_placement = ..(input$stripplacement),
        strip_outline = ..(input$removestrip),
        facet_spacing = ..(input$panelspacing),
        major_x_ticks = ..(major_x_ticks),
        minor_x_ticks = ..(minor_x_ticks),
        x_range = ..(x_range),
        logxscale = ..(input$logxscale),
        show_yaxis_gridlines = ..(input$showyaxisgridlines),
        show_xaxis_gridlines = ..(input$showxaxisgridlines),
        show_table_facet_strip = ..(input$showtablefacetstrips),
        table_facet_switch = ..(input$tablefacetswitch),
        show_table_yaxis_tick_label = ..(input$showtableyaxisticklabel),
        table_panel_border = ..(input$tablepanelborder),
        reserve_table_xaxis_label_space = ..(input$reservetablexaxislabelspace),
        table_position = ..(input$tableposition),
        plot_table_ratio = ..(input$plottotableratio),
        vertical_dodge_height = ..(input$vdodgeheight),
        legend_space_x_mult = ..(input$legendspacex),
        legend_ncol_interval = ..(input$ncolinterval),
        legend_ncol_shape = ..(input$ncolshape),
        plot_margin = c(..(input$margintop),..(input$marginright),
                        ..(input$marginbottom),..(input$marginleft)),
        table_margin = c(..(input$tabletop),..(input$tableright),
                        ..(input$tablebottom),..(input$tableleft))
      )
      plot
    })
  }, height = function() {
    input$height
  })
  
  observeEvent(input$get_code, {
    if (system.file(package = "shinyAce") == "") {
      stop("Please install.packages('shinyAce') and try again.")
    }
    if (system.file(package = "formatR") == "") {
      stop("Please install.packages('formatR') and try again.")
    }
    
    code <- expandChain(
      "# This code assumes you have a data file named `forest_plot_data.csv`.",
      "# You can download this file using the Download button below.",
      "#",
      "# ------",
      "#",
      quote({
        "# Load required packages"
        library(coveffectsplot)
        library(dplyr)
        library(table1)
        "# Load the data (make sure `forest_plot_data.csv` is in your working directory)"
        df <- read.csv("forest_plot_data.csv", na.strings = c("NA", "."))
        "# Helper functions"
        escape_newline <- function(s) {
          gsub("\\\\n", "\\\n", s)
        }

      }),
      "# Manipulate data and plot",
      output$plot()
    )
    
    code <- formatCode(code)
    code <- formatR::tidy_source(text = code, width.cutoff = 80, indent = 2)
    code <- code$text.tidy
    code <- paste(code, collapse = "\n")
    showModal(modalDialog(
      size = "l", easyClose = TRUE,
      shinyAce::aceEditor(
        "code_editor", value = code, wordWrap = TRUE 
      ),
      footer = tagList(
        actionButton("code_copy", "Copy to Clipboard", icon("copy")),
        downloadButton("code_download_data", "Download data file"),
        modalButton("Dismiss")
      )
    ))
  })
  
  observeEvent(input$code_copy, {
    if (system.file(package = "clipr") == "") {
      stop("Please install.packages('clipr') and try again.")
    }
    clipr::write_clip(input$code_editor, object_type = "character")
  })
  
  output$code_download_data <- downloadHandler(
    filename = "forest_plot_data.csv",
    content = function(file) {
      write.csv(maindata(), file, row.names = FALSE)
    }
  )
}
