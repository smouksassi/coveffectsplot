# Same as base R `which()` function, but return 0 instead of an empty vector
# if there are no TRUE values in the array
which0 <- function(x) {
  result <- which(x)
  if (length(result) == 0) {
    result <- 0
  }
  result
}

#' Foreset plot
#'
#' Produce forest plots to visualize covariate effects
#'
#' @param data Data to use.
#' @param facet_formula Facet formula.
#' @param xlabel X axis title.
#' @param ylabel Y axis title.
#' @param x_facet_text_size Facet text size X.
#' @param y_facet_text_size Facet text size Y.
#' @param x_label_text_size X axis labels size.
#' @param y_label_text_size Y axis labels size.
#' @param table_text_size Table text size.
#' @param ref_legend_text Reference legend text.
#' @param area_legend_text Area legend text.
#' @param interval_legend_text Pointinterval Legend text.
#' @param legend_order Legend order. A four-element vector with the following
#' items ordered in your desired order: "pointinterval", "ref", "area", "shape".
#' @param combine_area_ref_legend Combine reference and area legends if they
#' share the same text?
#' @param show_ref_area Show reference window?
#' @param ref_area Reference area. Two-element numeric vector.
#' @param ref_value X intercept of reference line.
#' @param ref_area_col Reference area background color.
#' @param interval_col Point range color.
#' @param strip_col Strip background color.
#' @param paramname_shape Map symbol to parameter(s)?
#' @param facet_switch Facet switch to near axis. Possible values: "both", "y",
#' "x", "none".
#' @param facet_scales Facet scales. Possible values: "free_y", "fixed",
#' "free_x", "free".
#' @param facet_space Facet spaces. Possible values: "fixed", "free_x",
#' "free_y", "free".
#' @param strip_placement Strip placement. Possible values: "inside", "outside".
#' @param major_x_ticks X axis major ticks. Numeric vector.
#' @param minor_x_ticks X axis minor ticks. Numeric vector.
#' @param x_range Range of X values. Two-element numeric vector.
#' @param show_table_facet_strip Show table facet strip?
#' @param table_position Table position. Possible values: "right", "below", "none".
#' @param plot_table_ratio Plot-to-table ratio. Suggested value between 1-5.
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1
#'
#' plotdata <- get_sample_data("forest-plot-table.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' param <- "BZD AUC"
#' plotdata <-  filter(plotdata,paramname==param)
#' plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
#' plotdata$label <- reorder(plotdata$label,plotdata$scen)
#' covs <- c("WEIGHT","AGE")
#' plotdata <-  filter(plotdata,covname%in%covs)

#' forest_plot(plotdata,
#'             ref_legend_text = "Reference (vertical line)",
#'             area_legend_text = "Reference (vertical line)",
#'             xlabel = paste("Fold Change in", param, "Relative to Reference"),
#'             show_ref_area = FALSE,
#'             facet_formula = "covname~.",
#'             facet_scales = "free_y",
#'             facet_space = "free_y",
#'             show_table_facet_strip = FALSE,
#'             table_position = "right",
#'             plot_table_ratio = 4)
#'
#' # Example 2
#'
#' plotdata <- get_sample_data("forest-plot-table.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' param <- c("BZD AUC","BZD Cmax")
#' plotdata <-  filter(plotdata,paramname%in%param)
#' plotdata <-  filter(plotdata,covname%in%"WEIGHT")
#' plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
#' plotdata$label <- reorder(plotdata$label,plotdata$scen)
#' forest_plot(plotdata,
#'             ref_legend_text = "Reference (vertical line)",
#'             area_legend_text = "Reference (vertical line)",
#'             xlabel = paste("Fold Change of Parameter", "Relative to Reference"),
#'             show_ref_area = FALSE,
#'             facet_formula = "covname~paramname",
#'             facet_scales = "free_y",
#'             facet_space = "free_y",
#'             x_facet_text_size = 10,
#'             y_facet_text_size = 10,
#'             y_label_text_size = 10,
#'             x_label_text_size = 10,
#'             facet_switch = "both",
#'             show_table_facet_strip = TRUE,
#'             table_position = "below",
#'             plot_table_ratio = 1)
#'
#' # Example 3
#'
#' plotdata <- get_sample_data("forestplotdatacpidata.csv")
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 12,
#'             y_facet_text_size = 12,
#'             y_label_text_size = 10,
#'             x_label_text_size = 10,
#'             table_text_size = 6,
#'             plot_table_ratio = 1.5,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to RHZE",
#'             facet_formula = "covname~paramname",
#'             table_position = "below")
#'
#' # Example 4
#'\dontrun{
#' plotdata <- get_sample_data("dataforest.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' plotdata <- plotdata %>%
#'   filter(covname%in%c("Weight"))
#' plotdata$label <- as.factor(as.character(plotdata$label))
#' plotdata$label <- factor(plotdata$label, c("36.2 kg","66 kg","110 kg"))
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 13,
#'             y_facet_text_size = 13,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to Parameter",
#'             facet_formula = "covname~paramname",
#'             facet_switch = "both",
#'             facet_scales = "free",
#'             facet_space = "fixed",
#'             table_position = "below",
#'             plot_table_ratio = 1)
#'
#' # Example 5
#'
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 13,
#'             y_facet_text_size = 13,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to Parameter",
#'             facet_formula = "covname~.",
#'             facet_switch = "both",
#'             facet_scales = "free",
#'             facet_space = "fixed",
#'             paramname_shape = TRUE,
#'             table_position = "below",
#'             plot_table_ratio = 1)
#'}
#' @export
forest_plot <- function(
  data,
  facet_formula = "covname~paramname",
  xlabel = "",
  ylabel = "",
  x_facet_text_size = 13,
  y_facet_text_size = 13,
  x_label_text_size = 16,
  y_label_text_size = 16,
  table_text_size = 7,
  ref_legend_text = "",
  area_legend_text = "",
  interval_legend_text = "",
  legend_order = c("pointinterval", "ref", "area", "shape"),
  combine_area_ref_legend = TRUE,
  show_ref_area = TRUE,
  ref_area = c(0.8, 1.25),
  ref_value = 1,
  ref_area_col = "#BEBEBE50",
  interval_col = "blue",
  strip_col = "#E5E5E5",
  paramname_shape = FALSE,
  facet_switch = c("both", "y", "x", "none"),
  facet_scales = c("fixed", "free_y", "free_x", "free"),
  facet_space = c("fixed", "free_x", "free_y", "free"),
  strip_placement = c("inside", "outside"),
  major_x_ticks = NULL,
  minor_x_ticks = NULL,
  x_range = NULL,
  show_table_facet_strip = FALSE,
  table_position = c("right", "below", "none"),
  plot_table_ratio = 4)
{

  table_position <- match.arg(table_position)
  legend_order <- match.arg(legend_order, several.ok = TRUE)
  facet_switch <- match.arg(facet_switch)
  facet_scales <- match.arg(facet_scales)
  facet_space <- match.arg(facet_space)
  strip_placement <- match.arg(strip_placement)
  facet_formula <- stats::as.formula(facet_formula)

  if (x_facet_text_size <= 0) {
    x.strip.text <- ggplot2::element_blank()
  } else {
    x.strip.text <- ggplot2::element_text(size = x_facet_text_size)
  }
  if (y_facet_text_size <= 0) {
    y.strip.text <- ggplot2::element_blank()
  } else {
    y.strip.text <- ggplot2::element_text(size = y_facet_text_size)
  }

  if (xlabel == "") {
    xlabel <- paste("Changes of Parameter Relative to Reference")
  }

  if (ref_legend_text == "") {
    ref_legend_text <- "Reference (vertical line)\nClinically relevant limits (colored area)"
  }
  if (area_legend_text == "") {
    area_legend_text <- "Reference (vertical line)\nClinically relevant limits (colored area)"
  }
  if (interval_legend_text == "") {
    interval_legend_text <- "Median (points)\n95% CI (horizontal lines)"
  }

  interval_pos <-  which0(legend_order == "pointinterval")[1]
  fill_pos <- which0(legend_order == "area")[1]
  linetype_pos <- which0(legend_order == "ref")[1]
  shape_pos <- which0(legend_order == "shape")[1]
  if (combine_area_ref_legend) {
    fill_pos <- linetype_pos
  }
  guide_interval <- ggplot2::guide_legend("", order = interval_pos)
  guide_fill <- ggplot2::guide_legend("", order = fill_pos)
  guide_linetype <- ggplot2::guide_legend("", order = linetype_pos)
  guide_shape <- ggplot2::guide_legend("", order = shape_pos,
                                       override.aes = list(linetype = 0, colour = "gray"))

  data$label <- factor(data$label)

  main_plot <-
    ggplot2::ggplot(data = data, ggplot2::aes_string(
      y = "label",
      x = "mid",
      xmin = "lower",
      xmax = "upper"
    )) +
    ggstance::geom_pointrangeh(
      position = ggstance::position_dodgev(height = 0.75),
      ggplot2::aes(color = interval_legend_text),
      size = 1,
      alpha = 1
    )

  if (show_ref_area) {
    main_plot <- main_plot +
      ggplot2::annotate(
        "rect",
        xmin = min(ref_area),
        xmax = max(ref_area),
        ymin = -Inf,
        ymax = Inf,
        fill = ref_area_col
      ) +
      ggplot2::geom_ribbon(
        x = 1,
        ymax = 1,
        ymin = 1,
        ggplot2::aes(fill = area_legend_text),
        size = 1
      )  # fake ribbon for fill legend
  }

  main_plot <- main_plot +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = ref_value, linetype = ref_legend_text),
      size = 1
    ) +
    ggplot2::scale_colour_manual("", breaks = interval_legend_text, values = interval_col) +
    ggplot2::scale_linetype_manual("", breaks = ref_legend_text, values = 2) +
    ggplot2::scale_fill_manual("", breaks = area_legend_text, values = ref_area_col) +
    ggplot2::guides(colour = guide_interval,
                    linetype = guide_linetype,
                    fill = guide_fill,
                    shape = guide_shape)

  if (!show_ref_area) {
    main_plot <- main_plot +
      ggplot2::guides(colour = guide_interval,
                      linetype = guide_linetype,
                      shape = guide_shape,
                      fill = NULL)

  }

  main_plot <- main_plot +
    ggplot2::aes_string(group = "paramname")
  if (paramname_shape) {
    main_plot <- main_plot +
      ggplot2::aes_string(shape = "paramname")
  }

  if (facet_switch != "none") {
    main_plot <- main_plot +
      ggplot2::facet_grid(facet_formula,
                          scales = facet_scales,
                          space = facet_space,
                          switch = facet_switch)
  } else {
    main_plot <- main_plot +
      ggplot2::facet_grid(facet_formula,
                          scales = facet_scales,
                          space = facet_space,
                          switch = NULL)
  }

  main_plot <- main_plot +
    ggplot2::ylab("") +
    ggplot2::theme_bw(base_size = 22) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        angle = 0,
        size = y_label_text_size
      ),
      axis.text.x = ggplot2::element_text(size = x_label_text_size),
      legend.position = "top",
      legend.justification = c(0.5, 0.5),
      legend.direction = "horizontal",
      legend.key.width = ggplot2::unit(3, "line"),
      strip.text.x = x.strip.text,
      strip.text.y = y.strip.text,
      panel.grid.minor = ggplot2::element_line(colour = "gray", linetype = "dotted"),
      panel.grid.major = ggplot2::element_line(colour = "gray", linetype = "solid"),
      strip.background = ggplot2::element_rect(fill = strip_col),
      strip.placement  = strip_placement
    ) +
    ggplot2::ggtitle("\n") +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)

  if (length(major_x_ticks) || length(minor_x_ticks)) {
    main_plot <- main_plot +
      ggplot2::scale_x_continuous(
        breaks = major_x_ticks,
        minor_breaks = minor_x_ticks
      )
  }

  if (!is.null(x_range)) {
    main_plot <- main_plot +
      ggplot2::coord_cartesian(xlim = x_range)
  }

  if (table_position != "none") {
    table_plot <- ggplot2::ggplot(data = data,
                                  ggplot2::aes_string(y = "label"))
    table_plot <- table_plot +
      ggplot2::aes_string(group = "paramname") +
      ggplot2::geom_text(
        ggplot2::aes_string(
          x = 1,
          label = "LABEL",
          hjust = 0.5
        ),
        size = table_text_size,
        position = ggstance::position_dodgev(height = 0.75)
      )

    if (facet_switch != "none") {
      table_plot <- table_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = facet_switch)
    } else {
      table_plot <- table_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = NULL)
    }

    table_plot <- table_plot +
      ggplot2::theme_bw(base_size = 26) +
      ggplot2::theme(
        strip.text.x = x.strip.text,
        strip.text.y = y.strip.text,
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = strip_col)
      ) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::xlim(c(0.99, 1.01)) +
      ggplot2::theme(legend.position = "none")


    if (!show_table_facet_strip) {
      table_plot <- table_plot +
        ggplot2::theme(
          strip.text = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank()
        )
    }
  }

  if (table_position == "none") {
    result <- main_plot
  } else if (table_position == "right") {
    result <- egg::ggarrange(
      main_plot,
      table_plot,
      nrow = 1,
      widths = c(plot_table_ratio, 1)
    )
  } else if (table_position == "below") {
    result <- egg::ggarrange(
      main_plot,
      table_plot,
      nrow = 2,
      heights = c(plot_table_ratio, 1)
    )
  }

  result
}

