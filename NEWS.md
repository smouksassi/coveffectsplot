# coveffectsplot 1.0.4
* updated vignettes and examples
* added citation and start up message
* added `major_x_labels` argument for custom x labels
* added `interval_linewidth` argument to control interval linewidth
* added and exported `expand_modelframe` function
* added the possibility to select the shapes manually via `interval_shape` and `bsv_shape`
* added capability for user to reverse color legend separately via `legend_color_reverse`
* added capability for user to specify text for legend titles via `interval_legend_title` and `shape_legend_title` as well as to control legend title text size via `legend_title_size`
* added capability for user to specify facet text color via `x_facet_text_col` and `y_facet_text_col` for improved theming support
* added possibility to add different ref line(s)/area(s) by parameter via
 `ref_value_by_panel` and `ref_value_by_panel_data` function arguments (not in the shiny, app)

# coveffectsplot 1.0.3
* fixed a bug when ref_value when not equal 1 affecting y axis limits
* fixed a bug when the guides for color and shape were not merged
* updated the UI to be able to specify user-defined color for parameters

# coveffectsplot 1.0.2
* vignettes updated to reflect latest addition for the tutorial CPT:PSP paper
* Added function arguments `paramname_color` and `combine_interval_shape_legend` to enable color by paramname
* Added function arguments `combine_interval_shape_legend` to enable merging of color and pointinterval legends
* Added `covdatasim` as a dataset

# coveffectsplot 1.0.1
* cran fix for url not available `wtage` is now a dataset that ships with the package

# coveffectsplot 1.0.0
* bumped the version to 1.0 to correspond with the PSP tutorial paper (under review)
* shinymeta on CRAN! Added back code generation in the app (contributed by Dean Attali)
* added arguments and app options to control pointinterval size `interval_size` and `interval_fatten`
* added arguments and app options to control ref value  `ref_value_size` and `ref_value_linetype`
* added arguments and app options to control which text is detected as bsv `bsv_text_id`

# coveffectsplot 0.0.9.1
* Conditional use of cairo png to avoid CRAN failures
* added back pk and pkpd vignettes after fixing R 4.0 issues
* added `break_y_label` and `y_label_text_width` arguments to break long y axis labels
* Added `facet_labeller` options in the shiny app
* Added a function argument `label_wrap_width` to be used with "label_wrap_gen""
* fixed a bug with `show_table_yaxis_tick_label` on the left

# coveffectsplot 0.0.9
* ui improvements and controls for the new arguments
* added `x/y_facet_text_vjust/hjust` arguments for the facets x/y text hjust/vjust
* added `xy_facet_text_bold` argument to control facets text face
* added `facet_spacing` argument to control facets spacing
* added `facet_labeller` argument (function only not in the shiny app)
* added `strip_outline` argument to allow the removal of strip.background
* added `show_yaxis_gridlines` and `show_xaxis_gridlines` arguments to be able to remove axes grid lines
* added `show_ref_value` and `ref_value_col` arguments for better controls on the ref line
* added `plot_margin` argument (controls plot margins)
* added `table_margin` argument (controls table margins)
* added `legend_margin` argument (controls legend margins)
* added `parse_xlabel` and `parse_ylabel` arguments to parse `xlabel`, `ylabel`
* added `table_panel_border` argument to allow the removal of panel.border of the table plot
* added `reserve_table_xaxis_label` argument to allow alignment of switched strips 
* added `legend_position` argument to control the legend position
* added `legend_ncol_interval` argument to control number of columns of the interval legend
* added `legend_ncol_shape` argument to control number of columns of the shape legend
* added vignette on pediatric multivariate covariate simulations and removed the pk and pkpd ones

# coveffectsplot 0.0.5
* added an option to have different color and text for BSV (issue submitted by professor France Mentre)
* added two vignettes showing how to simulate a PK , PK/PD and exposure response models from scratch.
* added an argument to control theme_bw: `base_size`
* added theming proposed by Benjamin Rich and arguments to add a table title and size.
* added an argument to ensure alignment of strips between table and plot.
* changing default rounding and padding for table numbers (proposed by Benjamin Rich)
* added options to control the legends number of columns.
* added hooks for more control on margins, strips etc.

# coveffectsplot 0.0.4
* added an option to have a log x axis
* added more control on the table strips


# coveffectsplot 0.0.3

* added an option to return a list of plots to enable further modification to the ggplot(s) if need be
* updated the vignette and examples to demo the new options
* added possibility to choose on which facet to show strips for the table
* fixed a bug that showed up with ggplot dev version by explicitly defining a data.frame to the data argument
* minor ui tweaks to enable named colors in colourpicker

# coveffectsplot 0.0.2
* Removed reference to the old name of the package
* updated vignette and docs
* tweaked the order of the shape legends to reverse
* modified default height of vertical dodging and added an option
* added an option to control legend item x spacing


# coveffectsplot 0.0.1
* Initial Release of coveffectsplot
