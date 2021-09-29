# djprshiny 0.0.6.90x
* Bootstrap 5 now used in Shiny theme
* Fixed issue where plots with dateslider would be rendered twice
* Render checkboxes on UI side

# djprshiny 0.0.6
* Plot in djpr_plot_server() does not resize when browser height changes
* Added check_box_selected arg to djpr_plot_server()
* Render download button on UI side
* Use body(plot_function) as a cache key
* Require bslib >= 0.3.0

# djprshiny 0.0.5
* New `interactive` argument to djpr_plot_server()

# djprshiny 0.0.4
* Download button now fully modularised

# djprshiny 0.0.3
* reactives can now be passed to plot functions through ... in plot_server
* set font to Arial before downloading chart with djpr_plot_server
* x_expand_mult argument added to djpr_ts_linechart()
* tooltip column excluded from downloaded data
* use djprtheme::remove_labs() to remove labels from plots in djpr_girafe()
* ensure 'tooltip' column is only removed where it exists, when saving chart data
using download button
* add group_var argument to djpr_ts_linechart
* ensure function is included as a cache key within djpr_plot_server

# djprshiny 0.0.2
* modifications to plot_server to enable nested modules

# djprshiny 0.0.1

* toc_row() and associated functions added to create table of contents
* Added a `NEWS.md` file to track changes to the package.
