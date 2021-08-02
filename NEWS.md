# djprshiny 0.0.3.9**
* reactives can now be passed to plot functions through ... in plot_server
* set font to Arial before downloading chart with djpr_plot_server
* x_expand_mult argument added to djpr_ts_linechart()
* tooltip column excluded from downloaded data
* use djprtheme::remove_labs() to remove labels from plots in djpr_girafe()
* ensure 'tooltip' column is only removed where it exists, when saving chart data
using download button
* add group_var argument to djpr_ts_linechart
* ensure function is included as a cache key within djpr_plot_server

# djprshiny 0.0.2.900
* modifications to plot_server to enable nested modules

# djprshiny 0.0.1.9000

* toc_row() and associated functions added to create table of contents
* Added a `NEWS.md` file to track changes to the package.
