
djpr_with_spinner <- function(ui_element,
                              type = 8,
                              color = "#2A6FA2") {
  shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = 8,
    color = "#2A6FA2"
  )
}
