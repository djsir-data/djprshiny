djpr_box <- function(
  selector,
  content_fun,
  ...,
  header = NULL,
  format_graphs = TRUE
  ){

  # Check inputs
  if(!is.function(content_fun)) stop("content_fun must be a function")
  if(!is.logical(format_graphs) & length(format_graphs) == 1L) stop(
    "format_graphs must be a single length logical"
  )
  if(is.na(as.character(selector))) stop("selector must be character")
  if(length(as.character(selector)) != 1) stop("selector must be length 1")

  # Generate unique content id
  id <- paste0(selector, "-",deparse(substitute(content_fun)))

  # Separate shiny input args from others
  arg_list <- list(...)
  is_shiny_input <- sapply(arg_list, function(x) all(class(x) == "shiny.tag"))
  shiny_inputs <- arg_list[is_shiny_input]
  shiny_input_ids <- sapply(shiny_inputs, extract_shiny_input_id)

  # Initial box (to generate input defaults)
  initial_box <- div(
    class = "box",
    if(!is.null(header)) div(class = "box-header", header),
    div(
      class = "box-body",
      do.call(
        tagList,
        c(
          djpr_with_spinner(uiOutput(id)),
          shiny_inputs
          )
        )
    )
    )

  insertUI(selector, "beforeEnd", initial_box)

  # Generate plot
  content <- reactive(
    future::fut
  )
}


extract_shiny_input_id <- function(shiny.tag){
  shiny.tag <- as.character(shiny.tag)
  id <- stringr::str_extract_all(shiny.tag, "(?<= id=)[:graph:]+")
  id <- stringr::str_remove_all(unlist(id), "\"|\\\\|-label")
  id <- unique(id)[1]
  return(id)
}

cache_fun_index <- function(){
  if(!file.exists(".cache_fun_index"))
}
