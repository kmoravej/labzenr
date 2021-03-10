#' Parse MDS lab files to return the markdown content
#'
#' @param notebook A character indicating the path or list of paths to MDS lab
#'   files (either.ipynb or .Rmd). If left blank, the function will recursively
#'   search for all labs in the working directory based on the file extension.
#'
#' @return A character vector where each element is the content of one markdown cell.
#'
#'
#' @examples
#' \dontrun{
#' parse_lab()
#' }
#'

parse_lab <- function(notebook) {

  # Extracting the fileextension
  file_ext <- getExtension(notebook)

  # check whether the file is jupyter notebook or Rmarkdown
  if (file_ext == "ipynb") {
  
  # read jupyternotebook as a json file and parse markdown contents
  py_parse = jsonlite::read_json(notebook)
  cells = py_parse$cells
  source <- character()
  for (cell in cells) {
    if (cell$cell_type == "markdown") {
      cell_content <- unlist(cell$source, use.names = FALSE)
      cell_content <- paste(cell_content, collapse = '')
      source[length(source) + 1] <- cell_content
    }
  }
  } else if (file_ext == "Rmd") {

    # read Rmarkdown file and parse markdown contents
    rmd_f <- readLines(notebook)
    cell_content <- paste(rmd_f, collapse = '')
    splitted <- stringr::str_split(cell_content, "```", simplify = TRUE)
    code_bool <- startsWith(splitted, "{python") | startsWith(splitted, "{r")
    source <- splitted[!code_bool]
  }
  return(source)
  }

  # helper function for extracting the file extension
  getExtension <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
