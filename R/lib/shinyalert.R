#' Define template de Spinner
#'
shinyalertTy <- function(...) {
  shinyalert(..., closeOnEsc = F, showConfirmButton = F, closeOnClickOutside = T)
}

