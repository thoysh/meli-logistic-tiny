#' Cria botões html nas tabelas
#'
#' @param FUN function: função shiny 
#' @param vetId character: vetor da coluna que será identificador
#' @param nomeId character:nome do identificador do html (sem hífen) 
#'
#' @examples
#' fshinySetInput(actionButton, df$ID, "btnDo", 
#'                label = icon("play"), 
#'                onclick = 'Shiny.setInputValue("sel_btnDo", this.id, {priority: "event"})')
#'              
fshinySetInput <- function(FUN, vetId, nomeId, ...) {
  vapply(vetId, function(id) {
    ifelse(is.na(id), "", as.character(FUN(paste0(nomeId, "-", id), ...)))
  }, character(1))
}
