#' Configura e cria o arquivo log 
#'
#' @param threshold threshold: limiar de rastreamento do log
#' @examples
#' setupFutilelogger(TRACE)
#' 
setupFutilelogger <- function(threshold) {
  flog.appender(appender.file(paste0("log/", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".log")))
  flog.layout(layout.format("[~l] [~t] [~n] [~f]  ~m"))
  flog.threshold(threshold)
  flog.info("Início da sessão")
}