
vec_eventType <- c("Suporte",
                   "Divisão de Rota",
                   "Desconto",
                   "Bônus Diária",
                   "Transferência de Rota",
                   "Ambulância",
                   "Ajuste km")

vec_eventDiscountType <- c("Abandono de Rota",
                           "Seguradora de Risco",
                           "Pacote Perdido",
                           "PNR",
                           "Outros (Inclua informações adicionais")


#' Get eventos
#'
#' @return dataframe de eventos não-removidas
get_event <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$find(query = '{ "RemovidoEm": null }',
                fields = '{}')
  if (ncol(df) == 0) return(NULL)
  if ("Data" %in% names(df))
    df$Data <- df$Data %>% as.Date()
  df
}


#' Set eventos
#'
#' @param df dataframe: dataframe para salvar no BD
#' @param user character: nome do usuário
#'
#' @return List(nInserted, nMatched, nRemoved, nUpserted, writeErrors)
set_event <- function(df, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  df <- df %>%
    mutate(
      CriadoEm  = Sys.time(),
      CriadoPor = user
    )
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Insert the data into the mongo collection as a data.frame
  lst <- db$insert(df)
}


#' Deleta (Atualiza) evento com status de remoção
#'
#' @param id character: id do evento
#' @param user character: nome do usuário
#'
#' @return List(modifiedCount, matchedCount, upsertedCount)
del_event <- function(id, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  lst <- db$update(query  = paste0('{ "_id": { "$oid": "', id, '" },',
                                   '  "RemovidoEm": null }'),
                   update = paste0('{ "$set": { "RemovidoPor": "', user, '" },',
                                   '  "$currentDate": { "RemovidoEm": true } }'),
                   multiple = F)
}


#' Drop collection de eventos
#'
#' @return T
drop_event <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  bool <- db$remove(query = '{}')
}
