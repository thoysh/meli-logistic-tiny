
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
  db$insert(df)
}


del_event <- function(id, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$update(query  = paste0('{ "_id": { "$oid": "', id, '" },',
                                  '  "RemovidoEm": null }'),
                  update = paste0('{ "$set": { "RemovidoPor": "', user, '" },',
                                  '  "$currentDate": { "RemovidoEm": true } }'),
                  multiple = F)
  df
}


drop_event <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$remove(query = '{}')
  df
}