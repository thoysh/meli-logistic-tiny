#' Converte rotas do json em dataframe
#'
#' @param fileJson character: caminho do arquivo
#' @param SC character: nome do serviceCenter
#'
#' @return dataframe com rotas do json
#'
#' @examples
#' etl_routeJson2Df("tests/route.json", "SDF1")
etl_routeJson2Df <- function(fileJson, SC) {
  flog.info("Função | %s", as.character(sys.call()[1]))

  df <- tryCatch({
    if (!file.exists(fileJson)) {
      stop("Arquivo não existe")
    } else {
      df <- fromJSON(fileJson, flatten = T) %>%
        transmute(
          Rota               = `id`,
          Cluster            = `cluster`,
          SC                 = SC,
          CC                 = ifelse(`facilityId` == "", SC, `facilityId`),
          Data               = `initDate` %>% as.POSIXct(origin = "1970-01-01", tz = timezoneTy),
          Ciclo              = `initDate` %>% as.POSIXct(origin = "1970-01-01", tz = timezoneTy) %>% am() %>% ifelse("AM", "PM"),
          Motorista          = `driver.driverName` %>% str_to_upper(),
          Tipologia          = `vehicle.description`,
          Placa              = `vehicle.license`,
          SPR                = `shipmentData.spr`,
          Entregues          = `shipmentData.delivered`,
          Ajudante           = `hasHelper`,
          Reclamacoes        = `flags.claimsCount`,
          OrhEstimada        = `plannedRoute.duration` %>% dminutes() %>% as_datetime() %>% format("%H:%M"),
          OrhRealizada       = `timingData.orh` %>% dminutes() %>% as_datetime() %>% format("%H:%M"),
          DistEstimada       = `plannedRoute.distance`,
          DistRealizada      = `shipmentData.traveledDistance`
        ) %>%
        mutate(
          TipoDia            = ifelse(format(`Data`, "%u") == 7, "DOMINGO", "UTIL/SAB"),
          FaixaKm            = ifelse(is.na(`DistRealizada`), 1, `DistRealizada`) %>% 
            findInterval(vec_rangeKm) %>% 
            vec_rangeKm[.] %>% 
            names(),
          Performance        = round(Entregues / SPR, 3)
        )
    }
  }, error = function(err) {
    flog.error("Erro | %s", err$message)
    stop("Template ou encoding do arquivo errado")
  })

  df
}


#' Get rotas
#'
#' @return dataframe de rotas não-removidas
get_route <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$find(query = '{ "RemovidoEm": null }')
  if (ncol(df) == 0) return(NULL)
  if ("Data" %in% names(df))
    df$Data <- df$Data %>% as.Date()
  df
}


#' Get rotas por período
#'
#' @param dataDe date: data de início
#' @param dataAte date: data de fim 
#'
#' @return dataframe de rotas não-removidas por período
get_routeByDate <- function(dataDe, dataAte) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  req(is.Date(c(dataDe, dataAte)))
  
  fmt <- '%Y-%m-%dT%H:%M:%S.000Z'
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # # Read entries
  df <- db$find(query = paste0('{ "Data": { "$gte": "ISODate("', format(dataDe, fmt), '")",',
                               '            "$lte": "ISODate("', format(dataAte, fmt), '")"',
                               '}}'))
  if (ncol(df) == 0) return(NULL)
  if ("Data" %in% names(df))
    df$Data <- df$Data %>% as.Date()
  df
}


#' Get upload de rotas
#'
#' @return dataframe de upload de rotas não-removidas
get_routeUploadHistory <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$aggregate(pipeline = paste0('[{ "$match": { "RemovidoEm": null } },',
                                       ' { "$group": {',
                                       '      "_id": { "Data": { "$dateToString": { "format": "%Y-%m-%d", "date": "$Data"} },',
                                       '               "SC": "$SC",',
                                       '               "CriadoPor": "$CriadoPor" },',
                                       '      "CriadoEm": { "$max": "$CriadoEm" }',
                                       ' }},',
                                       ' { "$project": {',
                                       '      "Data": "$_id.Data",',
                                       '      "SC": "$_id.SC",',
                                       '      "CriadoEm": "$CriadoEm",',
                                       '      "CriadoPor": "$_id.CriadoPor",',
                                       '      "_id": "$_id._id"', 
                                       ' }}]'))
  if (ncol(df) == 0) return(NULL)
  if ("Data" %in% names(df))
    df$Data <- df$Data %>% as.Date()
  df
}


#' Set rotas
#'
#' @param df dataframe: dataframe para salvar no BD
#' @param user character: nome do usuário
#'
#' @return List(nInserted, nMatched, nRemoved, nUpserted, writeErrors)
set_route <- function(df, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  df <- df %>%
    mutate(
      CriadoEm  = Sys.time(),
      CriadoPor = user
    )
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Insert the data into the mongo collection as a data.frame
  lst <- db$insert(df)
}


#' Valida exclusão de upload de rota, considera se já existe evento associado a uma rota do upload
#'
#' @param data date: data
#' @param SC character: nome do serviceCenter
#'
#' @return dataframe de eventos associados as rotas 
validate_del_routeUpload <- function(data, SC) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "event",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$aggregate(pipeline = paste0('[{ "$match": { "RemovidoEm": null } },',
                                       ' { "$lookup": {',
                                       '      "from": "route",',
                                       '      "localField": "Rota",',
                                       '      "foreignField": "Rota",',
                                       '      "pipeline": [{ "$match": {',
                                       '                        "Data": { "$eq": "', as.character(data), '" },',
                                       '                        "SC": { "$eq": "', SC, '" },',
                                       '                        "RemovidoEm": null',
                                       '                   }}],',
                                       '      "as": "childs"',
                                       ' }},',
                                       ' { "$match": { "childs": { "$ne": [] } } }',
                                       ' ]'))
  
  if (ncol(df) == 0) return(NULL)
  df
}


#' Deleta (Atualiza) rota com status de remoção
#'
#' @param data date: data
#' @param SC character: nome do serviceCenter
#' @param user character: nome do usuário
#'
#' @return List(modifiedCount, matchedCount, upsertedCount)
del_route <- function(data, SC, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  lst <- db$update(query  = paste0('{ "Data": { "$eq": "', as.character(data), '" },',
                                   '  "SC": { "$eq": "', SC, '" },',
                                   '  "RemovidoEm": null }'),
                   update = paste0('{ "$set": { "RemovidoPor": "', user, '" },',
                                   '  "$currentDate": { "RemovidoEm": true } }'),
                   multiple = T)
}


#' Drop collection de rotas
#'
#' @return T
drop_route <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  bool <- db$remove(query = '{}')
}
