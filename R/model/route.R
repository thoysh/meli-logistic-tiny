#' @examples
#' etl_routeJson2Df("tests/route.json", "SDF1")
etl_routeJson2Df <- function(fileJson, serviceCenter) {
  flog.info("Função | %s", as.character(sys.call()[1]))

  df <- tryCatch({
    if (!file.exists(fileJson)) {
      stop("Arquivo não existe")
    } else {
      df <- fromJSON(fileJson, flatten = T) %>%
        transmute(
          Rota               = `id`,
          Cluster            = `cluster`,
          SC                 = serviceCenter,
          CC                 = ifelse(`facilityId` == "", serviceCenter, `facilityId`),
          Data               = (`initDate` /24/60/60) %>% as.Date(origin = "1970-01-01"),
          Ciclo              = (`initDate` /24/60/60) %>% as_datetime(origin = "1970-01-01") %>% am() %>% ifelse("AM", "PM"),
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


get_route <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$find(query = '{ "RemovidoEm": null }')
  if (ncol(df) == 0) return(NULL)
  df
}


get_routeByData <- function(data) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$find(query  = paste0('{ "Data": { "$eq": "', as.character(data), '" } }'))
  if (ncol(df) == 0) return(NULL)
  df
}


get_routeUploadHistory <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$aggregate(pipeline = paste0('[{ "$match": { "RemovidoEm": null } },',
                                       ' { "$group": {',
                                       '      "_id": { "Data": "$Data", "SC": "$SC", "CriadoPor": "$CriadoPor" },',
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
  df
}


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
  db$insert(df)
}


del_route <- function(data, SC, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "route",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$update(query  = paste0('{ "Data": { "$eq": "', as.character(data), '" },',
                                  '  "SC": { "$eq": "', SC, '" },',
                                  '  "RemovidoEm": null }'),
                  update = paste0('{ "$set": { "RemovidoPor": "', user, '" },',
                                  '  "$currentDate": { "RemovidoEm": true } }'),
                  multiple = T)
  df
}
