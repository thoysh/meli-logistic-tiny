#' Converte motoristas do excel em dataframe
#'
#' @param fileExcel character: caminho do arquivo
#'
#' @return dataframe com motoristas do excel
#'
#' @examples
#' etl_driverXlsx2Df("tests/motoristas.xlsx")
etl_driverXlsx2Df <- function(fileExcel) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  df <- tryCatch({
    if (!file.exists(fileExcel)) {
      stop("Arquivo não existe")
    } else {
      df <- read_excel(fileExcel,
                       range = cell_limits(c(1, 1), c(NA, 9)),
                       col_types = rep("text", 9),
                       col_names = T) %>%
        transmute(
          SC                 = `SC` %>% str_squish() %>% str_to_upper(),
          Grupo              = `Grupo` %>% str_squish() %>% str_to_upper(),
          Nome               = `Nome` %>% str_squish() %>% str_to_upper(),
          Apelido            = `Nome` %>% word(1),
          CPF                = `CPF` %>% as.character(),
          Email              = `E-mail` %>% str_squish(),
          Telefone           = `Telefone` %>% str_squish(),
          Pix                = `Chave Pix` %>% str_squish(),
          Placa              = `Placa do veículo` %>% str_squish() %>% str_to_upper(),
          Renavam            = `Revanam do veículo` %>% str_squish()
        ) #%>%
        #mutate(
        #  CPF_valido         = isValidCPF(CPF),
        #  Email_valido       = isValidEmail(Email)
        #)
    }
  }, error = function(err) {
    flog.error("Erro | %s", err$message)
    stop("Template ou encoding do arquivo errado")
  })
  
  df
}


#' Get motoristas
#'
#' @return dataframe de motoristas não-removidos
get_driver <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "driver",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$find(query = '{ "RemovidoEm": null }')
  if (ncol(df) == 0) return(NULL)
  df
}


#' Set motoristas
#'
#' @param df dataframe: dataframe para salvar no BD
#' @param user character: nome do usuário
#'
#' @return List(nInserted, nMatched, nRemoved, nUpserted, writeErrors)
set_driver <- function(df, user) {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  df <- df %>%
    mutate(
      CriadoEm  = Sys.time(),
      CriadoPor = user
    )
  # Connect to the database
  db = mongo(collection = "driver",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Insert the data into the mongo collection as a data.frame
  lst <- db$insert(df)
}


#' Drop collection de motoristas
#'
#' @return T
drop_driver <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "driver",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  bool <- db$remove(query = '{}')
}
