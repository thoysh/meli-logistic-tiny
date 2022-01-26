
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
  db$insert(df)
}


drop_driver <- function() {
  flog.info("Função | %s", as.character(sys.call()[1]))
  
  # Connect to the database
  db = mongo(collection = "driver",
             db = Sys.getenv("MONGO_DB"), url = Sys.getenv("MONGO_URL"))
  # Read entries
  df <- db$remove(query = '{}')
  df
}
