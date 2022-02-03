#' Valida CPF
#'
#' @param str_cpf character: cpf 
#'
#' @return T, se válido, F, caso contrário
isValidCPF <- function(str_cpf) {
  # Obtém apenas os números do CPF, ignorando pontuações
  num_cpf <- str_cpf %>% 
    as.character() %>% 
    str_extract_all("[0-9]+") %>%
    .[[1]] %>% 
    paste(collapse = "") %>% 
    strsplit("") %>% 
    .[[1]] %>% 
    as.integer()
  
  if (length(num_cpf) == 0) return(F)
  if (length(num_cpf) > 11) return(F)
  
  num_cpf <- tail(c(rep(0, 11), num_cpf), 11)
  
  # Validação do primeiro dígito verificador:
  dv_1 <- head(num_cpf, 9) %>% 
    `*`(c(10:2)) %>% 
    sum(.) %>% 
    `*`(10) %>% 
    `%%`(11) %>% 
    `%%`(10)
  
  if (num_cpf[10] != dv_1) return(F)
  
  # Validação do segundo dígito verificador:
  dv_2 <- head(num_cpf, 10) %>% 
    `*`(c(11:2)) %>% 
    sum(.) %>% 
    `*`(10) %>% 
    `%%`(11) %>% 
    `%%`(10)
  
  if (num_cpf[11] != dv_2) return(F)
  
  return(T)
}


#' Valida Email
#'
#' @param str_email character: email 
#'
#' @return T, se válido, F, caso contrário
isValidEmail <- function(str_email) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(str_email), ignore.case = T)
}