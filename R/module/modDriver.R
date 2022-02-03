## UI Module ##
driverUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Motoristas", width = 12, id = "tabDriver", 
        tabPanel("Lista de Motoristas", 
                 withSpinnerTy(DT::DTOutput(ns("tbl_driver")))
                 )
      )
    )
  )
}


## SERVER Module ##
driverServer <- function(input, output, session, RV, rac_driverDB) {
    
  ## OUTPUT Lista de Motoristas
  output$tbl_driver <- DT::renderDT({
    req(rac_driverDB())
    df <- rac_driverDB()
    
    datatableTy(df, nome = "Motoristas",
                class = "nowrap",
                escape = 0,
                options = list(scrollX = T),
                callback = JS("table.order([1, 'asc']).draw();")) %>% 
      # formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>%
      formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
  })
  
  
  return(RV)

}
