
eventUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Eventos", width = 12, id = "tabEventos", 
        tabPanel("Histórico de Eventos", 
                 withSpinnerTy(DT::DTOutput(ns("tbl_eventHistoric")))
                 )
      )
    )
  )
}

eventServer <- function(id, RV, rac_eventDB) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}
