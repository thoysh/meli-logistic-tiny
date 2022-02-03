## UI Module ##
eventUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Eventos", width = 12, id = "tabEvent", 
        tabPanel("Histórico de Eventos",
                 withSpinnerTy(DT::DTOutput(ns("tbl_eventHistoric")))
                 ),
        
        tabPanel("Novo Evento", 
                 p("Para inserir um evento, preencha os campos necessários, em seguida, clique em 'Inserir evento'."),
                 fluidRow(
                   column(4, numericInput(ns("inNum_rota"), "Rota", value = NA)),
                   column(4, textInput(ns("inTxt_cluster"), "Cluster", value = NA)),
                   column(4, dateInput(ns("inDate_data"), "Data *", language = "pt-BR", format = "dd/mm/yyyy", value = NA))
                 ),
                 fluidRow(
                   column(4, selectInput(ns("inSel_tipoEvento"), "Tipo de evento *", c(Selecione = "", vec_eventType))),
                   column(4, selectInput(ns("inSel_tipoDesconto"), "Tipo de desconto", c(Selecione = "", vec_eventDiscountType)))
                 ),
                 fluidRow(
                   column(4, selectInput(ns("inSel_motoristaTitular"), "Motorista titular *", c(Selecione = ""))),
                   column(4, numericInput(ns("inNum_descontoTitular"), "Valor de desconto", 0, min = 0, step = 0.01)),
                   column(4, numericInput(ns("inNum_acrescimoTitular"), "Valor de acréscimo", 0, min = 0, step = 0.01))
                 ),
                 fluidRow(
                   column(4, selectInput(ns("inSel_motoristaSuplente"), "Motorista suplente", c(Selecione = ""))),
                   column(4, numericInput(ns("inNum_acrescimoSuplente"), "Valor de acréscimo", 0, min = 0, step = 0.01))
                 ),
                 textAreaInput(ns("inTxt_observacao"), "Observação", width = '100%'),
                 textOutput(ns("outTxt_validacao")),
                 actionButton(ns("btn_insertEvent"), "Inserir evento")
                 )
      )
    )
  )
}


## SERVER Module ##
eventServer <- function(input, output, session, RV, rac_routeDB, rac_eventDB) {

  ## OUTPUT Histórico de Eventos
  output$tbl_eventHistoric <- DT::renderDT({
    RV$upd_event
    req(rac_eventDB())
    df <- rac_eventDB()
    df <- df %>% 
      mutate(Excluir = fshinySetInput(actionButton, df$`_id`, "btn_delEvent",
                                      label = icon("times"),
                                      onclick = paste0('Shiny.setInputValue("', session$ns(""), 
                                                       'selBtn_delEvent", this.id, {priority: "event"})')))
    
    datatableTy(df, nome = "Eventos",
                class = "nowrap",
                extensions = "FixedColumns",
                escape = ncol(df) - 1,
                options = list(scrollX = T,
                               columnDefs = list(list(visible = F, targets = c(0)))),
                callback = JS("table.order([5, 'asc']).draw();")) %>% 
      formatCurrency(names(df) %in% c("DescontoTitular", "AcrescimoTitular", "AcrescimoSuplente"),
                     currency = "", mark = ".", dec.mark = ",", digits = 2) %>%
      formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>%
      formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
  })
  
  
  ## REACTIVE Valida Formulário de Evento
  rac_validateEvent <- reactive({
    if (isTruthy(input$inNum_rota)) {
      shiny::validate(need(input$inNum_rota %in% rac_routeDB()$Rota,
                           "Rota não existe!"))
    }
    shiny::validate(need(input$inDate_data,
                         "Data não pode ser vazio."))
    shiny::validate(need(input$inSel_tipoEvento,
                         "Tipo de evento não pode ser vazio."))
    if (input$inSel_tipoEvento == "Desconto") {
      shiny::validate(need(input$inSel_tipoDesconto,
                           "Tipo de desconto não pode ser vazio."))
    }
    shiny::validate(need(input$inSel_motoristaTitular != "",
                         "Motorista titular não pode ser vazio."))
    shiny::validate(need(!is.na(as.numeric(input$inNum_descontoTitular)),
                         "Valor de desconto do Motorista Titular deve ser numérico."))
    shiny::validate(need(as.numeric(input$inNum_descontoTitular) >= 0,
                         "Valor de desconto do Motorista Titular deve ser maior ou igual a zero."))
    shiny::validate(need(!is.na(as.numeric(input$inNum_acrescimoTitular)),
                         "Valor de desconto do Motorista Titular deve ser numérico."))
    shiny::validate(need(as.numeric(input$inNum_acrescimoTitular) >= 0,
                         "Valor de desconto do Motorista Titular deve ser maior ou igual a zero."))
    shiny::validate(need(!is.na(as.numeric(input$inNum_acrescimoSuplente)),
                         "Valor de desconto do Motorista Titular deve ser numérico."))
    shiny::validate(need(as.numeric(input$inNum_acrescimoSuplente) >= 0,
                         "Valor de desconto do Motorista Titular deve ser maior ou igual a zero."))
  })
  output$outTxt_validacao <- renderText({ rac_validateEvent() })
  
  
  ## EVENTO Atualiza parcialmente Formulário de Evento
  observeEvent(input$inNum_rota, {
    req(rac_routeDB())
    df <- rac_routeDB() %>%
      filter(Rota == input$inNum_rota)

    if (!isTruthy(input$inNum_rota)) {
      enable(id = "inTxt_cluster")
      enable(id = "inDate_data")
      enable(id = "inSel_motoristaTitular")
    } else {
      updateTextInput(session = session, "inTxt_cluster", value = df$Cluster)
      updateDateInput(session = session, "inDate_data", value = df$Data)
      updateSelectInput(session = session, "inSel_motoristaTitular", choices = df$Motorista)
      disable(id = "inTxt_cluster")
      disable(id = "inDate_data")
      disable(id = "inSel_motoristaTitular")
    }
  })
  
  
  ## BOTAO Salva evento no BD
  observeEvent(input$btn_insertEvent, {
    isolate({
      rac_validateEvent()
      if (input$btn_insertEvent != 0) {
        tryCatch({
          df <- data.frame(Rota              = input$inNum_rota %>% as.character(),
                           Cluster           = input$inTxt_cluster,
                           Data              = input$inDate_data %>% as.Date(),
                           TipoEvento        = input$inSel_tipoEvento,
                           TipoDesconto      = input$inSel_tipoDesconto,
                           MotoristaTitular  = input$inSel_motoristaTitular,
                           DescontoTitular   = input$inNum_descontoTitular %>% as.numeric(),
                           AcrescimoTitular  = input$inNum_acrescimoTitular %>% as.numeric(),
                           MotoristaSuplente = input$inSel_motoristaSuplente,
                           AcrescimoSuplente = input$inNum_acrescimoSuplente %>% as.numeric(),
                           Observacao        = input$inTxt_observacao) %>%
            mutate(across(where(is.factor), as.character)) %>%
            mutate(across(where(is.character), str_squish))
          
          set_event(df, user = RV$user)
          shinyalertTy("Sucesso na inserção", type = "success")
          
          RV$upd_event <- RV$upd_event + 1
        
        }, error = function(e) {
          flog.error("btn_insertEvent (E) | %s", e$message)
          shinyalertTy("Erro na inserção", type = "error")
        })
      }
    })
  })
  
  
  ## BOTAO Exclui eventos
  observeEvent(input$selBtn_delEvent, {
    selId <- strsplit(input$selBtn_delEvent, "-")[[1]][-1]
    
    del_event(id = selId, user = RV$user)
    shinyalertTy("Sucesso na exclusão", paste0("Evento id ", selId), type = "success")
    RV$upd_event <- RV$upd_event + 1
  })
  

  return(RV)

}
