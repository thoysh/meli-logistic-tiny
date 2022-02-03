## UI Module ##
reportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(title = "Relatórios", width = 12, id = "tabReport", 
        tabPanel("Relatório Quinzenal",
                 fluidRow(
                   column(width = 4,
                          dateInput(ns("inDate_quinzenalDe"), "De", format = "dd/mm/yyyy", language = "pt-BR")),
                   column(width = 4,
                          disabled(dateInput(ns("inDate_quinzenalAte"), "Até", format = "dd/mm/yyyy", language = "pt-BR"))),
                   column(width = 4,
                          selectInput(ns("inSel_motoristaQuinzenal"), "Motorista", c(Selecione = "")))
                 ),
                 withSpinnerTy(DT::DTOutput(ns("tbl_reportBiweekly")))
                 )
      )
    )
  )
}


## SERVER Module ##
reportServer <- function(input, output, session, RV) {
  
  ## EVENTO Atualiza filtros do Relatório Quinzenal
  observeEvent(input$inDate_quinzenalDe, {
    dateDe <- input$inDate_quinzenalDe
    dateAte <- dateDe
    day(dateDe) <- ifelse(day(dateDe) >= 16, 16, 1)
    day(dateAte) <- ifelse(day(dateAte) >= 16, day(ceiling_date(dateAte, "month") %m-% days(1)), 15)
    updateDateInput(session = session, "inDate_quinzenalDe", value = dateDe)
    updateDateInput(session = session, "inDate_quinzenalAte", value = dateAte)
  })
  
  
  ## OUTPUT Relatório Quinzenal
  output$tbl_reportBiweekly <- DT::renderDT({
    
    # tabela <- rac_routeDB_wEventReport() %>%
    #   filter(`Tipo Dia` == "UTIL/SAB") %>%
    #   filter(Data >= input$inDate_quinzenalDe,
    #          Data <= input$inDate_quinzenalAte) %>%
    #   .[-1] %>%
    #   dt.set.df4js()
    
    
    # RV$upd_report
    # req(rac_reportDB())
    # df <- rac_reportDB()
    # df <- df %>% 
    #   mutate(Excluir = fshinySetInput(actionButton, df$`_id`, "btn_delreport",
    #                                   label = icon("times"),
    #                                   onclick = paste0('Shiny.setInputValue("', id, 
    #                                                    '-selBtn_delreport", this.id, {priority: "report"})')))
    # 
    # datatableTy(df, nome = "reportos",
    #             class = "nowrap",
    #             extensions = "FixedColumns",
    #             escape = ncol(df) - 1,
    #             options = list(scrollX = T,
    #                            columnDefs = list(list(visible = F, targets = c(0)))),
    #             callback = JS("table.order([5, 'asc']).draw();")) %>% 
    #   formatCurrency(names(df) %in% c("DescontoTitular", "AcrescimoTitular", "AcrescimoSuplente"),
    #                  currency = "", mark = ".", dec.mark = ",", digits = 2) %>%
    #   formatDate(names(Filter(is.Date, df)), method = "toLocaleDateString", params = "pt-BR") %>%
    #   formatDate(names(Filter(is.POSIXct, df)), method = "toLocaleString", params = "pt-BR")
  })
  
  
  return(RV)
  
}
