#' Define template de tabela
#'
#' @param nome character: nome da tabela
#' 
datatableTy <- function(data, nome, options = NULL, extensions = NULL, rownames = F, selection = "none", dom = "Bfrtip",
                        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                        ...) {
  datatable(data = data,
            extensions = c("Buttons", extensions),
            rownames = rownames,
            selection = selection,
            options = append(list(
              ## Internationalisation
              language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"),
              ## Options
              dom = dom,
              lengthMenu = list(c(10, 25, 50, -1), paste(c("10", "25", "50", "Todas as"), "linhas")),
              ## Buttons
              buttons = list(list(extend = "pageLength", text = "Paginação"),
                             list(extend = "excel", title = nome, text = '<i class="fa fa-file-excel-o"></i>',
                                  exportOptions = list(columns = ":visible",
                                                       format = list(body = JS("function(data, row, column, node) {",
                                                                               "  data = $('<p>' + data + '</p>').text();",
                                                                               "  return ($.isNumeric(data.replace(/\\./g, '').replace(',', '.'))) ? data.replace(/\\./g, '').replace(',', '.') : data;",
                                                                               "}"))))),
              ## Callbacks
              preDrawCallback = preDrawCallback,
              drawCallback = drawCallback),
              options)
            )
}

#' Define columnDefs para substrings na coluna da tabela
#'
#' @param coluna numeric: número da coluna começando em 0
#' @param num_caract numeric: número de caracteres
#' 
dt.set.coldef_substr <- function(coluna, num_caract) {
  # Configura script
  partialcolumnDefs <- list(
    targets = coluna,
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > ", as.character(num_caract), " ?",
      "'<span title = \"' + data + '\">' + data.substr(0, ", as.character(num_caract - 2), ") + '...</span>' : data;",
      "}")
  )
}


#' Troca valores das colunas character e factor de NA para "" (recomendado para interação com JS)
#'
#' @param df dataframe
#' 
fdtSetDf4js <- function(df) {
  df[, sapply(df, class) %in% c("character", "factor")][is.na(df[, sapply(df, class) %in% c("character", "factor")])] = ""
  df
}
