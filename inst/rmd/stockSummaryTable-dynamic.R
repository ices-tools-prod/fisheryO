#' ---
#' title: ""
#' author: ""
#' date: ""
#' output: html_document
#' ---
#'
#'
# library(htmltools)
# library(DT)

f_range <- seq(active_year - 3,
               active_year - 1)
ssb_range <- seq(active_year - 2,
                 active_year)

colContainer <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Stock code'),
      th(rowspan = 2, 'Stock name'),
      th(rowspan = 2, 'Fish category'),
      # th(rowspan = 2, 'Ecoregion'),
      th(rowspan = 2, 'Advice type'),
      th(rowspan = 2, 'Data category'),
      th(rowspan = 2, 'Safe Biological Limits'),
      th(colspan = 3, 'F reference point'),
      th(colspan = 3, 'SSB reference point'),
      th(colspan = 3, 'MSFD descriptor')
    ),
    tr(
      lapply(c(f_range,
               ssb_range,
               'D3C1', 'D3C2', 'GES'), th)
    )
  )
))
#'
#'
#'

stockPlot[stockPlot == "GREEN"] <- "<i class=\"glyphicon glyphicon-ok-sign\" style=\"color:green; font-size:2.2em\"></i>"
stockPlot[stockPlot == "RED"] <- "<i class=\"glyphicon glyphicon-remove-sign\" style=\"color:red; font-size:2.2em\"></i>"
stockPlot[stockPlot == "GREY"] <- "<i class=\"glyphicon glyphicon-question-sign\" style=\"color:grey; font-size:2.2em\"></i>"
stockPlot[stockPlot == "ORANGE"] <- "<i class=\"glyphicon glyphicon-record\" style=\"color:#FAB700; font-size:2.2em\"></i>"

fullTable <- DT::datatable(stockPlot,
                       class = "display",
                       rownames = FALSE,
                       container = colContainer,
                       caption = NULL,
                       filter = c("none", "bottom","top")[3],
                       escape = FALSE,
                       extensions = 'Buttons',
                       options = list(
                         # dom = 'Bfrtip',
                         # buttons = c('csv', 'excel', 'pdf'), # need to work on formatting
                         pageWidth = '100%',
                         autoWidth = TRUE,
                         columnDefs = list(list(className = 'dt-center', targets = 4:14),
                                           list(orderable = FALSE, targets = 5:14),
                                           list(searchable = FALSE, targets = 5:14),
                                           list(width = '25%', targets = 'Stock name')
                                           )
                         )
                       )


return(fullTable)

# rmarkdown::render("~/git/ices-dk/FisheryO/vignettes/stockSummaryTable.Rmd",
#           output_file = "~/git/ices-dk/FisheryO/output/annexA_fullDynamic.html",
#           rmarkdown::html_document(template = NULL))

#'
