#' ---	
#' title: ""	
#' author: ""	
#' date: ""	
#' output: html_document	
#' ---	
#' 	
#' 	
library(htmltools)	
library(DT)	
	
colContainer <- htmltools::withTags(table(	
  class = 'display',	
  thead(	
    tr(	
      th(rowspan = 2, 'Stock code'),	
      th(rowspan = 2, 'Stock name'),	
      th(rowspan = 2, 'Fishery guild'),	
      # th(rowspan = 2, 'Ecoregion'),	
      th(rowspan = 2, 'Advice type'),	
      th(rowspan = 2, 'Data category'),	
      th(rowspan = 2, 'Within Safe Biological Limits'),	
      th(colspan = 3, 'F reference point'),	
      th(colspan = 3, 'SSB reference point'),	
      th(colspan = 3, 'MSFD GES')	
    ),	
    tr(	
      lapply(c(seq(2013, 2015),	
               seq(2014, 2016),	
               'D3C1', 'D3C2', 'GES <sub>stock</sub>'), th)	
    )	
  )	
))	
#' 	
#' 	
#' 	
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
                         # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), # need to work on formatting	
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
