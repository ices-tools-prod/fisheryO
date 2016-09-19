# bquote('Example map with'~italic(.(word)))

# install.packages("formattable")
library(formattable)
library(DT)
library(dplyr)
# Get stock list
sl <- data.frame(STOCK.CODE = c("abc-123", "def-456", "ghi-789"),
                 STOCK.NAME = c("Name (Species name) and areas 8d and 7a (North and South)",
                                "Name (Species name) and areas 1 and 2 (East and West)",
                                "Name (Species name) and areas 3d and 16a (Up and down)"),
                 CAT = c(1, 2, 3),
                 stringsAsFactors = FALSE)


# Get stock summary
ss <- data.frame(STOCK.CODE = sl$STOCK.CODE,
                 F_2012 = c(1, 0, 1),
                 F_2013 = c(0, 0, 1),
                 F_2014 = c(1, 0, 1),
                 SSB_2013 = c(1, 0, 1),
                 SSB_2014 = c(1, 0, 1),
                 SSB_2015 = c(1, 0, 1),
                 stringsAsFactors = FALSE)

stockDat <- sl %>%
  left_join(ss, c("STOCK.CODE" = "STOCK.CODE"))

stockDat[stockDat == 1] <- as.character(shiny::icon('remove-sign'))
stockDat[stockDat == 0] <- as.character(shiny::icon('ok-sign'))


colContainer <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Stock code'),
      th(rowspan = 2, 'Stock name'),
      th(rowspan = 2, 'Data category'),
      th(colspan = 3, 'Fishing presshre'),
      th(colspan = 3, 'Stock size')
    ),
    tr(
      lapply(c(seq(2012, 2014),
               seq(2013, 2015)), th)
      )
  )
))
print(colContainer)

renderDataTable(datatable(stockDat,
          options = list(),
          class = "display",
          # callback = JS("return table;"),
          rownames = FALSE,
          # colnames,
          container = colContainer,
          caption = NULL,
          filter = c("none", "bottom","top")[2],
          escape = FALSE))

,
          escape = TRUE,
          style = "default",
          width = NULL, height = NULL,
          elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = c("multiple", "single", "none"),
          extensions = list(),
          plugins = NULL)



as.character(shiny::icon('ok-sign'))
as.character(shiny::icon('remove-sign', class = 'title = "Align Center"'))







tt <- formattable(stockDat,
            list(area(col = F_2012:SSB_2015) ~ formatter("span",
                                                         style = x ~ ifelse(x == "1",
                                                                            style("background-color" = "transparent",
                                                                                  "color" = "green"),
                                                                            style("background-color" = "transparent",
                                                                                  "color" = "red"))
                                                         ,
                                                         x ~ icontext(ifelse(x == "1",
                                                                             "glass",
                                                                             # "ok-sign",
                                                                             # "<img src='http://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/200px-Flag_of_the_People%27s_Republic_of_China.svg.png' height='52'></img>",
                                                                             # "remove-sign"))
                                                                             "fire"))
                                                         )
            ),
            col.names = c("Stock code", "Stock name", "Data category",
                          " \n 2012", "Fishing pressure \n 2013", " \n 2014",
                          " \n 2013", "Stock size \n 2014", " \n 2015"))


as.htmlwidget(tt)


formattable(stockDat,
            list(area(col = F_2012:SSB_2015) ~ formatter("span",
                                                         style = x ~ ifelse(x == "1",
                                                                            style("background-color" = "green",
                                                                                  "color" = "green"),
                                                                            style("background-color" = "red",
                                                                                  "color" = "red"))
            )
            ))


formatter("span",
          style = x ~ style(color = ifelse(x,
                                           "green",
                                           "red")),
          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))


df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny",
           "Hans", "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))
