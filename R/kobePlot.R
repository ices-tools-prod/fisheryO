ecoregion <- unique(stock_status_full$EcoRegion)[10]
guild <- unique(stock_status_full$FisheriesGuild)[3]


plot_kobe <- function(ecoregion, guild = c("all", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch")[1], plotDir = "~/git/ices-dk/fisheryO/output/", plotTitle = NULL,
                      fig.width = 110, fig.height = 75, units = "mm", res = 300) {
  library(ggrepel)
  library(gridExtra)
  library(grid)
  library(ggiraph)


  plotName <- paste0(plotDir, "figure10_", ecoregion, "-", guild, ".png")
  #
  labTitle <- guild
  # guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
  if(any(guild %in% "all")) {
    guild <- c("benthic", "demersal", "pelagic", "crustacean", "elasmobranch")
    labTitle <- "All stocks"
  }

  kobeDat <- stock_status_full %>%
    filter(EcoRegion == ecoregion,
           FisheriesGuild == guild,
           !is.na(F_FMSY),
           !is.na(SSB_MSYBtrigger)) %>%
    group_by(StockCode) %>%
    mutate(max_bar = max(catches, landings, discards, na.rm = TRUE),
           catch_width = ifelse(is.na(catches),
                                0,
                                round((catches/(max_bar/1.25) * 100))),
           landings_width = ifelse(is.na(landings),
                                   0,
                                   round((landings/(max_bar/1.25) * 100))),
           discards_width = ifelse(is.na(discards),
                                   0,
                                   round((discards/(max_bar/1.25) * 100))))

  kobeDat$tip <- sprintf('
                        <div class="tipchart">
                          <h6>%s</h6>
                          <table>
                            <tr class="tiprow">
                             <td class="tipbarticks">F / F<sub>MSY</sub></td>
                             <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.2f&nbsp/&nbsp%3.2f&nbsp=&nbsp%3.2f</div></td>
                            </tr>
                            <tr class="tiprow">
                             <td class="tipbarticks">SSB / MSY B<sub>trigger</sub></td>
                             <td class="tipbardiv"><div class="tipbar" style="width:0px;">%3.0f&nbsp/&nbsp%3.0f&nbsp=&nbsp%3.2f</div></td>
                            </tr>
                            <tr class="tiprow">
                             <td class="tipbarticks">Catch (tonnes)</td>
                             <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                            </tr>
                            <tr class="tiprow">
                             <td class="tipbarticks">Landings (tonnes)</td>
                             <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                            </tr>
                            <tr class="tiprow">
                             <td class="tipbarticks">Discards (tonnes)</td>
                             <td class="tipbardiv"><div class="tipbar" style="width:%dpx;">%3.0f</div></td>
                            </tr>
                          </table>
                        </div>',
                         kobeDat$Description,
                         kobeDat$F, kobeDat$FMSY, kobeDat$F/kobeDat$FMSY,
                         kobeDat$SSB, kobeDat$MSYBtrigger, kobeDat$SSB/kobeDat$MSYBtrigger,
                         kobeDat$catch_width, kobeDat$catches,
                         kobeDat$landings_width, kobeDat$landings,
                         kobeDat$discards_width, kobeDat$discards)

  # javascript is too dumb to deal with line breaks in strings well
  kobeDat$tip <- gsub("\\\n", "", kobeDat$tip)
  # kobeDat$tip <- gsub(" ", "&nbsp", kobeDat$tip)


  if(nrow(kobeDat) != 0) {

    labs <- seq(0, max(kobeDat$F_FMSY, kobeDat$SSB_MSYBtrigger, na.rm = TRUE) + 1)
    dynamic = TRUE


    kobe_plot <- ggplot(kobeDat, aes(x = F_FMSY, y = SSB_MSYBtrigger,
                                     data_id = StockCode,
                                     tooltip = tip)) +
      geom_point(aes(color = colList,
                     size = catches),
                 alpha = 0.7) +
      geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
      geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
      geom_text_repel(aes(label = StockCode),
                      # box.padding = unit(.5, 'lines'),
                      # label.padding = unit(.5, 'lines'),
                      segment.size = .25,
                      force = 5,
                      size = 2) +
      scale_size("catches", range = c(1, 20)) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      scale_y_continuous(breaks = labs) +
      scale_x_continuous(breaks = labs) +
      coord_equal(xlim = range(labs), ylim = range(labs)) +
      labs(x = expression(F/F[MSY]),
           y = expression(SSB/MSY~B[trigger])) +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())


    # Lollipop plot
    catchBar <- stock_status_full %>%
      ungroup() %>%
      filter(EcoRegion == ecoregion,
             FisheriesGuild == guild) %>%
      arrange(!is.na(catches), catches, !is.na(landings), landings) %>%
      mutate(StockCode = factor(StockCode, StockCode))


    bar_plot <-
      ggplot(catchBar, aes(x = StockCode, y = catches)) +
      geom_segment(aes(x = StockCode, y = catches, xend = StockCode, yend = 0, color = colList), size = 2) +
      geom_point(stat = "identity", aes(y = catches, fill = colList), color = "grey50", shape = 24, size = 2, alpha = 0.8) +
      geom_point(stat = "identity", aes(y = landings, fill = colList), color = "grey50", shape = 21, size = 2, alpha = 0.8) +
      scale_fill_manual(values = c("GREEN" = "#4daf4a",
                                   "RED" = "#e41a1c",
                                   "GREY" = "#d3d3d3")) +
      scale_color_manual(values = c("GREEN" = "#4daf4a",
                                    "RED" = "#e41a1c",
                                    "GREY" = "#d3d3d3")) +
      labs(x = "Stock",
           y = "Catch and landings (tonnes)") +
      coord_equal() +
      coord_flip() +
      theme_bw(base_size = 7) +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line( size = 0.1, color = "grey80"))




    if(!dynamic) {
      kobe_plot <- kobe_plot +  geom_point(aes(color = colList,
                                               size = catches),
                                           alpha = 0.7)

      png(plotName, width = fig.width, height = fig.height, units = units, res = res)
      grid.arrange(kobe_plot,
                   bar_plot, ncol = 2, respect = TRUE, top = labTitle)
      dev.off()


    }

    if(dynamic) {
      kobe_plot <- kobe_plot +  geom_point_interactive(color = "white", fill = "white", shape = 21, size = 1, alpha = 0.01)

      suppressWarnings(
        rmarkdown::render("~/git/ices-dk/fisheryO/vignettes/kobe-dynamic.rmd",
                          output_file = paste0("~/git/ices-dk/fisheryO/output/TESTER-dynamic.html"),
                          envir = new.env())
      )
    }

  } else ("No stocks have MSY status")
}




#
#
# add.alpha <- function (hex.color.list, alpha) sprintf("%s%02X",hex.color.list,floor(alpha*256))
#
# kobePlot <- function(df, ecoregion, plotDir = "~/git/ices-dk/fisheryO/output/", plotTitle = NULL) {
#
#   numRows <- length(unique(ddt$FISHERIES.GUILD))
#   plotName <- paste0(plotDir, id, " Overview_figure8.png")
#
#   png(filename = plotName,
#       width = 172.4 * 2,
#       height = 172.4 * (numRows + 1),
#       units = "mm",
#       res = 600)
#
#   par(mfrow = c(1 + numRows, 2),
#       mar = c(2.15, 2.75, 0.45, 2.75),
#       oma = c(0, 0, 0, 0),
#       mgp = c(1.25, 0.35, 0),
#       tck = -0.01, ps = 1)
#   #
#   par(mfg = c(1, 1))
#   plot(NA,
#        xlim = c(0,5),
#        ylim = c(0,5),
#        xlab = expression("F / F"[MSY]),
#        ylab = expression("SSB / MSY B"[trigger]),
#        xaxs = "i",
#        yaxs = "i")
#   # add coloured regions
#   abline(h = 1, col = "grey60", lty = 2)
#   abline(v = 1, col = "grey60", lty = 2)
#   #plot bubbles
#   symbols(ddt$F_FMSY,
#           ddt$SSB_MSYBtrigger,
#           circles = ddt$cex,
#           inches = 1/3,
#           bg = add.alpha(ddt$colList, alpha = .5),
#           fg = add.alpha(ddt$colList, alpha = .8),
#           add = TRUE)
#
#   text(ddt$F_FMSY, ddt$SSB_MSYBtrigger, ddt$STOCK.CODE, col = "grey30", cex = .5)
#   #
#   # ddt$colList[is.na(ddt$colList)] <- "grey50"
#   dd <- ddt[order(ddt$CATCH),]
#   #
#   # set up main plot
#   par(mar = c(2.5, 4.25, 0.45, 2.75),
#       oma = c(0, 0, 0, 0),
#       mgp = c(1.25, 0.35, 0),
#       tck = -0.01,
#       new = T)
#
#   par(mfg = c(1,2))
#   barplot(dd$CATCH,
#           xlim = c(0, c(max(dd$CATCH) + max(dd$CATCH) * .05)),
#           axes = T,
#           col = dd$colList,
#           space = .5,
#           border = "grey80",
#           horiz = TRUE,
#           xlab = "Catches (tonnes)",
#           names.arg = dd$STOCK.CODE,
#           las = 1)
#   mtexti(text = "all stocks", side = 4, off = .1)
#   box()
#
#   # By type
#   for(i in 1:length(unique(ddt$FISHERIES.GUILD))) {
#     # png(filename = paste0("~/Fig7_", i, ".png"),
#     #     width = 172.4 * 2,
#     #     height = 172.4,
#     #     units = "mm",
#     #     res = 600)
#     GUILD.i <- unique(ddt$FISHERIES.GUILD)[i]
#     # set up main plot
#     par(mar= c(2.15, 2.75, 0.45, 2.75),
#         oma = c(0, 0, 0, 0),
#         mgp = c(1.25, 0.35, 0),
#         tck = -0.01,
#         new = TRUE)
#     #
#     par(mfg = c(1 + i, 1))
#     plot(NA,
#          xlim = c(0,5),
#          ylim = c(0,5),
#          xlab = expression("F / F"[MSY]),
#          ylab = expression("SSB / MSY B"[trigger]),
#          xaxs = "i",
#          yaxs = "i")
#     # add coloured regions
#     abline(h = 1, col = "grey60", lty = 2)
#     abline(v = 1, col = "grey60", lty = 2)
#     #plot bubbles
#     ddt.i <- ddt[ddt$FISHERIES.GUILD == GUILD.i,]
#     points(ddt.i$F.FMSY, ddt.i$SSB.Btrig, cex = ddt.i$cex, pch = 21, bg=ddt.i$colList, col=ddt.i$colList)
#     # text(ddt.i$F.FMSY, ddt.i$SSB.Btrig, ddt.i$STOCKID, col = "grey30")
#     text(ddt.i$textPoints, ddt.i$SSB.Btrig, ddt.i$STOCKID, col = "grey30")
#     ddt.i$colList[is.na(ddt.i$colList)] <- "grey50"
#     dd <- ddt.i[order(ddt.i$landings),]
#
#     # set up main plot
#     par(mar= c(2.5, 4.25, 0.45, 2.75),
#         oma = c(0, 0, 0, 0),
#         mgp = c(1.25, 0.35, 0),
#         tck = -0.01,
#         new = TRUE)
#     #
#     par(mfg = c(1 + i, 2))
#     barplot(dd$landings,
#             xlim = c(0, c(max(dd$landings) + max(dd$landings) * .05)),
#             axes = T,
#             col = dd$colList,
#             space = .5,
#             border = "grey80",
#             horiz = TRUE,
#             xlab = "Landings (tonnes)",
#             names.arg = dd$STOCKID,
#             las = 1)
#     mtexti(text = GUILD.i, side = 4, off = .1)
#     box()
#   }
#   dev.off()
# }
