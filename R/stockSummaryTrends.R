
stockSummaryTrends <- function(df, overallMean = TRUE, plotDir = "~/git/ices-dk/fisheryO/output/", plotTitle = NULL) {

  for(pgGroup in unique(df$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
    all.pg <- df[df$pageGroup == pgGroup,]
    #
    if(any(names(all.pg) == "plotGroup") == FALSE) {
      all.pg$plotGroup <- "NONE"
    }
    #
    all.pg$plotGroup <- factor(all.pg$plotGroup)
    plotFileName = paste0(plotDir, "figure08_", gsub(", ", "-", pgGroup), ".png")
    # PLOT AVG
    png(filename = plotFileName,
        width = 89,
        height = 50.25 * length(unique(all.pg$plotGroup)),
        units = "mm",
        res = 300)

    par(mfrow = c(length(unique(all.pg$plotGroup)),1),
        mar = c(2.15, 2.25, 0.45, 0.25),
        oma = c(0, 0, 0.75, 0),
        usr = c(0, 1, 0, 1),
        mgp = c(0, .35, 0),
        tck = -0.01)

    # Order the lineGroup to make sure mean is plotted
    if(overallMean == TRUE) {
      if(any(all.pg$lineGroup == "MEAN")) {
        lineGroupOrd <- relevel(factor(unique(all.pg$lineGroup),
                                       ordered = F),
                                ref = "MEAN")
        if(length(lineGroupOrd) > 10) {
          lineGroupOrd <- factor(lineGroupOrd, levels = rev(levels(lineGroupOrd)))
        } # close >= 10
      } #  reorder lineGroupOrd if overallMean == T
      else {
        lineGroupOrd <- factor(unique(all.pg$lineGroup),
                               ordered = T)
      } # reorder lineGroupOrd if overallMean == F
    } # TRUE overallMean
    if(overallMean == FALSE ) {
      lineGroupOrd <- factor(unique(all.pg$lineGroup),
                             ordered = T)
    } # FALSE overallMean
    #
    for(plGroup in unique(levels(all.pg$plotGroup))) { # Data grouped by PLot (e.g., F or SSB)
      #
      all.pl <- all.pg[all.pg$plotGroup == plGroup,]
      yRange <- c(0, max(all.pl$plotValue, na.rm =T) + max(all.pl$plotValue, na.rm = T) * .15)
      xRange <- c(min(all.pl$Year[!is.na(all.pl$plotValue)]),
                  max(all.pl$Year[!is.na(all.pl$plotValue)]))
      #
      plot(NA,
           type = "l",
           ylim = yRange,
           xlim = xRange,
           yaxt = "n",
           xaxt = "n",
           ann = FALSE)
      abline(h = 1.0, lty = 2, col = "black", lwd = 1)
      #
      # Add lines according to the lnGroup
      for(lnGroup in levels(lineGroupOrd)) {
        if(all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          lnGroup <- NA
          next
        } # close next if all NA
        if(!all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
          d <- data.frame(all.pl$plotValue[all.pl$lineGroup == lnGroup],
                          all.pl$Year[all.pl$lineGroup == lnGroup])
          d <- d[order(d[,2]),]
          col.d <- as.character(unique(all.pl$COLOR[all.pl$lineGroup == lnGroup]))
          lin.d <- ifelse(lnGroup == "MEAN", 4, 2)
          lines(d[,2], d[,1], col = col.d, lwd = lin.d)
        } # close line plotting
      } # close lnGroup
      #
      # Label axes
      axis(1, at = pretty(xRange), cex.axis = .75)
      # mtext("Year", side = 1, line = 1.25, cex = .75)
      axis(2, at = pretty(yRange), cex.axis = .75, las = 1)
      # if(plGroup == "F") {
      #   mtext(expression("F/F"["average"]), side = 2, line = 1, cex= 1)
      # } # close F
      # if(plGroup == "SSB") {
      #   mtext(expression("SSB/SSB"["average"]), side = 2, line = 1, cex= 1)
      # } # close SSB
      if(plGroup == "F_FMSY") {
        mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex = .75)
      } # close F
      if(plGroup == "SSB_MSYBtrigger") {
        mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= .75)
      } # close SSB
      mtext(gsub(".*, ","", pgGroup), side = 3, outer = T, cex = .75)

      # Legend
      if(length(lineGroupOrd) <= 10) {
        legend("topright",
               legend = as.character(unique(all.pl$lineGroup)),
               fill = as.character(unique(all.pl$COLOR)),
               bty = "n",
               ncol = 3,
               cex = .5)
      } # Close less than 9 lines legend
      if(length(lineGroupOrd) > 10) {
        legend("topright",
               legend = "MEAN",
               fill = "grey40",
               bty = "n",
               ncol = 1,
               cex = .5)
      } # close more than 10 lines legend
    } # Close plGroup
    dev.off()
  }# Close pgGroup
} # Close function
