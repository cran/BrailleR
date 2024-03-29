WTF =
    function() {
      MainTitle = SubTitle = XAxisLabel = YAxisLabel = NULL

      if (!is.null(dev.list()) &  # there must be an open graphics device
              length(grid::grid.ls(print = FALSE)$name) == 0) {  # if not grid
                                                                 # already, then
                                                                 # convert
        gridGraphics::grid.echo()
      }  # end open device condition

      if (length(grid::grid.ls(print = FALSE)$name) == 0) {
        .NoGraphicsDevice()
        GridLS = NULL
      } else {
        GridLS = grid::grid.ls(print = FALSE, flatten = TRUE)
      }


      # Now looking for things using the grobs on device ", dev.cur(), ".
      #print(GridLS)
      ## there is a dictionary listing of these in the gridGraphs paper.


      if (is.null(names(grid.get("graphics-plot-1-points-2")))) {
        PointSet =
            na.omit(
                data.frame(
                    x = unclass(grid.get("graphics-plot-1-points-1")$x),
                    y = unclass(grid.get("graphics-plot-1-points-1")$y),
                    pch = unclass(grid.get("graphics-plot-1-points-1")$pch)))
      } else {
        PointSet =
            na.omit(
                data.frame(
                    x = unclass(grid.get("graphics-plot-1-points-2")$x),
                    y = unclass(grid.get("graphics-plot-1-points-2")$y),
                    pch = unclass(grid.get("graphics-plot-1-points-2")$pch)))
      }

      # col=grid.get("graphics-plot-1-points-1")$col
      NPoints = nrow(PointSet)
      UniquePCH = unique(PointSet$pch)
      NPCH = length(UniquePCH)

      XAxisLabel = grid.get("graphics-plot-1-xlab-1")$label
      YAxisLabel = grid.get("graphics-plot-1-ylab-1")$label
      MainTitle = grid.get("graphics-plot-1-main-1")$label
      SubTitle = grid.get("graphics-plot-1-sub-1")$label

      cat('This graph has ')
      if (is.null(MainTitle)) {
        cat('no main title; ')
      } else {
        cat(paste0('the main title "', MainTitle, '"; '))
      }
      if (is.null(SubTitle)) {
        cat(' and o subtitle;\n')
      } else {
        cat(paste0('the subtitle "', SubTitle, '";\n'))
      }
      if (is.null(XAxisLabel)) {
        cat('No label on the x axis;\n')
      } else {
        cat(paste0('"', XAxisLabel, '" as the x axis label;\n'))
      }
      if (is.null(YAxisLabel)) {
        cat('No label on the y axis;\n')
      } else {
        cat(paste0('"', YAxisLabel, '" as the y axis label;\n'))
      }

      if (NPoints > 0) {
        cat("There are", NPoints, "points marked on this graph.\n")
      } else {
        cat('There are no points marked on the graph.\n')
      }



      ## I'd like to say something like "This window is empty" or
      ## "This window has <b> rectangles." etc. etc.

      ## I would guess that a window having boxes and lines might be a boxplot, while a window having no lines but rectangles is a histogram. etc.
      return(invisible(NULL))
    }
