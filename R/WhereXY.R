WhereXY <-
function (x, y, grid = c(3, 3), Dist = "uniform") 
{
    XMin = min(x, na.rm = TRUE)
    YMin = min(y, na.rm = TRUE)
    XMax = max(x, na.rm = TRUE)
    YMax = max(y, na.rm = TRUE)
    XRange = XMax - XMin
    YRange = YMax - YMin
    if (Dist == "uniform") {
        XNew = cut(x, breaks = grid[1], labels = FALSE)
        YNew = cut(y, breaks = grid[2], labels = FALSE)
    }
    else {
        XMean = mean(x, na.rm = TRUE)
        YMean = mean(y, na.rm = TRUE)
        XSD = sd(x, na.rm = TRUE)
        YSD = sd(y, na.rm = TRUE)
        XBreaks = XMean + XSD * qnorm((1:(grid[1] - 1)/grid[1]))
        YBreaks = YMean + YSD * qnorm((1:(grid[2] - 1)/grid[2]))
        XNew = cut(x, breaks = XBreaks, labels = FALSE)
        YNew = cut(y, breaks = YBreaks, labels = FALSE)
    }
    Output = tapply(x, list(YNew, XNew), length)
    return(Output[rev(1:grid[2]), ])
}
