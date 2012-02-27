VI.aov <-
function (x) 
{
    summary.aov(x)
    par(mfrow = c(2, 2))
    plot(x)
}
