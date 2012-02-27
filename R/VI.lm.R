VI.lm <-
function (x) 
{
    summary.lm(x)
    par(mfrow = c(2, 2))
    plot(x)
}
