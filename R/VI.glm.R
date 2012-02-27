VI.glm <-
function (x) 
{
    summary.glm(x)
    par(mfrow = c(2, 2))
    plot(x)
}
