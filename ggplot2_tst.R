rm(list=ls())
require(ggplot2)
require(scales)

medians_mtcars <- data.frame("wt.median"=median(mtcars$wt))

# legend shows but linetype is wrong (solid)
p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + geom_vline(aes(xintercept=wt.median, linetype="dotted"),
                    data=medians_mtcars, show_guide=TRUE)
p

# linetype is correct but legend does not show
p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + geom_vline(aes(xintercept=wt.median),
                    data=medians_mtcars, show_guide=TRUE, linetype="dotted")
p

packageVersion("ggplot2")
packageVersion("scales")

#http://stackoverflow.com/questions/12545322/add-vline-to-existing-plot-and-have-it-appear-in-ggplot2-legend
#http://stackoverflow.com/questions/25253624/incorrect-linetype-in-legend-ggplot2-in-r

p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + geom_vline(aes(xintercept=wt.median, linetype="dotted"),
                    data=medians_mtcars, show_guide=TRUE)
p <- p + scale_linetype_identity(guide="legend", name="Stats", labels="median")
p

p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + geom_vline(aes(xintercept=wt.median, linetype="dotted"),
                    data=medians_mtcars, show_guide=TRUE)
p <- p + scale_linetype_manual(values="dotted")
p

stats_mtcars <- rbind(data.frame(type="median", "wt"=median(mtcars$wt)),
                      data.frame(type="mean", "wt"=mean(mtcars$wt)))
# legend shows but linetype is wrong (solid)
p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + geom_vline(aes(xintercept=wt, linetype=type),
                    data=stats_mtcars, show_guide=TRUE)
#p <- p + scale_linetype_manual(values="dotted")
p

