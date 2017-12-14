
library(ggplot2)
data("cars")

# ===== SCATTER PLOTS =====
# quick scatter plot
qplot(displ, hwy, data = mpg, color = drv)
# quick scatter plot with regression lines
qplot(displ, hwy, data = mpg, color = drv, geom = c("point","smooth"))
# quick scatter plot by sequence on data frame
qplot(y = hwy, data = mpg, color = drv)

# ===== BOXPLOT ====
# boxplot
qplot(drv, hwy, data = mpg, geom = "boxplot")
# in categories
qplot(drv, hwy, data = mpg, geom = "boxplot", color = manufacturer) 

# ===== HISTOGRAMS =====
qplot(hwy, data = mpg, fill = drv)

# ===== FACETS =====
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(hwy, data = mpg, facets = drv~., binwidth= 2)

# ===== GGPLOT ====
g <- ggplot(mpg, aes(displ,hwy))
# plot with linear regression
g + geom_point() + geom_smooth(method = "lm")
# plot with multiple facets
g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~drv)
# Aesthetics: Color and labels
g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~drv) + 
    ggtitle("This is the title")
g+geom_point(color="pink",size=4,alpha=1/2)
g+geom_point(size=4,alpha=1/2,aes(color=drv))
g + geom_point(aes(color = drv)) + labs(title = "Swirl Rules!", x = "Displacement" , y = "Hwy Mileage")
g + geom_point(aes(color = drv),size=2,alpha=1/2) + geom_smooth(size=4,linetype=3,method="lm",se=FALSE)
# Different background style
g + geom_point(aes(color = drv)) + theme_bw(base_family = "Times")

# Limits
# using a call to g + ylim will limit the range that is going to be plotted
# what you would need is 
#g + geom_line() + coord_cartesian(ylim = c(-3,3))

# Scatterplot factored by year
g <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(year)))
g + geom_point()

# a 4x5 plot
g + geom_point() + facet_grid(drv~cyl, margins = TRUE)
g + geom_point() + facet_grid(drv~cyl, margins = TRUE) + 
    geom_smooth(method = "lm", se = FALSE, size = 2, color = "black")
g + geom_point() + facet_grid(drv~cyl, margins = TRUE) + 
    geom_smooth(method = "lm", se = FALSE, size = 2, color = "black") + 
    labs(x = "Displacement", y = "Highway Mileage", title = "Title")
