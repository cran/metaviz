## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse =TRUE, comment ="#>")

## ------------------------------------------------------------------------
library(metaviz)

## ------------------------------------------------------------------------
head(mozart)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6, fig.width = 6----
rainforest(x = mozart[, c("d", "se")])

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = "Summary (Fixed effect)", xlab = "Cohen d")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = "Summary (Random Effects)", xlab = "Cohen d", method = "REM")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = "Summary (Fixed effect)", xlab = "Cohen d", confidence_level = 0.99)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], names = mozart[1:10, "study_name"], 
           summary_symbol = "rain")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], names = mozart[1:10, "study_name"], 
           summary_symbol = "diamond")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], names = mozart[1:10, "study_name"], 
           summary_symbol = "none")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = "Summary (Fixed effect)", xlab = "Cohen d", col = "Greys")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = "Summary (Fixed effect)", xlab = "Cohen d", col = "Greys",
           shading = FALSE)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], detail_level = 0.3)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], detail_level = 2)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 5.5, fig.width = 5.5----
rainforest(x = mozart[1:5, c("d", "se")], text_size = 1.5)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d", "se")], names = mozart[, "study_name"], 
           summary_name = c("Summary (published)", "Summary (unpublished)"), xlab = "Cohen d", 
           group = mozart[, "unpublished"])

## ------------------------------------------------------------------------
group <- interaction(mozart[, "unpublished"], mozart[, "rr_lab"])

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
rainforest(x = mozart[, c("d","se")], names = mozart[, "study_name"], 
           summary_name = c("Summary published/all other", "Summary unpublished/all other", 
                            "Summary published/RR lab", " Summary unpublished/RR lab" ),
           xlab = "Cohen d", group = group)

## ------------------------------------------------------------------------
library(metafor)

## ------------------------------------------------------------------------
res <- rma.uni(yi = mozart[, "d"], sei = mozart[, "se"])

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5, eval = FALSE----
#  rainforest(x = res, names = mozart[, "study_name"], xlab = "Cohen d")

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
res <- rma.uni(yi = mozart[, "d"], sei = mozart[, "se"], mods = ~mozart[, "unpublished"])

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5, eval = FALSE----
#  rainforest(x = res, names = mozart[, "study_name"], xlab = "Cohen d")

## ------------------------------------------------------------------------
library(ggplot2)
p <- rainforest(mozart[, c("d", "se")], names = mozart[, "study_name"])

## ------------------------------------------------------------------------
cum_summary <- cumsum((1/mozart[, "se"]^2)*mozart[, "d"])/(cumsum(1/mozart[, "se"]^2))
y <- nrow(mozart):1
cum_data <- data.frame(x = cum_summary, y = y)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
p + 
geom_path(data = cum_data, aes(y = y, x = x), col=" black ", size = 0.5) +
geom_point(data = cum_data, aes(y = y, x = x), col="black", size = 2, shape = 18)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
p + 
geom_vline(xintercept =  sum((1/mozart[, "se"]^2)*mozart[, "d"])/(sum(1/mozart[, "se"]^2)), 
           linetype = 3) +
theme(axis.title.x = element_text(size = rel(2), colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

## ------------------------------------------------------------------------
mozart_OR <- mozart
mozart_OR[, c("d", "se")] <- mozart_OR[, c("d", "se")]  * pi/sqrt(3)

## ---- dev='CairoPNG', fig.align = 'center', fig.height = 6.5, fig.width = 6.5----
p_OR <- rainforest(mozart_OR[, c("d", "se")], names = mozart_OR[, "study_name"])
p_OR + scale_x_continuous(name = "Odds Ratio", limits = c(-3, 4), breaks = -3:4, 
                          labels = function(x) {round(exp(x), 2)})

