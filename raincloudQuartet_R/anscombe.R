

# from:
# https://shinyapps.org/apps/RGraphCompendium/index.php#anscombes-quartet
#

# library(stats)
# library(graphics)
library(plotrix)

# summary(anscombe) -- now some 'magic' to do the 4 regressions in a
# loop:
ff <- y ~ x
for (i in 1:4) {
  ff[2:3] <- lapply(paste(c("y", "x"), i, sep = ""), as.name)
  ## or ff[[2]] <- as.name(paste('y', i, sep='')) ff[[3]] <-
  ## as.name(paste('x', i, sep=''))
  assign(paste("lm.", i, sep = ""), lmi <- lm(ff, data = anscombe))
  # print(anova(lmi))
}

op <- par(mfrow = c(2, 2), mar = 0.1 + c(4, 4, 1, 1), oma = c(0, 0, 2,
                                                              0), cex.lab = 1.5, font.lab = 1.5, cex.axis = 1.3, bty = "n", las = 1,
          cex.main = 1.5)

for (i in 1:4) {
  ff[2:3] <- lapply(paste(c("y", "x"), i, sep = ""), as.name)
  plot(ff, data = anscombe, col = "black", pch = 21, bg = "grey", cex = 2,
       xlim = c(3, 21), ylim = c(3, 13), ylab = "", xlab = "", axes = F)
  axis(1, at = seq(3, 21, 3))
  axis(2)
  text(15, 6, "r = .816", cex = 1.5)
  # mtext(ff[2:3][[1]], side=2, line=2.5, cex=1.3) #y-labels
  # mtext(ff[2:3][[2]], side=1, line=2.5, cex=1.3) #x-labels
  ablineclip(get(paste("lm.", i, sep = "")), x1 = 3, x2 = 21, col = "black",
             lwd = 2)
}

# mtext("Anscombe's Quartet", outer = TRUE, cex = 1.5)
par(op)

# Export as PDF
# Device size: 8.84, 6.74 (inches)



# Summary Statistics for Paper


