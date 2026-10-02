#### Start ####

# Use the model result for plotting

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(tidySEM)
library(lavaan.mi)
library(ggplot2)
library(dplyr)

#### Data ####
dt1 <- readRDS("Output/m1.rds") 

#### Plotting (model with controls, with direct effects) ####
# Pooled estimates and keep regression coefficient only
est <- parameterEstimates.mi(dt1) |> 
  filter(op %in% c("~"))

# Significance levels
stars <- with(est, ifelse(pvalue < .001, "***",
                   ifelse(pvalue < .01,  "**",
                   ifelse(pvalue < .05,  "*", ""))))

# Edges
edg <- data.frame(
  from  = est$rhs,
  to    = est$lhs,
  label = paste0(formatC(est$est, format = "f", digits = 2), stars),
  sig   = est$pvalue < .05
)

# Node size
w <- 1.5   # box width  (observed)
h <- 0.6   # box height (observed)
r <- 0.5   # circle radius (latent)

# Variable labels
labs <- c(comtime = "Commuting time (hrs)",
          wfc     = "Work-family\nconflict",
          dep     = "Depression")

# Node position
nod <- expand.grid(var = c("comtime", "wfc", "dep"), wave = 1:3,
                   stringsAsFactors = FALSE) |>
  mutate(name   = paste0(var, "_w", wave),
         lab    = paste0(labs[var], "\nW", wave),
         x      = (wave - 1) * 3,
         y      = c(comtime = 3, wfc = 1.5, dep = 0)[var],
         latent = var %in% c("wfc", "dep"),
         hw     = ifelse(latent, r, w / 2))

# Circle outlines
theta <- seq(0, 2 * pi, length.out = 200)
circ <- filter(nod, latent) |>
  cross_join(data.frame(theta)) |>
  mutate(cx = x + r * cos(theta),
         cy = y + r * sin(theta))

#### Edge positions ####
edg <- edg |>
  left_join(select(nod, from = name, x0 = x, y0 = y, hw0 = hw), by = "from") |>
  left_join(select(nod, to   = name, x1 = x, y1 = y, hw1 = hw), by = "to") |>
  mutate(lag2 = (x1 - x0) > 3,              # spans two waves
         xs   = x0 + hw0,                   # right edge of source
         xe   = x1 - hw1,                   # left edge of target
         pos  = ifelse(y0 == y1, 0.5, 0.3), # cross-lagged labels near start
         lx   = xs + pos * (xe - xs),       # label x
         ly   = y0 + pos * (y1 - y0))       # label y

# Main paths
main <- filter(edg, !lag2)

# Lag-2 curve
curv <- 0.26   # bow size; flip the sign if it bends the wrong way
l2 <- filter(edg, lag2) |>
  mutate(
    lx = (xs + xe) / 2 + abs(curv) * (y1 - y0) / 2,
    ly = (y0 + y1) / 2 - abs(curv) * (xe - xs) / 2
  )

#### Plot ####
arr <- arrow(length = unit(0.2, "cm"), type = "closed")

p1 <- ggplot() +
  # Straight paths
  geom_segment(data = main,
               aes(x = xs, y = y0, xend = xe, yend = y1,
                   colour = sig, linetype = sig),
               arrow = arr, linewidth = .8) +
  # Curved lag-2 path
  geom_curve(data = l2,
             aes(x = xs, y = y0, xend = xe, yend = y1,
                 colour = sig, linetype = sig),
             curvature = curv, ncp = 20, arrow = arr, linewidth = .8) +
  # Boxes
  geom_tile(data = filter(nod, !latent), aes(x, y),
            width = w, height = h, fill = "white", colour = "black") +
  # Latent variables: circles
  geom_polygon(data = circ, aes(cx, cy, group = name),
               fill = "white", colour = "black") +
  # Labels for variables
  geom_text(data = nod, aes(x, y, label = lab), size = 4, lineheight = 2) +
  # Labels last, with white fill so they cut a gap in the lines
  geom_label(data = bind_rows(main, l2),
             aes(lx, ly, label = label),
             size = 4, fill = "white", border.colour = NA,
             label.padding = unit(0.2, "lines")) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"),
                      guide = "none") +
  scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"),
                        guide = "none") +
  coord_equal() +
  theme_void()

# Save the plot
ggsave(filename = "Output/p1.pdf", p1, width = 12, height = 8)

#### End ####