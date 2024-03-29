source("01-prelim.R")
load("data/btb.Rdata")

# load("data/BTBppp.RData")
# plot.df <- as.data.frame(pppdata$window)
# W <- pppdata$window
# simpW <- simplify.owin(W, 1000)  # This reduces resolution of plot
# plot.df <- as.data.frame(W)
#
# # Get data points (location, spoligotypes and year)
# btb <- read.table("data/BTB_spoligotype_data.txt", header = FALSE, sep = " ",
#                   skip = 1)
# colnames(btb) <- c("x", "y", "year", "sp")
# btb$sp <- factor(btb$sp)
#
# # Keep the four largest classes
# btb %>%
#   group_by(sp) %>%
#   tally() %>%
#   arrange(desc(n)) -> btb.n
# (classes.to.keep <- as.character(btb.n$sp[1:4]))
# levels(btb$sp) <- c("Sp9",    "Others", "Others", "Sp12" ,  "Sp15",
#                     "Others", "Sp20",   "Others", "Others", "Others")
# btb$sp <- factor(btb$sp, levels = c("Sp9", "Sp12", "Sp15", "Sp20", "Others"))
#
# # A similar table to Diggle et al. (2005)
# btb.summary <- btb %>%
#   group_by(year, sp) %>%
#   arrange(year) %>%
#   tally() %>%
#   complete(sp, year, fill = list(n = 0)) %>%
#   reshape2::dcast(year ~ sp)
#
# # Group the years into 4 categories: 1) < 1997; 2) 1997-1998; 3) 1999-2000; 4)
# # 2001-02
# btb_year_fn <- function(x) {
#   res <- x
#   res[x < 1997] <- "< 1997"
#   res[x == 1997 | x == 1998] <- "1997-1998"
#   res[x == 1999 | x == 2000] <- "1999-2000"
#   res[x == 2001 | x == 2002] <- "2001-2002"
#   res
# }
# as.tibble(btb) %>%
#   mutate(period = factor(btb_year_fn(year))) -> btb
#
# ## ---- plot.cow ----
# as.tibble(btb.summary) %>%
#   reshape2::melt(id.vars = "year") %>%
#   ggplot(aes(year, value, col = variable)) +
#   geom_segment(aes(xend = year, yend = 0), size = 12) +
#   scale_x_continuous(breaks = seq(1989, 2002, by = 1),
#                      minor_breaks = seq(1989, 2002, by = 1)) +
#   labs(x = "Year", y = "Count") +
#   scale_color_discrete(name = "Spoligotype") +
#   theme_bw() +
#   guides(col = guide_legend(override.aes = list(size = 6)))
#
# ## ---- plot.cornwall ----
# ggplot() +
#   geom_polygon(data = plot.df, aes(x, y, group = id), fill = NA, col = "grey25") +
#   geom_point(data = btb, aes(x, y, col = sp)) +
#   labs(x = "Eastings (1,000 km)", y = "Northings (1,000 km)",
#        col = "Spoligotype") +
#   scale_x_continuous(labels = function(x) x / 1000) +
#   scale_y_continuous(labels = function(x) x / 1000) +
#   theme_bw() +
#   theme(legend.position = c(0.98, 0.005), legend.justification = c(1, 0))
#
# ## ---- mod.btb ----
# Spoligotype <- btb$sp
# X <- scale(btb[, 1:2])
# mu.x <- attr(X, "scaled:center")
# sd.x <- attr(X, "scaled:scale")
# year <- scale(btb$year, scale = FALSE)
# mu.year <- attr(year, "scaled:center")
# period <- btb$period
#
# ## ---- mod.btb.new ----
# mod0 <- iprobit(y = Spoligotype, X)  # constant model (manipulated iprobit fn)
# # > mod0
# # Training error rate: 46.25 %
# # Lower bound value: -1197.43
# #
# # Class = 1 Class = 2 Class = 3 Class = 4 Class = 5
# # Intercept  -0.05879  -1.17894  -0.90297  -1.20836  -1.68314
# # res         0.00000   0.00000   0.00000   0.00000   0.00000
#
# mod1 <- iprobit(y = Spoligotype, X, kernel = "fbm",  # spatial model
#                 control = list(maxit = 200, restarts = TRUE, par.maxit = 20))
#
# # > mod1
# # Training error rate: 19.59 %
# # Lower bound value: -665.3313
# #
# # Class = 1 Class = 2 Class = 3 Class = 4 Class = 5
# # Intercept   1.24948  -0.55276  -0.15134  -0.91544  -0.24708
# # res         0.17807   0.17807   0.17807   0.17807   0.17807
#
# mod2 <- iprobit(y = Spoligotype, X, period, kernel = "fbm",  # spatio-period model
#                 interactions = "1:2", control = list(maxit = 200, restarts = TRUE, par.maxit = 20))
# # > mod2
# # Training error rate: 18.5 %
# # Lower bound value: -664.65
# #
# # Class = 1 Class = 2 Class = 3 Class = 4 Class = 5
# # Intercept    2.18919   0.30092   0.74937  -0.11294   0.65687
# # lambda[1,]   0.15575   0.15575   0.15575   0.15575   0.15575
# # lambda[2,]  -0.00376  -0.00376  -0.00376  -0.00376  -0.00376
#
# mod3 <- iprobit(y = Spoligotype, X, year, kernel = "fbm",  # spatio-temp model
#                 interactions = "1:2", control = list(maxit = 200, restarts = TRUE, par.maxit = 20))
#
# # > mod3
# # Training error rate: 17.95 %
# # Lower bound value: -656.2398
# #
# # Class = 1 Class = 2 Class = 3 Class = 4 Class = 5
# # Intercept    1.46258  -0.46739   0.02123  -0.84227  -0.02781
# # lambda[1,]   0.15698   0.15698   0.15698   0.15698   0.15698
# # lambda[2,]   0.00614   0.00614   0.00614   0.00614   0.00614
#
# ## ---- plot.btb.prep ----
# # Obtain points inside the polygon
# rescalee <- function(x) {
#   res <- x * rep(sd.x, each = nrow(x)) + rep(mu.x, each = nrow(x))
#   colnames(res) <- c("x", "y")
#   res
# }
# X.var <- 1:2
# maxmin <- cbind(apply(X, 2, min), apply(X, 2, max))
# xx <- list(NULL)
# for (j in 1:2) {
#   mm <- maxmin[X.var[j], ]
#   xx[[j]] <- seq(from = mm[1] - 1, to = mm[2] + 1, length.out = 500)
# }
# mm <- maxmin[X.var, ]
# x.df.full <- expand.grid(xx[[1]], xx[[2]])
# tmp <- x.df.full * rep(sd.x, each = nrow(x.df.full)) +
#   rep(mu.x, each = nrow(x.df.full))
# isin <- inside.owin(tmp[, 1], tmp[, 2], W)
# x.df <- x.df.full[isin, ]
#
# # Calculate fitted probabilities
# fill.col <- iprior::gg_col_hue(5)
# N <- nrow(x.df)
# a <- predict(mod1, list(x.df))  # spatial-model
#
# period.df <- data.frame(
#   factor(rep(levels(btb$period)[1], N), levels = levels(btb$period)),
#   factor(rep(levels(btb$period)[2], N), levels = levels(btb$period)),
#   factor(rep(levels(btb$period)[3], N), levels = levels(btb$period)),
#   factor(rep(levels(btb$period)[4], N), levels = levels(btb$period))
# )
#
# a1 <- predict(mod2, list(x.df, period.df[, 1]))
# a2 <- predict(mod2, list(x.df, period.df[, 2]))
# a3 <- predict(mod2, list(x.df, period.df[, 3]))
# a4 <- predict(mod2, list(x.df, period.df[, 4]))
#
# b <- list(NULL)
# unq.year <- sort(as.numeric(unique(year)))
# for (i in seq_along(unique(year))) {
#   b[[i]] <- predict(mod3, list(x.df, matrix(unq.year[i], nrow = N, ncol = 1)))$prob
#   print(i)
# }

## ---- plot.btb ----
# Function to plot for SPATIAL MODEL
plot_spatial_model <- function(m = 1, method = "bottom.pieces", points = FALSE,
                               contour.labels = FALSE) {
  current.label <- paste0("Spoligotype ", gsub("Sp", "", levels(btb$sp)[m]))
  contour.df <- cbind(rescalee(x.df), prob = a$prob[, m])
  ggplot(data = contour.df, aes(x, y)) +
    geom_raster(aes(fill = prob)) -> v

  v <- v + geom_contour(aes(z = prob), col = "grey20", size = 0.3) +
    geom_polygon(data = plot.df, aes(x, y, group = id), fill = NA,
                 col = "grey25") +
    labs(x = "Eastings (1,000 km)", y = "Northings (1,000 km)") +
    scale_x_continuous(labels = function(x) x / 1000) +
    scale_y_continuous(labels = function(x) x / 1000) +
    scale_fill_continuous(name = NULL, low = "white", high = fill.col[m],
                          limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme_bw() +
    annotate("text", x = min(plot.df$x), y = max(plot.df$y),
             label = current.label, vjust = 1, hjust = 0) +
    theme(panel.grid = element_blank())

  if (isTRUE(points)) {
    v <- v + geom_point(data = subset(btb, sp == levels(btb$sp)[m]), aes(x, y),
                        col = "grey30", shape = 21, fill = fill.col[m], size = 1)
  }

  if (isTRUE(contour.labels)) {
    v <- v + directlabels::geom_dl(aes(z = prob, label = ..level..),
                                   stat = "contour", colour = "grey20",
                                   method = list("far.from.others.borders",
                                                 "calc.boxes", "draw.rects",
                                                 cex = 0.65)) +
      guides(fill = FALSE)
  } else {
    v <- v + guides(fill = guide_colorbar(barwidth = 0.45, barheight = 16))
  }

  v
}

p1 <- plot_spatial_model(1, points = TRUE, contour.labels = TRUE)
p2 <- plot_spatial_model(2, points = TRUE, contour.labels = TRUE)
p3 <- plot_spatial_model(3, points = TRUE, contour.labels = TRUE)
p4 <- plot_spatial_model(4, points = TRUE, contour.labels = TRUE)
cowplot::plot_grid(p1, p2, p3, p4, ncol = 2,
                   labels = c("(a)", "(b)", "(c)", "(d)"), label_size = 10,
                   label_fontface = "plain")

ggsave("btb_spat_1.pdf", p1, width = 5, height = 5)
ggsave("btb_spat_2.pdf", p2, width = 5, height = 5)
ggsave("btb_spat_3.pdf", p3, width = 5, height = 5)
ggsave("btb_spat_4.pdf", p4, width = 5, height = 5)

## ---- plot.temporal.btb ----
# Function to plot for SPATIO-TEMPORAL MODEL
mean.year <- attr(year, "scaled:center")
plot_stemporal_model <- function(year = 1, points = TRUE,
                                 mod = c("period", "temporal")) {
  mod <- match.arg(mod, c("period", "temporal"))

  if (mod == "period") {
    current.period <- levels(btb$period)[year]
    current.label <- paste0("Year: ", current.period)
    alphaa <- 0.95
    if (year == 1)
      contour.df <- cbind(rescalee(x.df), prob = a1$prob)
    if (year == 2)
      contour.df <- cbind(rescalee(x.df), prob = a2$prob)
    if (year == 3)
      contour.df <- cbind(rescalee(x.df), prob = a3$prob)
    if (year == 4)
      contour.df <- cbind(rescalee(x.df), prob = a4$prob)
  } else if (mod == "temporal") {
    current.period <- as.integer(mean.year + unq.year[year])
    current.label <- paste0("Year: ", current.period)
    alphaa <- 0.95
    contour.df <- cbind(rescalee(x.df), prob = b[[year]])
  }

  # Add first layer ------------------------------------------------------------
  p <- ggplot(contour.df, aes(x, y)) +
    geom_raster(aes(alpha = alphaa * contour.df[, 2 + 1]), fill = fill.col[1]) +
    scale_alpha_continuous(range = c(0, alphaa))

  # Add subsequent layers ------------------------------------------------------
  for (j in 2:4) {
    p <- p +
      annotate(geom = "raster", x = contour.df$x, y = contour.df$y,
               alpha = alphaa * contour.df[, 2 + j], fill = fill.col[j])
  }

  # Add decision boundaries ----------------------------------------------------
  tmp.df <- reshape2::melt(contour.df, id.vars = c("x", "y"))
  p <- p +
    geom_contour(data = tmp.df, aes(x, y, z = value, col = variable), size = 1,
                 linetype = "dashed", binwidth = 0.1,
                 breaks = seq(0.5, 0.5, by = 0.1)) +
    scale_colour_manual(values = iprior::gg_colour_hue(5)[1:4])

  # # Add contour labels ---------------------------------------------------------
  # for (j in 1:4) {
  #   p <- p +
  #     geom_dl(data = contour.df, aes(x, y, z = contour.df[, 2 + j],
  #                                    label = ..level..), col = "grey20",
  #             stat = "contour", method = list("top.pieces", cex = 0.65),
  #             breaks = seq(0.5, 0.9, by = 0.1))
  # }

  # Add points -----------------------------------------------------------------
  if (isTRUE(points)) {
    if (mod == "period") {
      as.tibble(btb) %>%
        subset(sp != "Others" & period == current.period) -> points.df
    } else if (mod == "temporal") {
      as.tibble(btb) %>%
        subset(sp != "Others" & year == current.period) -> points.df
    }
    p <- p +
      geom_point(data = points.df, aes(x, y, fill = sp), col = "grey15",
                 shape = 21, size = 2) +
      scale_fill_manual(values = fill.col[1:4])
  }

  p +
    geom_polygon(data = plot.df, aes(x, y, group = id), fill = NA,
                 col = "grey25") +
    labs(x = "Eastings (1,000 km)", y = "Northings (1,000 km)") +
    scale_x_continuous(labels = function(x) x / 1000) +
    scale_y_continuous(labels = function(x) x / 1000) +
    theme_bw() +
    guides(colour = FALSE, alpha = FALSE,
           fill = guide_legend(nrow = 2,
                               title = "Spoligotype",
                               direction = "horizontal",
                               title.position = "top",
                               override.aes = list(size = 2))) +
    theme(legend.position = c(0.99, 0.01), legend.justification = c(1, 0),
          legend.title = element_text(size = 8), legend.title.align = 0.5,
          panel.grid = element_blank()) +
    # legend.box.background = element_rect(fill = NA)) +
    annotate("text", x = min(plot.df$x), y = max(plot.df$y),
             label = current.label, vjust = 1, hjust = 0)
}

p1 <- plot_stemporal_model(1)
p2 <- plot_stemporal_model(2)
p3 <- plot_stemporal_model(3)
p4 <- plot_stemporal_model(4)
cowplot::plot_grid(p1, p2, p3, p4, ncol = 2,
                   labels = c("(a)", "(b)", "(c)", "(d)"), label_size = 10,
                   label_fontface = "plain")

ggsave("btb_1.pdf", p1, width = 5, height = 5)
ggsave("btb_2.pdf", p2, width = 5, height = 5)
ggsave("btb_3.pdf", p3, width = 5, height = 5)
ggsave("btb_4.pdf", p4, width = 5, height = 5)

## ---- gif ----
ggsave("btb_st_1.pdf", plot_stemporal_model(1, mod = "temporal"), width = 5, height = 5)
ggsave("btb_st_2.pdf", plot_stemporal_model(5, mod = "temporal"), width = 5, height = 5)
ggsave("btb_st_3.pdf", plot_stemporal_model(10, mod = "temporal"), width = 5, height = 5)
ggsave("btb_st_4.pdf", plot_stemporal_model(14, mod = "temporal"), width = 5, height = 5)

makeplot <- function() {
  for (i in seq_along(unq.year)) {
    p <- plot_stemporal_model(i, mod = "temporal")
    print(p)
  }
  animation::ani.pause()
}
animation::saveGIF(makeplot(), interval = 1, ani.width = 600, ani.height = 550)
