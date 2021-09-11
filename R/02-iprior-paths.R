source("01-prelim.R")

theme_kernel_path <- theme_classic() +
  theme(legend.position = "none",
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        # axis.text = element_blank()
  )

kernel_path_canonical <- function(n = 1000, seed = 789, cen = TRUE,
                                  intercept = FALSE) {
  x <- seq(-1, 1, length = n)
  m <- 5  # no. of paths
  f.prior <- matrix(NA, nrow = n, ncol = m)
  if (!is.null(seed)) set.seed(seed)
  for (i in seq_len(m))
    f.prior[, i] <- (as.numeric(intercept) +
                       iprior::kern_canonical(x, centre = cen)) %*% rnorm(n)
  plot.df <- data.frame(x, f = f.prior)
  plot.df <- melt(plot.df, id = "x")
  p <- ggplot(plot.df, aes(x, value)) +
    geom_line(aes(col = variable))
  p <- p +
    labs(x = expression(italic(x)), y = expression(italic(f(x))),
         title = "Sample linear I-prior paths") +
    theme_kernel_path +
    scale_y_continuous(breaks = 0) +
    scale_x_continuous(breaks = 0)
  p
}

kernel_path_fbm <- function(n = 1000, seed = 789, hurst = 0.5, cen = TRUE,
                            intercept = FALSE) {
  the.title <- paste0("Sample fBm I-prior paths (Hurst = ", hurst, ")")
  x <- seq(-1, 1, length = n)
  m <- 5  # no. of paths
  f.prior <- matrix(NA, nrow = n, ncol = m)
  if (!is.null(seed)) set.seed(seed)
  for (i in seq_len(m))
    f.prior[, i] <- (as.numeric(intercept) +
                       iprior::kern_fbm(x, gamma = hurst, centre = cen)) %*% rnorm(n)
  plot.df <- data.frame(x, f = f.prior)
  plot.df <- melt(plot.df, id = "x")
  p <- ggplot(plot.df, aes(x, value)) +
    geom_line(aes(col = variable))
  p <- p +
    labs(x = expression(italic(x)), y = expression(italic(f(x))),
         title = the.title) +
    theme_kernel_path +
    scale_y_continuous(breaks = 0) +
    scale_x_continuous(breaks = 0)
  p
}

kernel_path_poly <- function(n = 1000, seed = 789, c = 0, d = 2, cen = TRUE,
                             intercept = 0.5) {
  the.title <- paste0("Sample polynomial I-prior paths (degree = ", d, ")")
  x <- seq(-1, 1, length = n)
  m <- 5  # no. of paths
  f.prior <- matrix(NA, nrow = n, ncol = m)
  if (!is.null(seed)) set.seed(seed)
  for (i in seq_len(m))
    f.prior[, i] <- (as.numeric(intercept) +
                       iprior::kern_poly(x, c = c, d = d, centre = cen)) %*% rnorm(n)
  plot.df <- data.frame(x, f = f.prior)
  plot.df <- melt(plot.df, id = "x")
  p <- ggplot(plot.df, aes(x, value)) +
    geom_line(aes(col = variable))
  p <- p +
    labs(x = expression(italic(x)), y = expression(italic(f(x))),
         title = the.title) +
    theme_kernel_path +
    scale_y_continuous(breaks = 0) +
    scale_x_continuous(breaks = 0)
  p
}

kernel_path_pearson <- function(n = 1000, seed = 789, th = FALSE) {
  x <- factor(sample(LETTERS[1:5], size = n, replace = TRUE))
  m <- 5  # no. of paths
  f.prior <- matrix(NA, nrow = n, ncol = m)
  if (!is.null(seed)) set.seed(seed)
  for (i in seq_len(m)) f.prior[, i] <- iprior::kern_pearson(x) %*% rnorm(n)
  plot.df <- data.frame(x, f = f.prior)
  plot.df <- plot.df[match(unique(plot.df$x), plot.df$x), ]
  plot.df <- melt(plot.df, id = "x")
  p <- ggplot(plot.df, aes(x, value)) +
    geom_point(aes(col = variable), size = 3, shape = 21, fill = NA, stroke = 1.2) +
    geom_point(aes(fill = variable, col = variable), size = 3, shape = 21, alpha = 0.5)
  if (isTRUE(th)) p <- p + theme_kernel_path_th
  else p <- p +
    labs(x = expression(italic(x)), y = expression(italic(f(x))),
         title = "Sample Pearson I-prior points") +
    theme_kernel_path +
    scale_y_continuous(breaks = 0)
  p
}

ggsave("kernel_path_canonical.pdf", kernel_path_canonical(), width = 4,
       height = 4)
ggsave("kernel_path_pearson.pdf", kernel_path_pearson(), width = 4,
       height = 4)
ggsave("kernel_path_fbm.pdf", kernel_path_fbm(), width = 4,
       height = 4)
