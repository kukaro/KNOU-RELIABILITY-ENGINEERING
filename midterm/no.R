data <- c(5.2, 5.5, 5.5, 4.1, 3.6, 4.2, 4.3, 5.0, 4.1, 4.5,
          4.2, 4.8, 5.0, 4.5, 5.2, 5.5, 5.2, 5.2, 4.4, 4.9,
          4.5, 5.5, 5.5, 5.1, 5.6, 5.4, 4.8, 5.3, 5.0, 4.5,
          5.1, 4.8, 5.0, 5.4, 5.1, 5.6, 4.5, 5.3, 4.9, 5.1,
          5.1, 4.9, 5.2, 4.9, 4.7, 5.6, 5.5, 4.7, 5.0, 6.0)

pt <- hist(data)
pt
par(new = T)
ft <- plot(pt$density, type = 'o', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
ft

tbl <- table(data)

f_data <- NULL
for (i in 1:4) {
  distance <- 6
  dist_value <- (pt$density[i + 1] - pt$density[i]) / distance
  for (j in 0:(distance - 1)) {
    f_data <- c(f_data, pt$density[i] + dist_value * j)
  }
}
f_data <- c(f_data, pt$density[length(pt$density)])
