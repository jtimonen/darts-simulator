# Define dartboard radius
total_radius <- 451 / 2

# Define radii based on the actual dartboard dimensions
r_bullseye <- 6.35 / total_radius
r_outer_bull <- 16 / total_radius
r_double_inner <- 162 / total_radius
r_double_outer <- (162 + 8) / total_radius
r_triple_inner <- 99 / total_radius
r_triple_outer <- (99 + 8) / total_radius

# Main function
run_simulation <- function(n_throws = 1000, throw_std = 0.1, a) {
  # Create a new plot
  mm <- paste0("Throw std = ", round(a, 1), " x bullseye radius")
  plot(1,
    xlim = c(-1, 1), ylim = c(-1, 1), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1,
    main = mm
  )

  # Draw the bullseye, outer bull, triple ring, and double ring
  symbols(0, 0, circles = r_bullseye, inches = FALSE, add = TRUE, col = "gray90")
  symbols(0, 0, circles = r_outer_bull, inches = FALSE, add = TRUE, col = "gray90")
  symbols(0, 0, circles = r_triple_inner, inches = FALSE, add = TRUE, col = "gray90")
  symbols(0, 0, circles = r_triple_outer, inches = FALSE, add = TRUE, col = "gray90")
  symbols(0, 0, circles = r_double_inner, inches = FALSE, add = TRUE, col = "gray90")
  symbols(0, 0, circles = r_double_outer, inches = FALSE, add = TRUE, col = "gray90")

  # Actual order of numbers on a dartboard
  dartboard_numbers <- c(20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)

  # Add the numbers around the edge of the outer ring
  score_angles <- rep(0, 20)
  for (i in 1:20) {
    angle <- (i - 1) * 2 * pi / 20 - pi / 2
    angle <- -angle
    x <- 0.9 * cos(angle)
    y <- 0.9 * sin(angle)
    text(x, y, labels = dartboard_numbers[i], cex = 1.2)
    score_angles[i] <- angle - pi / 20
  }


  # Draw the radial lines for score zones
  for (i in 1:20) {
    angle <- score_angles[i]
    lines(c(0.07 * cos(angle), cos(angle)), c(0.07 * sin(angle), sin(angle)),
      col = "gray20"
    )
  }

  # From cartesian to polar
  coords_polar <- function(x, y) {
    r <- sqrt(x * x + y * y)
    theta <- atan2(y, x)
    list(r = r, theta = theta)
  }

  # Score multiplier
  score_multiplier <- function(r) {
    if (r > r_double_inner && r < r_double_outer) {
      multiplier <- 2
    } else if (r > r_triple_inner && r < r_triple_outer) {
      multiplier <- 3
    } else if (r > r_double_outer) {
      multiplier <- 0
    } else {
      multiplier <- 1
    }
    multiplier
  }


  # Board setup
  borders <- -pi / 20 + 1:19 * c(pi / 20) * 2 - pi
  score_numbers <- c(
    11, 8, 16, 7, 19, 3, 17, 2, 15, 10,
    6, 13, 4, 18, 1, 20, 5, 12, 9, 14
  )

  # Compute score
  score_dart <- function(x, y) {
    cp <- coords_polar(x, y)
    r <- cp$r
    th <- cp$theta
    multip <- score_multiplier(r)
    if (r < r_bullseye) {
      score <- 50
    } else if (r < r_outer_bull) {
      (
        score <- 25
      )
    } else {
      a <- which(borders < th)
      idx <- length(a) + 1
      if (th > (pi - pi / 20)) {
        idx <- 1
      }
      score <- score_numbers[idx]
    }
    multip * score
  }

  # Throws
  throw <- function(nt, aim, std, plot = TRUE) {
    mean <- aim
    scores <- rep(0, nt)
    for (j in 1:nt) {
      x <- rnorm(1, mean = mean[1], sd = std)
      y <- rnorm(1, mean = mean[2], sd = std)

      if (plot) {
        points(x, y, pch = 4, col = "firebrick2")
      }
      scores[j] <- score_dart(x, y)
      # text(x, y, labels = scores[j], cex = 0.9)
    }
    scores
  }


  nt <- n_throws
  aim_x <- rep(0, 21)
  aim_y <- rep(0, 21)
  rt <- (r_triple_inner + r_triple_outer) / 2
  aim_scores <- rep(0, 21)
  rrr <- rt

  for (j in 1:20) {
    th <- score_angles[j] + pi / 20
    aim_x[j + 1] <- rrr * cos(th)
    aim_y[j + 1] <- rrr * sin(th)
  }

  std <- throw_std
  for (j in 1:21) {
    aim <- c(aim_x[j], aim_y[j])
    scores <- throw(nt, aim, std, FALSE)
    aim_scores[j] <- mean(scores)
    lab <- round(aim_scores[j], 1)
    points(aim_x[j], aim_y[j], pch = 20, col = "blue")
    yy <- 1.3 * aim_y[j]
    if (j == 1) {
      yy <- r_outer_bull * 2
    }
    text(1.3 * aim_x[j], yy,
      labels = lab, cex = 1,
      col = "blue"
    )
  }
}

# Run
par(mfrow = c(2, 3))
nt <- 10000
mults <- c(0.25, 0.5, 1, 2, 5, 10)
for (m in mults) {
  ss <- m * r_bullseye
  run_simulation(nt, ss, m)
}
