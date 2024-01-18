## not done by any stretch

kernel = c(0.5, 0.25, 0.25)
1 %*% kernel[1]
1:2 %*% kernel[1:2]
1:3 %*% kernel
2:4 %*% kernel
simple_sims(
  list(
    x ~ x + 1,
    y ~ convolution(x, kernel)
  ),
  time_steps = 10,
  mats = list(x = 0, y = empty_matrix, kernel = kernel)
) |> macpan2:::filter(time != 0, time != 11, matrix == "y")
