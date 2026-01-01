library(dplyr)

print("True values of theta given X")

truth <- tibble(
  x1 = c(0,0,0,0,1,1,1,1),
  x2 = c(0,0,1,1,0,0,1,1),
  x3 = c(0,1,0,1,0,1,0,1)
) |>
  mutate(
    sum_x = x1 + x2 + x3,
    theta  = case_when(
      (x1 + x2 + x3) == 0 ~ .1,
      (x1 + x2 + x3) == 1 ~ .8,
      (x1 + x2 + x3) == 2 ~ .2,
      (x1 + x2 + x3) == 3 ~ .9
    )
  ) |>
  arrange(sum_x) |>
  mutate(arm = 1:n()) |>
  print()


# Additive model
print("Coefficients of additive model")
as.matrix(round(coef(lm(theta ~ x1 + x2 + x3, data = truth)),3))
# Two-way interactions
print("Coefficients of 2-way interaction model")
as.matrix(round(coef(lm(theta ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3, data = truth)),3))
# Three-way interaction
print("Coefficients of 3-way (saturated) model")
as.matrix(round(coef(lm(theta ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 + x1:x2:x3, data = truth)),3))

# Ranges
print("Ranges of predictions from all 3 models")
rbind(
  additive = range(predict(lm(theta ~ x1 + x2 + x3, data = truth))),
  twoway = range(predict(lm(theta ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3, data = truth))),
  threeway = range(predict(lm(theta ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 + x1:x2:x3, data = truth))),
  truth = range(truth$theta)
)