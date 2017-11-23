
context('recompute')

test_that('maxError and maxAttempts options work', {
  set.seed(123)

  data("puzzles")
  result = anovaBF(RT ~ shape * color + ID, data = puzzles, whichRandom = "ID",
                   whichModels = 'top', iterations = 100, progress=FALSE)
  expect_true(all(result@bayesFactor$error > 0.01))

  recomputed_result <- recompute(result, maxError = 0.01, maxAttempts = 15, progress = FALSE)
  expect_true(all(recomputed_result@bayesFactor$error < 0.01))

  mc_recomputed_result <- recompute(result, maxError = 0.01, maxAttempts = 15, multicore = TRUE)
  expect_true(all(mc_recomputed_result@bayesFactor$error < 0.01))

  expect_warning(recompute(result, maxError = 0.01, maxAttempts = 2, progress = FALSE))


  npk_mixed <- npk
  npk_mixed$id <- factor(rep(1:(nrow(npk)/2), each = 2))
  npk_mixed$N <- factor(npk_mixed$N)
  npk_mixed$P <- factor(npk_mixed$P)
  npk_mixed$K <- factor(npk_mixed$K)
  result = anovaBF(yield ~ N * P * K + id, data = npk_mixed, whichRandom = "id",
                   whichModels = 'top', iterations = 100, progress=FALSE)
  expect_true(all(result@bayesFactor$error > 0.01))

  expect_warning(recomputed_result <- recompute(result, maxError = 0.01, maxAttempts = 15, progress = FALSE))

  expect_warning(rerecomputed_results <- recompute(recomputed_result, maxError = 0.01, maxAttempts = 5, progress = FALSE))

  rererecomputed_results <- recompute(rerecomputed_results, maxError = 0.01, maxAttempts = 15, progress = FALSE)
  expect_true(all(rererecomputed_results@bayesFactor$error < 0.01))
})
