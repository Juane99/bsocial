test_that("analyze_social_behavior identifies monocultures and calculates fitness", {
  obj <- create_curated_test_object()
  obj <- transform_curated_data(obj)
  obj <- analyze_social_behavior(obj)

  sb <- obj@resultados_analisis$social_behavior
  expect_true(sb$success)
  expect_true(!is.null(sb$social_generations_plot))
  expect_true(!is.null(sb$social_gr_plot))
})
