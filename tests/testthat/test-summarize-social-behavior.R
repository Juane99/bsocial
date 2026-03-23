test_that("summarize_social_behavior classifies strains", {
  obj <- create_curated_test_object()
  obj <- transform_curated_data(obj)
  obj <- analyze_social_behavior(obj)

  skip_if(!obj@resultados_analisis$social_behavior$success,
          "Social behavior analysis did not succeed with example data")

  obj <- summarize_social_behavior(obj)

  expect_true(!is.null(obj@resultados_analisis$summary_gen))
  expect_true(!is.null(obj@resultados_analisis$summary_gr))

  summary <- obj@resultados_analisis$summary_gen
  expect_true(all(c("positives", "negatives", "neutrals") %in% names(summary)))
})
