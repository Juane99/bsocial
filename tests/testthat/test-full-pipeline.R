test_that("full curated workflow runs end-to-end", {
  obj <- create_curated_test_object()

  obj <- transform_curated_data(obj)
  expect_true(nrow(obj@datos_procesados) > 0)

  obj <- analyze_growth(obj)
  expect_s3_class(obj@graficos$growth_scatter, "ggplot")

  obj <- analyze_social_behavior(obj)
  skip_if(!obj@resultados_analisis$social_behavior$success)

  obj <- summarize_social_behavior(obj)
  expect_true(!is.null(obj@resultados_analisis$summary_gen))

  obj <- analyze_diversity(obj)
  expect_true(!is.null(obj@graficos$diversity_gen_plot))

  obj <- analyze_stability(obj)
  expect_true(!is.null(obj@graficos$stability_ngen_plot))

  obj <- analyze_biofilm_sequence(obj)
  expect_true(!is.null(obj@resultados_analisis$biofilm_gen_paths))
})
