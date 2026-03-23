test_that("bsocial class can be created with new()", {
  obj <- new("bsocial")
  expect_s4_class(obj, "bsocial")
  expect_equal(obj@id_proyecto, character(0))
  expect_equal(obj@cepas_seleccionadas, character(0))
  expect_type(obj@datos_crudos, "list")
  expect_s3_class(obj@datos_procesados, "data.frame")
  expect_type(obj@resultados_analisis, "list")
  expect_type(obj@graficos, "list")
})

test_that("bsocial slots can be set", {
  obj <- new("bsocial")
  obj@id_proyecto <- "test_project"
  obj@cepas_seleccionadas <- c("cepa_A", "cepa_B")
  expect_equal(obj@id_proyecto, "test_project")
  expect_equal(obj@cepas_seleccionadas, c("cepa_A", "cepa_B"))
})
