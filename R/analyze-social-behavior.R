#' Analyze Social Behavior (Fitness Effects)
#'
#' Compares the fitness of each strain in consortia vs its monoculture baseline.
#' Generates boxplots showing relative fitness for each strain across all consortia.
#'
#' @param .Object A \linkS4class{bsocial} object with \code{datos_procesados} populated.
#' @return The modified \linkS4class{bsocial} object with \code{resultados_analisis$social_behavior}.
#'
#' @export
setMethod("analyze_social_behavior", "bsocial", function(.Object) {
  bsocial_log("INFO", "analyze_social_behavior(): iniciando")

  df <- .Object@datos_procesados
  cepas <- .Object@cepas_seleccionadas

  pres_mat <- df[, cepas, drop = FALSE]
  monocultivos <- df[rowSums(!is.na(pres_mat)) == 1, ]

  if (nrow(monocultivos) < length(cepas)) {
    .Object@resultados_analisis$social_behavior <- list(
      success = FALSE,
      message = "Monocultures missing for some strains."
    )
    return(.Object)
  }

  info <- df

  prepare_boxplot_data <- function(data, monocultures, pos_name, all_strains) {
    nr <- nrow(data)
    nc <- length(all_strains)
    df_all <- data.frame(matrix(NA_real_, nrow = nr, ncol = 3 * nc))

    for (i in seq_len(nc)) {
      strain_name <- all_strains[i]
      monoculture_row <- monocultures[!is.na(monocultures[[strain_name]]), ]
      if (nrow(monoculture_row) == 0) next

      mono_val <- monoculture_row[[pos_name]][1]
      if (is.na(mono_val) || !is.finite(mono_val)) next

      all_fitness <- data[[pos_name]] / mono_val

      not_present_rows <- which(is.na(data[[strain_name]]))
      present_rows     <- which(!is.na(data[[strain_name]]))

      all_not_present <- rep(NA_real_, nr)
      all_present     <- rep(NA_real_, nr)

      all_not_present[not_present_rows] <- data[[pos_name]][not_present_rows] / mono_val
      all_present[present_rows]         <- data[[pos_name]][present_rows]     / mono_val

      df_all[, (i - 1) * 3 + 1] <- all_fitness
      df_all[, (i - 1) * 3 + 2] <- all_not_present
      df_all[, (i - 1) * 3 + 3] <- all_present
    }

    df_all
  }

  data_gen <- prepare_boxplot_data(info, monocultivos, "NGen", cepas)
  data_gr  <- prepare_boxplot_data(info, monocultivos, "GR",   cepas)

  generate_social_plot <- function(data_boxplot, cepas, tipo, ylab) {
    nr <- nrow(data_boxplot)
    nc <- length(cepas)

    if (nr == 0 || ncol(data_boxplot) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Insufficient data for boxplot", size = 4) +
          ggplot2::theme_void()
      )
    }

    labels <- c("all / monoculture", "not present / monoculture", "present / monoculture")

    mx <- suppressMessages(
      reshape2::melt(data_boxplot, measure.vars = colnames(data_boxplot))
    )

    mx$type <- rep(rep(labels, each = nr), times = nc)
    mx$pos  <- rep(cepas, each = nr * 3)

    mx <- mx[!is.na(mx$value) & is.finite(mx$value), , drop = FALSE]

    if (nrow(mx) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Insufficient data for boxplot", size = 4) +
          ggplot2::theme_void()
      )
    }

    ggplot2::ggplot(mx, ggplot2::aes(x = type, y = value, fill = type)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::facet_grid(. ~ pos) +
      ggplot2::labs(x = NULL, y = ylab, title = tipo) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  plot_gen <- generate_social_plot(data_gen, cepas, "Fitness over number of generations", "Fitness (NGen)")
  plot_gr  <- generate_social_plot(data_gr,  cepas, "Fitness over growth rate", "Fitness (GR)")

  result <- list(
    success = TRUE,
    data_gen = data_gen,
    data_gr = data_gr,
    social_generations_plot = plot_gen,
    social_gr_plot = plot_gr
  )

  .Object@resultados_analisis$social_behavior <- result
  .Object
})
