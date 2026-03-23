#' S4 Class for Microbial Social Behavior Analysis
#'
#' The \code{bsocial} class encapsulates data and results for analyzing microbial
#' social behavior in bacterial consortia.
#'
#' @slot id_proyecto Character. Project identifier.
#' @slot cepas_seleccionadas Character vector. Names of the selected strains.
#' @slot datos_crudos List. Raw input data (plates, curated, consortia).
#' @slot datos_procesados Data frame. Processed metrics (NGen, GR, LogPhase).
#' @slot resultados_analisis List. Analysis results from each pipeline step.
#' @slot graficos List. Generated ggplot2 plots and plotting functions.
#'
#' @export
#' @examples
#' obj <- new("bsocial")
#' obj@id_proyecto <- "my_experiment"
setClass("bsocial",
         representation = representation(
           id_proyecto = "character",
           cepas_seleccionadas = "character",
           datos_crudos = "list",
           datos_procesados = "data.frame",
           resultados_analisis = "list",
           graficos = "list"
         ),
         prototype = list(
           id_proyecto = character(),
           cepas_seleccionadas = character(),
           datos_crudos = list(),
           datos_procesados = data.frame(),
           resultados_analisis = list(),
           graficos = list()
         )
)

# --- S4 Generics ---

#' Preprocess Raw Plate Reader Data
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @param ... Additional arguments (groups, bg_type, bg_param).
#' @export
setGeneric("transform_raw_data", function(.Object, ...) standardGeneric("transform_raw_data"))

#' Import Pre-processed (Curated) Data
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @param ... Additional arguments.
#' @export
setGeneric("transform_curated_data", function(.Object, ...) standardGeneric("transform_curated_data"))

#' Calculate Growth Parameters from Preprocessed Curves
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @param ... Additional arguments (method).
#' @export
setGeneric("calculate_growth_params", function(.Object, ...) standardGeneric("calculate_growth_params"))

#' Plot Preprocessed Growth Curves
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("plot_processed_curves", function(.Object) standardGeneric("plot_processed_curves"))

#' Analyze Growth Metrics
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("analyze_growth", function(.Object) standardGeneric("analyze_growth"))

#' Analyze Social Behavior (Fitness Effects)
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("analyze_social_behavior", function(.Object) standardGeneric("analyze_social_behavior"))

#' Analyze Diversity Effect on Fitness
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("analyze_diversity", function(.Object) standardGeneric("analyze_diversity"))

#' Analyze Biofilm Assembly Sequences
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("analyze_biofilm_sequence", function(.Object) standardGeneric("analyze_biofilm_sequence"))

#' Classify Strains as Cooperators, Cheaters, or Neutrals
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @param ... Additional arguments.
#' @export
setGeneric("summarize_social_behavior", function(.Object, ...) standardGeneric("summarize_social_behavior"))

#' Analyze Consortium Stability
#'
#' @param .Object A \linkS4class{bsocial} object.
#' @export
setGeneric("analyze_stability", function(.Object) standardGeneric("analyze_stability"))
