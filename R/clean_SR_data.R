#' Cleaned dataset of Ca2+ Waves in Permeabilized cells.
#'
#' This dataframe contain only the parameters analyzed from Ca2+ waves in permeabilized cardiomyocites.

#' @format A data frame with 499 rows and 18 variables:
#' @section Fields:
#' \describe{
#' \item{\code{Date}:}{Object of class \code{"character"}, Date of experiment. Format "%Y%m%d".}
#' \item{\code{Animal_No}:}{Object of class \code{"factor"}, The internal animal number identifier.}
#' \item{\code{Animal}:}{Object of class \code{"factor"}, Experimental Aninmal used: WT, CPVT, etc.}
#' \item{\code{Experiment}:}{Object of class \code{"character"}, Permeabilization and Coverslip ID of experiment eg. P1 CS1, P3 CS2, ...}
#' \item{\code{Linescan}:}{Object of class \code{"character"}, Image recording linescan ID of experiment eg. LS1, LS2, ...}
#' \item{\code{Wave_No}:}{Object of class \code{"character"}, ID of wave in the recording eg. W1, W2, ...}
#' \item{\code{Treatment}:}{Object of class \code{"factor"}, Tretatment used in the experiment eg. cAMP, Fab, etc.}
#' \item{\code{Condition}:}{Object of class \code{"factor"}, The condition in a given treatment used eg. Control, or cAMP, etc.}
#' \item{\code{Inc_time_min}:}{Object of class \code{"integer"}, The time in min that last the tretament incubation in a given record}
#' \item{\code{Cyto_tau}:}{Object of class \code{"double"}, Decay time constant calculated from the descending phase of deskewed wave. Units are in ms.}
#' \item{\code{Cyto_DFF0}:}{Object of class \code{"double"}, Amplitude of deskewed waves. Units are in DF/F0.}
#' \item{\code{Cyto_Wave_Speed}:}{Object of class \code{"double"}, Velocity of Ca wave propagation. Units are in µm/s.}
#' \item{\code{Freq_waves_Hz}:}{Object of class \code{"double"}, Frequency of waves observed in a Linescan. Units are in Hz.}
#' \item{\code{For_Analysis}:}{Object of class \code{"logic"}, T} Are this experiment valid for final analysis? default to `TRUE` since this dataset already exclude no working experiment.
#' \item{\code{log.Cyto_tau}:}{Object of class \code{"double"}, Decay time constant calculated from the descending phase of deskewed wave. Units are in ms.}
#' \item{\code{log.Cyto_DFF0}:}{Object of class \code{"double"}, Amplitude of deskewed waves. Units are in DF/F0.}
#' \item{\code{log.Cyto_Wave_Speed}:}{Object of class \code{"double"}, Velocity of Ca wave propagation. Units are in µm/s.}
#' \item{\code{log.Freq_waves_Hz}:}{Object of class \code{"double"}, Frequency of waves observed in a Linescan. Units are in Hz.}
#' }
#'
#'
#' @source Department of Physiology, University of Bern
"clean_SR_data"
