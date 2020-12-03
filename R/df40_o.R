#' Raw dataframe of Ca2+ Waves in Intact cells.
#'
#' the raw containing the parameters analyzed from Ca2+ waves in intact cardiomyocites.
#'
#' @format A data frame with 112 rows and 42 variables:
#' @section Fields:
#' \describe{
#' \item{\code{Date}:}{Object of class \code{"factor"}, Date of experiment. Format "%Y%m%d".}
#' \item{\code{filename}:}{Object of class \code{"character"}, File name of experiment.}
#' \item{\code{Experiment}:}{Object of class \code{"character"}, Coverslip ID of experiment eg. CS1, CS2, ...}
#' \item{\code{Condition}:}{Object of class \code{"factor"}, Tretatment used eg. Control, ISO, etc.}
#' \item{\code{Animal}:}{Object of class \code{"factor"}, Experimental Aninmal used: WT, CPVT, etc.}
#' \item{\code{For WL Analysis?}:}{Object of class \code{"character"}, The experiment went well for Wave latency (WL) analysis? Y/N.}
#' \item{\code{For Occu Analysis?}:}{Object of class \code{"character"}, The experiment went well for occurency (Occu) analysis? Y/N.}
#' \item{\code{For EAP_Occu Analysis?}:}{Object of class \code{"character"}, The experiment went well for occurence of expontaneous action potential (EAP) analysis? Y/N.}
#' \item{\code{For_Caff_Transient}:}{Object of class \code{"character"}, The experiment went well for caffeine transient analysis? Y/N.}
#' \item{\code{For_Wave_Kinetics_Analysis?}:}{Object of class \code{"character"}, The experiment went well for Wave kinetics analysis? Y/N.}
#' \item{\code{Waves}:}{Object of class \code{"character"}, Presence of waves. Yes or Not: Y, N}
#' \item{\code{Wave_latency}:}{Object of class \code{"double"}, Time from the las electrically stimulated transient to first waves  in ms.}
#' \item{\code{Wave_speed_mean}:}{Object of class \code{"double"}, Average of wave velocity in um/s}
#' \item{\code{Wave_tau_mean}:}{Object of class \code{"double"}, Time constant (tau) of the fitted decay phase of the wave by a single exponential function in ms.}
#' \item{\code{Wave_t50_mean}:}{Object of class \code{"double"}, time to 50% of the wave peak in ms.}
#' \item{\code{Wave_deltaFF0_mean}:}{Object of class \code{"double"}, Amplitude of de-skewed waves in DF/F0}
#' \item{\code{Caff_tau}:}{Object of class \code{"double"}, Time constant (tau) of the fitted decay phase of the caffeine-induced transient by a single exponential function}
#' \item{\code{Caff_t50}:}{Object of class \code{"double"}, time to 50% of the caffeine-induced transient in ms}
#' \item{\code{Caff_deltaFF0}:}{Object of class \code{"double"}, Amplitude of the caffeine-induced transient in DF/F0}
#' \item{\code{Groups}:}{Object of class \code{"factor"}, interaction of Animals and Conditions factors}
#' \item{\code{core_name}:}{Object of class \code{"character"}, filenmae just without extention.}
#' }
#'
#'
#' @source Department of Physiology, University of Bern
"df40_o"
