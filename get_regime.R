`%>%` <- magrittr::`%>%`
#' @description Split timeseries of rodent observations and covariates into train and test for a single regime
#' time period
#'
#' @param data rodent data table, with newmoon numbers
#' @param regime specify 1:4
#'               Regime 1: Jan 1979 - Nov 1983,
#'                  transition 1: newmoon 80:88: Dec 1983 - July 1984,
#'                  event: newmoon 77:Sep 27-Oct 3 1983
#'               Regime 2: Aug 1984 - Sep 1988,
#'                  transition 2: newmoon 140:230: Oct 1988 - Jan 1996,
#'                  event: 
#'               Regime 3: Feb 1996 - Aug 1998,
#'                  transition 3: newmoon 263:278: Sep 1998 - Dec 1999,
#'               Regime 4: Jan 2000 - May 2009,
#'                  transition 4: newmoon 396:411: Jun 2009 - Sep 2010
#'
#' @param break specify "transition" or "event" to choose regime break at Christensen et al transition window or 
#' newmoon immediately before known event
#'
#'
get_regime <- function(regime = 4, break = "out") {
 if (break == event){
   if (regime == 1) {
     regime_start = 4
     regime_stop <- 77
   }
  if (regime == 2) {
    regime_start = 78
    regime_stop = 140
    transition_stop <- 230
  }

  if (regime == 3) {
    regime_stop <- 263
    transition_stop <- 278
  }

  if (regime == 4) {
    regime_stop <- 396
    transition_stop <- 411
  }

  if (!(regime %in% 1:4)) {
    stop("Regime number must be 1, 2, 3, or 4:
              Regime 1: Jan 1979 - Nov 1983,
                  transition 1: newmoon 80:88: Dec 1983 - July 1984,
               Regime 2: Aug 1984 - Sep 1988,
                  transition 2: newmoon 140:230: Oct 1988 - Jan 1996,
               Regime 3: Feb 1996 - Aug 1998,
                  transition 3: newmoon 263:278: Sep 1998 - Dec 1999,
               Regime 4: Jan 2000 - May 2009,
                  transition 4: newmoon 396:411: Jun 2009 - Sep 2010")
  }

  if (test == "out") {
    train_start <- regime_stop - 60
    train_stop <- regime_stop - 1
    test_start <- transition_stop + 1
    test_stop <- transition_stop + 13
  }

  if (test == "in") {
    gap <- transition_stop - regime_stop + 1
    train_start <- regime_stop - 13 - gap - 60
    train_stop <- regime_stop - 13 - gap - 1
    test_start <- regime_stop - 13
    test_stop <- regime_stop - 1
  }

  return(list(
    train_start = train_start,
    train_stop = train_stop,
    test_start = test_start,
    test_stop = test_stop
  ))
}
