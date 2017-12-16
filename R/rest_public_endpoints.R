#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Provides a way to access charting candle info
#'
#' @description
#'
#' Candles endpoint of Bitfinex API V2. URL Structure: URL structure: https://api.bitfinex.com/v2/candles/trade::TimeFrame::Symbol/Section
#'
#' @param symbol The symbol you want information about (e.g. 'tBTCUSD' for pair Bitcoin/US Dollar or 'fUSD' for funding info).
#' @param time_frame Available values: '1m', '5m', '15m', '30m', '1h', '3h', '6h', '12h', '1D', '7D', '14D', '1M'.
#' @param section Available values: "last", "hist".
#' @param limit Number of candles requested.
#' @param start Filter start in milliseconds (ms).
#' @param end Filter end in milliseconds (ms).
#' @param period (for funding only)
#' @param aggr
#' @param sort if = -1 it sorts results returned with new > old. if = 1 it sorts in the opposite order. Default is -1.
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' # 15 minutos
#' candles <- tidyr::crossing(symbol = c("tBTCUSD", "tBCHUSD",
#'                                       "tBTGUSD", "tIOTUSD",
#'                                       "tXMRUSD", "tLTCUSD",
#'                                       "tETHUSD", "tDSHUSD",
#'                                       "tETCUSD", "tZECUSD",
#'                                       "tXRPUSD", "tEOSUSD",
#'                                       "tSANUSD", "tOMGUSD"),
#'                            start = as.numeric(1504234800000 + seq(0, 1200000, by = 15000) * 60 * 1000)) %>%
#'   filter(start <= as.numeric(now()) * 1000)
#'
#' #
#' safely_bitfinex_candles <- purrr::safely(bitfinex_candles)
#' candles %<>%
#'   mutate(bitfinex_candles = map2(symbol, start, ~ {
#'     print(.x)
#'
#'     nome_do_rds <- sprintf("data/%s_15m_%s.rds", .x, .y)
#'     if(file.exists(nome_do_rds)) {print("já existe");return(read_rds(nome_do_rds))}
#'
#'     Sys.sleep(.1)
#'     deu_certo <- FALSE
#'     repeat{
#'       data <- safely_bitfinex_candles(symbol = .x,
#'                                       time_frame = '15m',
#'                                       section = 'hist',
#'                                       limit = 1000,
#'                                       start = .y,
#'                                       period = NULL,
#'                                       aggr = NULL,
#'                                       sort = 1
#'       )
#'
#'       deu_certo <- is.null(data$error)
#'       if(deu_certo){ break }
#'       print("deu errado. tentando novamente em 60 segundos.")
#'       Sys.sleep(30)
#'     }
#'
#'
#'     data <- data$result %>%
#'       mutate(symbol = .x)
#'     write_rds(data, path = nome_do_rds)
#'     return(data)
#'   }))
#' }
#'
#' @export
bitfinex_candles <- function(symbol = c("tBTCUSD"),
                             time_frame = c("1m", "5m", "15m", "30m", "1h", "3h", "6h", "12h", "1D", "7D", "14D", "1M"),
                             section = c("last", "hist"),
                             limit = 100,
                             start = NULL,
                             end = NULL,
                             period = NULL,
                             aggr = NULL,
                             sort = -1) {

  if(!is.null(period)) period <- paste0("p", period)
  if(!is.null(aggr)) aggr <- paste0("a", aggr)

  key <- paste(c("trade", time_frame[1], symbol[1], aggr[1], period[1]), collapse = ":")

  data <- httr::GET(glue::glue("https://api.bitfinex.com/v2/candles/{key}/{section[1]}"),
                    query = list(limit = limit,
                                 start = start,
                                 end = end,
                                 sort = sort))
  print(data$url)
  data %<>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    matrix(ncol = 6) %>%
    tibble::as_tibble() %>%
    setNames(c("mts", "open", "close", "high", "low", "volume")) %>%
    dplyr::mutate(mts_dt = lubridate::as_datetime(mts/1000)) %>%
    timetk::tk_tbl(preserve_index = FALSE) %>%
    dplyr::select(mts_dt, dplyr::everything())

  return(data)
}
