bitfinex_candles <- function(symbol = c("tBTCUSD"),
                             time_frame = c("1m", "5m", "15m", "30m", "1h", "3h", "6h", "12h", "1D", "7D", "14D", "1M"),
                             section = c("last", "hist"),
                             limit = 100,
                             start = NULL,
                             end = NULL,
                             period = NULL,
                             aggr = NULL,
                             sort = -1) {
  key <- paste(c("trade", time_frame[1], symbol[1], aggr[1], period[1]), collapse = ":")

  data <- httr::GET(glue::glue("https://api.bitfinex.com/v2/candles/{key}/{section[1]}"),
                    query = list(limit = limit,
                                 start = start,
                                 end = end,
                                 sort = sort)) %>%
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
