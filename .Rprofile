library(stats)
library(tidyquant)
library(magrittr)
library(tidyverse)
library(corrplot)
library(corrr)
library(TTR)
library(ggjoy)
library(keras)
library(stringi)
options(scipen = 999)

log_retorno <- function(x) log(x) - log(lag(x))

add_lags <- function(data, lags, ...) {
  vars <- map(names(data %>% select(...)), as.name)
  fun_names <- stri_c("lag_", stri_pad_left(lags, 3, "0"))
  lag_funs <- map(lags, ~ partial(lag, n = .x, .lazy = FALSE)) %>% set_names(fun_names)

  vars %>%
    map(~ data %>%
          select(!!!.x) %>%
          mutate_all(.funs = funs(!!!lag_funs)) %>%
          set_names(c(first(names(.)), sprintf("%s_%s", first(names(.)), fun_names))) %>%
          select(-!!!.x)) %>%
    reduce(bind_cols, .init = data)
}

add_leads <- function(data, leads, ...) {
  vars <- map(names(data %>% select(...)), as.name)
  fun_names <- stri_c("lead_", stri_pad_left(leads, 3, "0"))
  lead_funs <- map(leads, ~ partial(lead, n = .x, default = 0, .lazy = FALSE)) %>% set_names(fun_names)

  vars %>%
    map(~ data %>%
          select(!!!.x) %>%
          mutate_all(.funs = funs(!!!lead_funs)) %>%
          set_names(c(first(names(.)), sprintf("%s_%s", first(names(.)), fun_names))) %>%
          select(-!!!.x)) %>%
    reduce(bind_cols, .init = data)
}
