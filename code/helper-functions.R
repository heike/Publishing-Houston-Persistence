library(tidyverse)
library(randomForest)

csafe2 <- readRDS(here::here("models/csafe_rf2.rds"))
csafe3 <- readRDS(here::here("models/csafe_rf3.rds"))

files <- dir(here::here("."),pattern="csv", recursive = TRUE, full.names = TRUE)

comparisons <- data.frame(source = files)

comparisons <- comparisons %>% mutate(
  data = source %>% purrr::map(.f = readr::read_csv)
)

#sapply(comparisons$data, dim)

comparisons <- comparisons %>% mutate(
  data = data %>% purrr::map(.f = function(d) {
    if (!("id1" %in% names(d))) {
      d <- d %>%
        mutate(
          id1 = land1,
          id2 = land2
        ) %>%
        separate_wider_delim(
          land1, delim="-", names = c("study1", "barrel1", "bullet1", "land1")
        ) %>%
        separate_wider_delim(
          land2, delim="-", names = c("study2", "barrel2", "bullet2", "land2")
        )
    }
    d
  })
)

#sapply(comparisons$data, dim)

comparisons <- comparisons %>% 
  mutate(
    data = data %>% purrr::map(.f = function(d) {
    d %>% mutate(
      bullet1 = gsub("B", "", bullet1),
      barrel1 = substring(barrel1, 2, 3),
      bullet2 = gsub("B", "", bullet2),
      barrel2 = substring(barrel2, 2, 3)
    )
    })
  )

long <- comparisons %>% tidyr::unnest(data)
long$lag <- abs(long$lag)
long$rf2 <- predict(csafe2, newdata=long, type="prob")[,2]
long$rf3 <- predict(csafe3, newdata=long, type="prob")[,2]

long$source <- basename(long$source)
long %>% write_csv(file="data/houston-all-comparisons.csv")
