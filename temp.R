library("tidyverse")
library("datasets")

data_df <- datasets::iris %>%
  tibble::as_tibble()


test_result <- data_df %>%
  dplyr::pull(Sepal.Length) %>%
  t.test(mu = 0, alternative = "two.sided")

summary_df <- data_df %>%
  dplyr::select(Sepal.Length) %>%
  dplyr::summarise(tibble(min = min(Sepal.Length),
                          q25 = quantile(Sepal.Length, probs = c(0.25)),
                          mu = mean(Sepal.Length),
                          median = median(Sepal.Length),
                          sigma = sd(Sepal.Length),
                          q75 = quantile(Sepal.Length, probs = c(0.75)),
                          max = max(Sepal.Length))) %>%
  dplyr::mutate(t.statistic = test_result$statistic) %>%
  dplyr::mutate(p.Value = test_result$p.value) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  tibble::as_tibble()

colnames(summary_df) <- c("statistics", "values")

summary_df

result <- data_df %>%
  dplyr::pull(Sepal.Length) %>%
  t.test(mu = 0, alternative = "two.sided")

result$p.value
result$statistic


data_vec <- data_df %>%
  dplyr::pull(Sepal.Length)

mu <- 0
(mean(data_vec) - mu) / (sd(data_vec)/sqrt(length(data_vec)))
