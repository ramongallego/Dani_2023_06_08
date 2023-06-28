read_lines("lengths.txt") |> 
  as_tibble() |> 
  mutate(value = as.numeric(value)) |>
  mutate(value = case_when (value > 2000 ~ 2000,
                            TRUE        ~ value)) |> 
  ggplot(aes(value)) +
  geom_histogram(bins = 100)

read_lines("lengths_discarded.txt") |> 
  as_tibble() |> 
  mutate(value = as.numeric(value)) |>
  mutate(value = case_when (value > 2000 ~ 2000,
                            TRUE        ~ value)) |> 
  ggplot(aes(value)) +
  geom_histogram(bins = 100)

read_lines("pipeline_output/demultiplexed_20230623_1200/length.barcodes.txt")|> 
  as_tibble() |> 
  mutate(value = as.numeric(value)) |>
  mutate(value = case_when (value > 2000 ~ 2000,
                            TRUE        ~ value)) |> 
  ggplot(aes(value)) +
  geom_histogram(bins = 100)
