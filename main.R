suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr, warn.conflicts = FALSE)
  library(flowCore)
  library(base64enc)
})

source("./utils.R")

ctx <- tercenCtx()

data <- ctx$as.matrix() %>% t()
colnames(data) <- ctx$rselect()[[1]]

files <- ctx$cselect() %>% 
  select(contains("filename")) %>%
  mutate(.ci = seq_len(nrow(.)) - 1L)

fset <- data %>% as_tibble() %>% 
  bind_cols(files)

filename_idx <- grep("filename", names(fset))[1]
has_filename <- length(filename_idx) > 0
filename_col <- colnames(fset)[filename_idx]

fset <- fset %>%
  group_by(across(contains("filename"))) %>%
  group_map(~tim::matrix_to_flowFrame(as.matrix(.x)))

# get comp
doc.id <- ctx$select(ctx$labels[[1]], nr = 1)[[1]]
table <- ctx$client$tableSchemaService$select(doc.id) %>%
  as_tibble()

spill.matrices = table %>% 
  group_by(across(contains("filename"))) %>%
  group_map(~{
    tmp <- tidyr::pivot_wider(.x, id_cols = "channel_1",names_from = "channel_2", values_from = "value")
    tmp %>% select(-channel_1) %>% as.matrix()
  })

out <- sapply(
  seq_len(length(fset)),
  function(x) {
    cond <- colnames(spill.matrices[[x]]) %in% colnames(fset[[x]])
    compensate(fset[[x]], spill.matrices[[x]][cond, cond])
  }
)

rmap <- ctx$rselect() %>% mutate(.ri = seq_len(nrow(.)) - 1L)
ri_vec <- rmap$.ri
names(ri_vec) <- rmap[[1]]

df_tmp <- lapply(out, function(x) as_tibble(exprs(x))) %>% bind_rows()

colnames(df_tmp)[colnames(df_tmp) != ".ci"] <- ri_vec[colnames(df_tmp)[colnames(df_tmp) != ".ci"]]

df_out <- df_tmp %>% 
  tidyr::pivot_longer(cols = !.ci, names_to = ".ri", values_to = "compensated") %>%
  mutate(.ri = as.integer(.ri), .ci = as.integer(.ci)) %>% 
  arrange(.ri, .ci) %>%
  ctx$addNamespace()

ctx$save(df_out)
