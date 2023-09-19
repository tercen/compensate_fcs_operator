suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr, warn.conflicts = FALSE)
  library(flowCore)
  library(base64enc)
})

ctx <- tercenCtx()

if(is.null(ctx$task)) {
  stop("Task is null.")
} else {
  pair <- Find(function(pair) identical(pair$key, "task.siblings.id"), ctx$task$environment)
  task_siblings_id <- jsonlite::fromJSON(pair$value)
  ctx2 <- tercenCtx(taskId = task_siblings_id)
}

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

# Get compensation matrices
df_comp <- ctx2 %>% select(.ci, .ri, .y)
df_col <- ctx2$cselect() %>%
  mutate(.ci = seq_len(nrow(.)) - 1L)
df_row <- ctx2$rselect() %>%
  mutate(.ri = seq_len(nrow(.)) - 1L)
table <- df_comp %>%
  left_join(df_col, by = ".ci") %>%
  left_join(df_row, by = ".ri") %>%
  rename(channel_1 = comp_1, channel_2 = comp_2, value = .y)

## Check unique compensation values
if(any(table(df_comp$.ci, df_comp$.ri) > 1)) {
  stop("Multiple compensation values found in at least one cell.")
}
## Check compensation channel names
if(!identical(sort(unique(table$channel_1)), sort(unique(table$channel_2)))) {
  stop("Channel names must be identical in rows and columns of the compensation matrix.")
}
## Check compensation channel names vs. data channels
n_chan <- length(unique(table$channel_1)[which(unique(table$channel_1) %in% colnames(data))])
if(n_chan == 0) {
  stop("No compensation matrix channel found in the raw data.")
} else {
  ctx$log(paste0("Performing compensation on ", n_chan, " channels")) 
}

spill.matrices = table %>% 
  group_by(across(contains("filename"))) %>%
  group_map(~{
    tmp <- tidyr::pivot_wider(.x, id_cols = "channel_1",names_from = "channel_2", values_from = "value")
    tmp %>% select(-channel_1) %>% as.matrix()
  })

n_spill <- length(spill.matrices)

out <- sapply(
  seq_len(length(fset)),
  function(fs_id) {
    if(n_spill == 1) {
      sp <- spill.matrices[[1]]
    } else {
      sp <- spill.matrices[[fs_id]]
    } 
    cond <- colnames(sp) %in% colnames(fset[[fs_id]])
    compensate(fset[[fs_id]], sp[cond, cond])
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
