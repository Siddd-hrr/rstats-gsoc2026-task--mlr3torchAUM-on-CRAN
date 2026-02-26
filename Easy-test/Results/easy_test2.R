# Reproduction script for:
# https://tdhock.github.io/blog/2025/mlr3torch-batch-samplers/
# Run with:
#   Rscript easy/blog_repro_mlr3torch_batch_samplers.R > easy/blog_repro_output.txt 2>&1

options(width = 120)

cat("=== Setup ===\n")
set.seed(1)

required <- c("mlr3", "mlr3torch", "torch", "data.table", "remotes")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

# sonar task may require mlr3data depending on your setup
if (!requireNamespace("mlr3data", quietly = TRUE)) {
  install.packages("mlr3data")
}

# Install mlr3torchAUM (used at end to show packaged function)
if (!requireNamespace("mlr3torchAUM", quietly = TRUE)) {
  remotes::install_github("tdhock/mlr3torchAUM", upgrade = "never", dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(mlr3)
  library(mlr3torch)
  library(torch)
  library(data.table)
  library(mlr3data)
})

cat("Package versions:\n")
pkgs <- c("torch","mlr3","mlr3torch","mlr3torchAUM","data.table","mlr3data")
print(sapply(pkgs, function(p) if (requireNamespace(p, quietly=TRUE)) as.character(packageVersion(p)) else NA))

cat("\n=== Load sonar task and create imbalance ===\n")
sonar_task <- mlr3::tsk("sonar")
count_tab <- table(sonar_task$data(sonar_task$row_ids, "Class")$Class)
print(count_tab)
print(count_tab / sum(count_tab))

sonar_task$filter(208:86)
count_tab <- table(sonar_task$data(sonar_task$row_ids, "Class")$Class)
print(count_tab)
print(count_tab / sum(count_tab))

sonar_task$col_roles$stratum <- "Class"

cat("\n=== Define custom batch sampler (instrumented) ===\n")
batch_sampler_stratified <- function(min_samples_per_stratum, shuffle=TRUE){
  torch::sampler(
    "StratifiedSampler",
    initialize = function(data_source) {
      self$data_source <- data_source
      TSK <- data_source$task
      self$stratum <- TSK$col_roles$stratum
      if(length(self$stratum)==0)stop(TSK$id, "task missing stratum column role")
      self$stratum_dt <- data.table(
        TSK$data(cols=self$stratum),
        row.id=1:TSK$nrow)
      self$set_batch_list()
    },
    set_batch_list = function() {
      get_indices <- if(shuffle){
        function(n)torch::as_array(torch::torch_randperm(n))+1L
      }else{
        function(n)1:n
      }
      index_dt <- self$stratum_dt[
        get_indices(.N)
      ][
      , i.in.stratum := 1:.N, by=c(self$stratum)
      ][]
      count_dt <- index_dt[, .(
        max.i=max(i.in.stratum)
      ), by=c(self$stratum)][order(max.i)]
      count_min <- count_dt$max.i[1]
      num_batches <- max(1, count_min %/% min_samples_per_stratum)
      max_samp <- num_batches * min_samples_per_stratum
      index_dt[
      , n.samp := i.in.stratum/max(i.in.stratum)*max_samp
      , by=c(self$stratum)
      ][
      , batch.i := ceiling(n.samp/min_samples_per_stratum)
      ][]
      save_list$count[[paste("epoch", length(save_list$count)+1)]] <<- dcast(
        index_dt,
        batch.i ~ Class,
        list(length, indices=function(x)paste(x, collapse=",")),
        value.var="row.id"
      )[
      , labels := index_dt[, paste(Class, collapse=""), by=batch.i][order(batch.i), V1]
      ]
      self$batch_list <- split(index_dt$row.id, index_dt$batch.i)
      self$batch_sizes <- sapply(self$batch_list, length)
      self$batch_size_tab <- sort(table(self$batch_sizes))
      self$batch_size <- as.integer(names(self$batch_size_tab)[length(self$batch_size_tab)])
    },
    .iter = function() {
      batch.i <- 0
      function() {
        if (batch.i < length(self$batch_list)) {
          batch.i <<- batch.i + 1L
          indices <- self$batch_list[[batch.i]]
          save_list$indices[[length(save_list$indices)+1]] <<- indices
          if (batch.i == length(self$batch_list)) {
            self$set_batch_list()
          }
          return(indices)
        }
        coro::exhausted()
      }
    },
    .length = function() {
      length(self$batch_list)
    }
  )
}

cat("\n=== Define loss module that saves targets ===\n")
nn_bce_with_logits_loss_save <- torch::nn_module(
  "nn_print_loss",
  inherit = torch::nn_mse_loss,
  initialize = function() {
    super$initialize()
    self$bce <- torch::nn_bce_with_logits_loss()
  },
  forward = function(input, target) {
    save_list$target[[length(save_list$target)+1]] <<- target
    self$bce(input, target)
  }
)

cat("\n=== Train with stratified sampler ===\n")
set.seed(4)
mlp_learner <- mlr3torch::LearnerTorchMLP$new(
  task_type="classif",
  loss=nn_bce_with_logits_loss_save
)
mlp_learner$predict_type <- "prob"

mlp_learner$param_set$set_values(
  epochs=1,
  p=0,
  batch_size=1, # ignored but required
  batch_sampler=batch_sampler_stratified(min_samples_per_stratum = 1)
)

save_list <- list()
mlp_learner$train(sonar_task)

cat("save_list names:\n")
print(names(save_list))

cat("\nBatch label counts (epoch 1 and epoch 2 tables):\n")
print(save_list$count)

cat("\nConsistency check: epoch 1 should have row.id_length_R == 1 for all batches:\n")
epoch1 <- save_list$count[["epoch 1"]]
print(epoch1[, .(batch.i, row.id_length_M, row.id_length_R)])
stopifnot(all(epoch1$row.id_length_R == 1))

cat("\n=== Double-check correct indices and labels ===\n")
sonar_dataset <- mlp_learner$dataset(sonar_task)
for(batch.i in 1:length(save_list$target)){
  index_vec <- save_list$indices[[batch.i]]
  target_tensor <- save_list$target[[batch.i]]
  target_vec <- torch::as_array(target_tensor)
  label_vec <- names(count_tab)[ifelse(target_vec==1, 1, 2)]
  data.table::set(
    save_list$count[["epoch 1"]],
    i=batch.i,
    j="check",
    value=paste(label_vec, collapse="")
  )
  stopifnot(all.equal(
    target_tensor$flatten(),
    sonar_dataset$.getbatch(index_vec)$y$flatten()
  ))
}
print(save_list$count[["epoch 1"]])

cat("\n=== Varying batch size via min_samples_per_stratum ===\n")
label_count_dt_list <- list()
for(min_samples_per_stratum in c(1:7,20)){
  mlp_learner$param_set$set_values(
    epochs=1,
    p=0,
    batch_size=1,
    batch_sampler=batch_sampler_stratified(min_samples_per_stratum = min_samples_per_stratum)
  )
  save_list <- list()
  mlp_learner$train(sonar_task)
  label_count_dt_list[[paste0("min_samples_per_stratum=", min_samples_per_stratum)]] <-
    save_list$count[["epoch 1"]][, data.table(
      batch.i, row.id_length_M, row.id_length_R,
      batch_size=row.id_length_M + row.id_length_R
    )]
}
print(label_count_dt_list)

cat("\n=== shuffle parameter behavior ===\n")
shuffle_list <- list()
for(shuffle in c(TRUE, FALSE)){
  mlp_learner$param_set$set_values(
    epochs=1,
    p=0,
    batch_size=1,
    batch_sampler=batch_sampler_stratified(min_samples_per_stratum = 1, shuffle = shuffle)
  )
  save_list <- list()
  mlp_learner$train(sonar_task)
  shuffle_list[[paste0("shuffle=", shuffle)]] <- save_list$count
}
print(shuffle_list)

cat("\n=== Randomness control part 1: seed controls random weights ===\n")
param_list <- list()
set.seed(5)
for(set_seed in c(0:1)){
  for(rep_i in 1:2){
    L <- mlr3torch::LearnerTorchMLP$new(task_type="classif")
    L$param_set$set_values(epochs=0, batch_size=10)
    if(set_seed) L$param_set$values$seed <- 1
    L$train(sonar_task)
    param_list[[sprintf("set_seed=%d rep=%d", set_seed, rep_i)]] <-
      unlist(lapply(L$model$network$parameters, torch::as_array))
  }
}
print(do.call(rbind, param_list)[, 1:5])

cat("\n=== Randomness control part 2: seed controls batching (default + custom) ===\n")
param_list <- list()
set.seed(5)
for(set_batch_sampler in c(0:1)){
  for(rep_i in 1:2){
    L <- mlr3torch::LearnerTorchMLP$new(task_type="classif")
    L$param_set$set_values(epochs=1, batch_size=10, seed=1)
    if(set_batch_sampler) L$param_set$values$batch_sampler <- batch_sampler_stratified(1)
    L$train(sonar_task)
    param_list[[sprintf("set_batch_sampler=%d rep=%d", set_batch_sampler, rep_i)]] <-
      unlist(lapply(L$model$network$parameters, torch::as_array))
  }
}
print(do.call(rbind, param_list)[, 1:5])

cat("\n=== Confirm packaged sampler exists (non-instrumented) ===\n")
print(mlr3torchAUM::batch_sampler_stratified)

cat("\n=== Session info ===\n")
print(sessionInfo())