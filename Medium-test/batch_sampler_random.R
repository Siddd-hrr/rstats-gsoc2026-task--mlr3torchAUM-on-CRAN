batch_sampler_random <- function(batch_size, shuffle=TRUE){
  .N <- `:=` <- i.in.stratum <- . <- max.i <- n.samp <- batch.i <- self <- NULL
  ## Above for CRAN check.
  torch::sampler(
    "RandomSampler",
    initialize = function(data_source) {
      self$N <- data_source$task$nrow
      self$batch_vec <- seq_len(self$N) %/% batch_size
      self$set_batch_list()
    },
    set_batch_list = function() {
      index_vec <- get_shuffled_index(self$N, shuffle)
      self$batch_list <- split(index_vec, self$batch_vec)
    },
    .iter = function() {
      make_batch_iter(self)
    },
    .length = function() {
      length(self$batch_list)
    }
  )
}