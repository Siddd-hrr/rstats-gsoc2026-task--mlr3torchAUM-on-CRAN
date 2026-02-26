get_shuffled_index <- function(n, shuffle = TRUE) {
  if (!shuffle) return(seq_len(n))
  if (torch::torch_is_installed()) {
    torch::as_array(torch::torch_randperm(n)) + 1L
  } else {
    sample(n)
  }
}

make_batch_iter <- function(self) {
  batch.i <- 0
  function() {
    if (batch.i < length(self$batch_list)) {
      batch.i <<- batch.i + 1L
      indices <- self$batch_list[[batch.i]]
      if (batch.i == length(self$batch_list)) {
        self$set_batch_list()
      }
      return(indices)
    }
    coro::exhausted()
  }
}