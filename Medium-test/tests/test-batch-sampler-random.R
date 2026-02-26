library(testthat)
data.table::setDTthreads(1L)

test_that("random batch sampler covers all indices exactly once when shuffle=FALSE", {
  sonar_task <- mlr3::tsk("sonar")
  sonar_list <- list(task = sonar_task)

  sampler_class <- mlr3torchAUM::batch_sampler_random(batch_size = 10, shuffle = FALSE)
  sampler_instance <- sampler_class(sonar_list)

  all_ids <- unlist(sampler_instance$batch_list, use.names = FALSE)

  expect_equal(length(all_ids), sonar_task$nrow)
  expect_equal(length(unique(all_ids)), sonar_task$nrow)
  expect_equal(sort(all_ids), seq_len(sonar_task$nrow))
})