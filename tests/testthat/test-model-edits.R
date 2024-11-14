model_file_name <- system.file(package = "stanedit", "regression.stan")
reg <- stanedit(filename = model_file_name)

test_that("the example file can be loaded and the right class is assigned", {
  expect_s3_class(reg, "stanedit")
})

test_that("operators work", {
  reg2 <- remove_lines(reg, "parameters")
  expect_true(reg == reg)
  expect_false(reg == reg2)
  expect_true(reg != reg2)
  expect_false(reg == reg2)
  expect_equal(reg[3], "vector[N] x;")
  reg[3] <- "test;"
  expect_equal(reg[3], "test;")
})

test_that("blocks can be added and retrieved", {
  block <- c("real gamma;", "gamma = alpha * beta;")
  reg2 <- add_block(reg, "transformed parameters", block)
  line_nos <- find_block(reg2, "transformed parameters", inner = TRUE)
  line_nos_with_shell <- find_block(reg2, "transformed parameters")
  lines <- get_block(reg2, "transformed parameters")
  lines_with_shell <- get_block(reg2, "transformed parameters", shell = TRUE)

  expect_equal(lines, block)
  expect_equal(reg2[line_nos], block)
  expect_equal(reg2[line_nos_with_shell], lines_with_shell)
  expect_equal(lines_with_shell[-c(1, length(lines_with_shell))], block)
})

test_that("declarations are found", {
  expect_equal(find_declaration(reg, "alpha"), 7)
})

test_that("variables are found", {
  expect_equal(get_vars(reg, "parameters"), c("alpha", "beta", "sigma"))
})

test_that("lines can be inserted", {
  newline <- "real test;"
  reg2 <- insert_lines(reg, newline, before = 3)
  expect_equal(reg2[3], newline)
})

test_that("lines can be removed", {
  reg2 <- remove_lines(reg, "model", only = "y", type = "sample")
  reg3 <- remove_lines(
    reg, "model", only = "y", type = "sample", preserve_shell = TRUE
  )
  reg4 <- remove_lines(reg, "model", only = "y", type = "assignment")
  expect_length(get_block(reg2, "model"), 0)
  expect_length(get_block(reg3, "model", shell = TRUE), 2)
  expect_equal(reg4, reg)
})


test_that("lines can be moved", {
  reg2 <- move(reg, find_declaration(reg, "alpha"), at_end_of = "data")
  data <- get_block(reg2, "data")
  expect_equal(get_declaration(reg, "alpha"), data[length(data)])
})
