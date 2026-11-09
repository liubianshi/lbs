test_that("gg saves a real image without preview in batch mode", {
  path <- file.path(withr::local_tempdir(), "test plot.png")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_equal(gg(path, plot = p, width = 2, height = 2, open = FALSE), path)
  expect_gt(file.info(path)$size, 0)
  expect_error(gg(path, plot = p, open = NA))
})

test_that("gg supplies a white background without overriding an explicit background", {
  backgrounds <- list()
  # A device callback observes the arguments actually supplied by ggsave.
  device <- function(filename, width, height, bg, ...) {
    backgrounds[[length(backgrounds) + 1L]] <<- bg
    grDevices::png(filename, width = width, height = height, units = "in", res = 72, bg = bg)
  }
  p <- ggplot2::ggplot()
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  gg(path, p, device = device, width = 1, height = 1, open = FALSE)
  gg(path, p, device = device, width = 1, height = 1, bg = "transparent", open = FALSE)
  expect_equal(backgrounds, list("white", "transparent"))
})

test_that("preview dispatch quotes paths and preserves SSH defaults", {
  calls <- list()
  local_mocked_bindings(
    .gg_command_available = function(x) TRUE,
    .gg_launch = function(command, args) {
      calls[[length(calls) + 1L]] <<- list(command = command, args = args, wait = FALSE)
      0L
    }
  )
  withr::local_envvar(c(SSH_TTY = "", OPENER = "xdg-open"))
  .gg_preview("a b.png")
  expect_equal(calls[[1]], list(command = "xdg-open", args = shQuote("a b.png"), wait = FALSE))
  Sys.setenv(SSH_TTY = "/dev/pts/1")
  .gg_preview("a b.png")
  expect_equal(calls[[2]]$command, "ssh_push")
  expect_equal(calls[[2]]$args, c("--port", "2222", "--user", "liubianshi", "--dest", "/tmp",
                                 "--force", "--open", shQuote("a b.png")))
})

test_that("missing preview commands warn after successful saving", {
  withr::local_envvar(c(SSH_TTY = "", OPENER = ""))
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  expect_warning(result <- gg(path, ggplot2::ggplot(), width = 1, height = 1), "preview failed")
  expect_equal(result, path)
  expect_true(file.exists(path))
})
