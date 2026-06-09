# task_list_meta ---------------------------------------------------------------

test_that("task_list_meta returns NULL for non-list input", {
  expect_null(task_list_meta(42))
  expect_null(task_list_meta("string"))
})

test_that("task_list_meta returns NULL when root node is itself a task", {
  # A root-level mod+func node has no name, so it cannot become a task entry.
  expect_null(task_list_meta(list(mod = "R/m", func = "f")))
})

test_that("task_list_meta extracts a single task with colon-joined name", {
  cfg <- list(data = list(clean = list(mod = "R/clean", func = "run")))
  result <- task_list_meta(cfg)
  expect_named(result, "data:clean")
  expect_equal(result[["data:clean"]]$name, "data:clean")
  expect_equal(result[["data:clean"]]$mod,  "R/clean")
  expect_equal(result[["data:clean"]]$func, "run")
})

test_that("task_list_meta extracts multiple tasks at the same level", {
  cfg <- list(
    data = list(
      clean = list(mod = "R/clean", func = "run"),
      merge = list(mod = "R/merge", func = "run")
    )
  )
  result <- task_list_meta(cfg)
  expect_length(result, 2)
  expect_setequal(names(result), c("data:clean", "data:merge"))
})

test_that("task_list_meta handles three levels of nesting", {
  cfg <- list(a = list(b = list(c = list(mod = "m", func = "f"))))
  result <- task_list_meta(cfg)
  expect_named(result, "a:b:c")
})

test_that("task_list_meta ignores non-task branches", {
  cfg <- list(
    data  = list(clean = list(mod = "R/clean", func = "run")),
    notes = "just a string"
  )
  result <- task_list_meta(cfg)
  expect_length(result, 1)
  expect_named(result, "data:clean")
})

# task_output_path -------------------------------------------------------------

test_that("task_output_path defaults to default.qs when filename is absent", {
  meta <- list(name = "data:clean", filename = NULL)
  expect_equal(task_output_path(meta), file.path("data", "clean", "default.qs"))
})

test_that("task_output_path uses an explicit filename", {
  meta <- list(name = "data:clean", filename = "result.qs")
  expect_equal(task_output_path(meta), file.path("data", "clean", "result.qs"))
})

test_that("task_output_path appends .qs when extension is missing", {
  meta <- list(name = "data:clean", filename = "result")
  expect_equal(task_output_path(meta), file.path("data", "clean", "result.qs"))
})

test_that("task_output_path maps three-level name to three-level directory", {
  meta <- list(name = "a:b:c", filename = NULL)
  expect_equal(task_output_path(meta), file.path("a", "b", "c", "default.qs"))
})

test_that("task_output_path errors on empty or NULL name", {
  expect_error(task_output_path(list(name = "",   filename = NULL)))
  expect_error(task_output_path(list(name = NULL, filename = NULL)))
})

# task_load_config -------------------------------------------------------------

test_that("task_load_config returns empty list and warns when no config file exists", {
  withr::with_tempdir({
    result <- expect_warning(task_load_config(), "not found")
    expect_equal(result, list())
  })
})

test_that("task_load_config reads config.yml", {
  withr::with_tempdir({
    yaml::write_yaml(list(key = "value"), "config.yml")
    expect_equal(task_load_config()$key, "value")
  })
})

test_that("task_load_config prioritises config.R over config.yml", {
  withr::with_tempdir({
    writeLines("list(source = 'R')", "config.R")
    yaml::write_yaml(list(source = "yml"), "config.yml")
    expect_equal(task_load_config()$source, "R")
  })
})

# task_assert_meta -------------------------------------------------------------

test_that("task_assert_meta errors on NULL", {
  expect_error(task_assert_meta(NULL))
})

test_that("task_assert_meta errors when name is not a single string", {
  expect_error(task_assert_meta(list(name = 42)))
  expect_error(task_assert_meta(list(name = c("a", "b"))))
})

test_that("task_assert_meta errors on name with no colon", {
  meta <- list(name = "nocolon", mod = "m", func = "f", path = "a/b.qs")
  expect_error(task_assert_meta(meta), "Invalid task name format")
})

test_that("task_assert_meta errors when func is missing", {
  withr::with_tempdir({
    dir.create("R/mod", recursive = TRUE)
    writeLines("", "R/mod/clean.R")
    meta <- list(name = "a:b", mod = "R/mod/clean", func = "", path = "a/b.qs")
    expect_error(task_assert_meta(meta), "'func' is missing")
  })
})

test_that("task_assert_meta errors on non-.qs output path", {
  withr::with_tempdir({
    dir.create("R/mod", recursive = TRUE)
    writeLines("", "R/mod/clean.R")
    meta <- list(name = "a:b", mod = "R/mod/clean", func = "run", path = "a/b.csv")
    expect_error(task_assert_meta(meta), "must end with .qs")
  })
})

test_that("task_assert_meta returns TRUE for valid metadata", {
  withr::with_tempdir({
    dir.create("R/mod", recursive = TRUE)
    writeLines("", "R/mod/clean.R")
    meta <- list(name = "a:b", mod = "R/mod/clean", func = "run", path = "a/b.qs")
    expect_true(task_assert_meta(meta))
  })
})

# task_write_file --------------------------------------------------------------

test_that("task_write_file creates tasks/r_tasks.yml from config.yml", {
  withr::with_tempdir({
    dir.create("R/pipeline", recursive = TRUE)
    writeLines("run <- function() 42", "R/pipeline/clean.R")
    yaml::write_yaml(
      list(data = list(clean = list(mod = "R/pipeline/clean", func = "run"))),
      "config.yml"
    )
    task_write_file()
    expect_true(file.exists(file.path("tasks", "r_tasks.yml")))
    lines <- readLines(file.path("tasks", "r_tasks.yml"))
    expect_true(any(grepl("lbs::task_run", lines)))
  })
})

test_that("task_write_file warns when no tasks are found", {
  withr::with_tempdir({
    yaml::write_yaml(list(), "config.yml")
    expect_warning(task_write_file())
  })
})

# task_init_project ------------------------------------------------------------

test_that("task_init_project writes a Taskfile.yml", {
  withr::with_tempdir({
    task_init_project()
    expect_true(file.exists("Taskfile.yml"))
    lines <- readLines("Taskfile.yml")
    expect_true(any(grepl("version",            lines)))
    expect_true(any(grepl("lbs::task_write_file", lines)))
    expect_true(any(grepl("r_tasks.yml",         lines)))
  })
})

test_that("task_init_project refuses to overwrite an existing file", {
  withr::with_tempdir({
    writeLines("original", "Taskfile.yml")
    expect_warning(task_init_project())
    expect_equal(readLines("Taskfile.yml"), "original")
  })
})

test_that("task_init_project overwrites when overwrite = TRUE", {
  withr::with_tempdir({
    writeLines("original", "Taskfile.yml")
    task_init_project(overwrite = TRUE)
    lines <- readLines("Taskfile.yml")
    expect_false(identical(lines, "original"))
    expect_true(any(grepl("version", lines)))
  })
})

# task_help --------------------------------------------------------------------

test_that("task_help writes an example config.yml", {
  withr::with_tempdir({
    task_help(write_example = TRUE)
    expect_true(file.exists("config.yml"))
    lines <- readLines("config.yml")
    expect_true(any(grepl("^mod:", trimws(lines))))
    expect_true(any(grepl("^func:", trimws(lines))))
  })
})

test_that("task_help refuses to overwrite an existing config.yml", {
  withr::with_tempdir({
    writeLines("original", "config.yml")
    expect_warning(task_help(write_example = TRUE))
    expect_equal(readLines("config.yml"), "original")
  })
})
