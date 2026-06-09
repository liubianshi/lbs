delayedAssign("LOG", logger_factory("Task"))

# Session-scoped cache -- avoids re-reading files on every call within the same directory.
.task_state <- new.env(parent = emptyenv())

# --- 1. Config loading ---

task_load_config <- function() {
  if (file.exists("config.R")) {
    cfg <- tryCatch(
      source("config.R", local = TRUE, echo = FALSE)[["value"]],
      error = function(e) {
        LOG$warning(glue::glue("Error sourcing config.R: {e$message}"))
        NULL
      }
    )
    if (!is.null(cfg)) return(cfg)
  }

  if (file.exists("config.yml")) {
    return(yaml::read_yaml("config.yml"))
  }

  LOG$warning("Config file not found (config.R or config.yml)")
  list()
}

# Returns the CONFIG for the current working directory, reloading automatically on directory change.
.get_config <- function() {
  cwd <- getwd()
  if (!identical(.task_state$config_dir, cwd)) {
    .task_state$config_dir <- cwd
    .task_state$config <- task_load_config()
    .task_state$tasks  <- NULL  # invalidate TASKS cache when directory changes
  }
  .task_state$config
}

# --- 2. Metadata ---

# Recursively traverses a nested config list to produce a flat named list of
# task definitions. A node is a task if it contains both `mod` and `func`.
# Task names are built by joining nested keys with colons.
task_list_meta <- function(meta_tree = .get_config(), name = NULL) {
  if (!is.list(meta_tree)) return(NULL)

  if (all(c("mod", "func") %in% names(meta_tree))) {
    if (is.null(name)) return(NULL)
    meta <- c(list(name = name), meta_tree)
    return(stats::setNames(list(meta), name))
  }

  node_names <- names(meta_tree)
  child_names <- if (is.null(name)) node_names else paste(name, node_names, sep = ":")

  results <- mapply(
    task_list_meta,
    meta_tree = meta_tree,
    name = child_names,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  unlist(results, recursive = FALSE)
}

# Returns TASKS for the current directory; shares the invalidation logic of .get_config().
.get_tasks <- function() {
  if (is.null(.task_state$tasks)) {
    .task_state$tasks <- task_list_meta(.get_config())
  }
  .task_state$tasks
}

# Resolves the physical file path for a module string (e.g. "utils.logger").
# Searches cwd and paths in getOption("box.path").
task_find_mod_path <- function(mod_str) {
  rel_path <- chartr(".", "/", mod_str)
  search_roots <- unique(c(getwd(), getOption("box.path")))
  candidates <- c(paste0(rel_path, ".R"), file.path(rel_path, "__init__.R"))

  for (root in search_roots) {
    existing <- file.path(root, candidates)
    existing <- existing[file.exists(existing)]
    if (length(existing) > 0) return(fs::path_rel(existing[1]))
  }

  LOG$error(glue::glue(
    "Module '{mod_str}' not found in search paths: {paste(search_roots, collapse = ', ')}"
  ))
}

# Derives the output path from task metadata, using the colon-separated
# name as a directory hierarchy.
task_output_path <- function(meta) {
  if (is.null(meta$name) || !nzchar(meta$name)) {
    LOG$error("Task name is required and cannot be empty!")
  }

  path_components <- strsplit(meta$name, ":", fixed = TRUE)[[1]]
  target_dir <- do.call(file.path, as.list(path_components))

  fname <- ifthen(meta$filename, "default.qs")
  if (tools::file_ext(fname) == "") fname <- paste0(fname, ".qs")

  file.path(target_dir, fname)
}

# --- 3. Validation ---

task_assert_meta <- function(meta) {
  if (is.null(meta)) LOG$error("Task metadata is NULL")

  if (!is.character(meta$name) || length(meta$name) != 1L) {
    LOG$error("Task 'name' must be a single string.")
  }

  if (!grepl("^[-_A-Za-z0-9]+(:[-_A-Za-z0-9]+)+$", meta$name)) {
    LOG$error(glue::glue(
      "Invalid task name format: '{meta$name}'. Expected 'group:subgroup:task'"
    ))
  }

  # errors internally if module not found
  task_find_mod_path(meta$mod)

  if (is.null(meta$func) || !nzchar(meta$func)) {
    LOG$error(glue::glue("Task '{meta$name}': 'func' is missing"))
  }

  if (is.null(meta$path) || !nzchar(meta$path)) {
    LOG$error(glue::glue("Task '{meta$name}': Output path is missing"))
  }
  if (!tolower(tools::file_ext(meta$path)) %in% c("qs2", "qs")) {
    LOG$error(glue::glue("Task '{meta$name}': Output file must end with .qs or .qs2"))
  }

  TRUE
}

# --- 4. Core functions ---

# Merges config defaults with user-supplied meta, fills in the output path,
# and validates the result. Errors stop execution.
task_fetch_meta <- function(name = NULL, meta = NULL) {
  tasks <- .get_tasks()
  base_meta <- list()
  if (!is.null(name)) {
    name <- trimws(name)
    if (!name %in% names(tasks)) {
      LOG$error(glue::glue("Task '{name}' is not defined in config."))
    }
    base_meta <- tasks[[name]]
  }

  final_meta <- utils::modifyList(base_meta, ifthen(meta, list()))

  if (is.null(final_meta$name) && !is.null(name)) final_meta$name <- name
  if (is.null(final_meta$path)) final_meta$path <- task_output_path(final_meta)
  final_meta$args <- ifthen(final_meta$args, list())

  task_assert_meta(final_meta)

  final_meta
}

#' Run a Task Defined in config.yml
#'
#' Executes the named task and caches its result to disk as a `.qs` file.
#' Task definitions live in `config.yml` (or `config.R`) under a hierarchical
#' key structure, e.g. `data:clean:step1`.
#'
#' @param name Character string, colon-separated task path.
#' @param meta Optional list of metadata overrides (merged over config defaults).
#'
#' @return The task result, invisibly.
#' @export
task_run <- function(name = NULL, meta = NULL) {
  task_meta <- task_fetch_meta(name, meta)

  LOG$info(glue::glue("Starting task: {task_meta$name}"))

  tryCatch(
    {
      val <- cache_result(
        execute_box_mod_func(task_meta$mod, task_meta$func, task_meta$args),
        filename = task_meta$path,
        update = task_meta$update
      )
      LOG$info(glue::glue("Task '{task_meta$name}' completed. Cache: {task_meta$path}"))
      invisible(val)
    },
    error = function(e) {
      LOG$error(glue::glue("Task '{task_meta$name}' FAILED: {e$message}"))
      stop(e)
    }
  )
}

#' Write a Go-Task YAML File from Loaded Configuration
#'
#' Generates a Taskfile (compatible with [taskfile.dev](https://taskfile.dev))
#' in the `tasks/` subdirectory. Each configured task becomes a named entry
#' whose `cmds` field invokes `lbs::task_run()`.
#'
#' @param taskfile_name Output filename inside `tasks/`. Defaults to
#'   `"r_tasks.yml"`.
#'
#' @return `NULL` invisibly; called for its side-effect of writing a file.
#' @export
task_write_file <- function(taskfile_name = "r_tasks.yml") {
  tasks <- .get_tasks()
  if (length(tasks) == 0) {
    LOG$warning("No tasks found to write.")
    return(invisible(NULL))
  }

  go_tasks <- lapply(tasks, function(meta) {
    tryCatch(
      {
        if (is.null(meta$path)) meta$path <- task_output_path(meta)
        task_assert_meta(meta)

        cmd_str <- 'Rscript -e \'lbs::task_run("{{.TASK}}")\''

        task <- utils::modifyList(
          ifthen(meta$task, list()),
          list(
            desc = ifthen(meta$desc, glue::glue("Run task: {meta$name}")),
            cmds = list(cmd_str)
          )
        )
        task$sources  <- c(task$sources,  task_find_mod_path(meta$mod)) |> as.list()
        task$generates <- c(task$generates, meta$path) |> as.list()
        task
      },
      error = function(e) {
        LOG$warning(glue::glue("Skipping invalid task '{meta$name}': {e$message}"))
        NULL
      }
    )
  })

  go_tasks <- Filter(Negate(is.null), go_tasks)

  output_dir <- "tasks"
  fs::dir_create(output_dir, recurse = TRUE)
  output_file <- file.path(output_dir, taskfile_name)

  con <- file(output_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines(
    c(
      "# [WARNING] This file is automatically generated. Do not modify manually.",
      format(Sys.time(), "# Modified at: %Y-%m-%d %H:%M:%S"),
      "# https://taskfile.dev",
      "version: '3'",
      ""
    ),
    con
  )

  yaml::write_yaml(list(tasks = go_tasks), file = con)

  LOG$info(glue::glue("Taskfile generated at {output_file} ({length(go_tasks)} tasks)"))
  invisible(NULL)
}

#' Initialise a Project-Level Taskfile
#'
#' Writes a root `Taskfile.yml` that loads environment variables from `.env`,
#' includes the auto-generated `tasks/r_tasks.yml`, and defines a
#' `build_r_tasks` task that regenerates it via [task_write_file()].
#'
#' The root Taskfile is a project-specific file the user may customise, so
#' this function refuses to overwrite an existing file unless `overwrite = TRUE`.
#'
#' @param taskfile_name Filename for the root Taskfile. Defaults to
#'   `"Taskfile.yml"`.
#' @param r_tasks_file Path to the generated sub-Taskfile, relative to the
#'   project root. Must match the argument passed to [task_write_file()].
#' @param overwrite Logical. If `FALSE` (default), an existing file is left
#'   untouched and a warning is emitted.
#'
#' @return `NULL` invisibly.
#' @export
task_init_project <- function(
    taskfile_name = "Taskfile.yml",
    r_tasks_file  = "tasks/r_tasks.yml",
    overwrite     = FALSE) {
  if (file.exists(taskfile_name) && !overwrite) {
    LOG$warning(glue::glue(
      "'{taskfile_name}' already exists. Use overwrite = TRUE to replace it."
    ))
    return(invisible(NULL))
  }

  # Normalise path separators for YAML (always forward slashes)
  r_tasks_yaml <- gsub("\\\\", "/", r_tasks_file)
  r_tasks_dir  <- dirname(r_tasks_yaml)

  lines <- c(
    "# https://taskfile.dev",
    paste0('version: "3"'),
    "",
    "# Load environment variables from .env file for configuration context",
    "dotenv:",
    "  - .env",
    "",
    "includes:",
    "  lib:",
    paste0("    taskfile: ", r_tasks_yaml),
    "    flatten: true",
    "    optional: true",
    "",
    "tasks:",
    "  build_r_tasks:",
    "    desc: update tasks/r_tasks.yml",
    "    sources:",
    "      - config.yml",
    "      - config.R",
    "    generates:",
    paste0("      - ", r_tasks_yaml),
    "    watch: true",
    "    method: checksum",
    "    cmds:",
    "      - Rscript -e 'lbs::task_write_file()'"
  )

  con <- file(taskfile_name, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con)

  LOG$info(glue::glue("Root Taskfile written to '{taskfile_name}'"))
  invisible(NULL)
}

#' Show Configuration Guide for the Task System
#'
#' Prints a reference guide explaining how to write `config.yml` (or
#' `config.R`) for the lbs task system. Optionally writes an annotated
#' `config.yml` template to disk.
#'
#' @param write_example Logical. If `TRUE`, writes a commented example to
#'   `config.yml` in the current directory. Refused if the file already exists.
#'
#' @return `NULL` invisibly.
#' @export
task_help <- function(write_example = FALSE) {
  guide <- c(
    "",
    "  lbs task system -- configuration guide",
    "  -------------------------------------------------------",
    "",
    "  Config file: config.yml (or config.R) in your project root.",
    "  Tasks are nested YAML keys. A node becomes a task when it contains",
    "  both 'mod' and 'func'. Task names are the colon-joined key path.",
    "",
    "  -- Minimal example ---------------------------------------",
    "   ",
    "     data:",
    "       clean:                    # task name: 'data:clean'",
    "         mod: R/pipeline/clean   # box module path",
    "         func: run               # exported function in that module",
    "   ",
    "   --------------------------------------------------------",
    "",
    "  -- Full example ------------------------------------------",
    "   ",
    "     data:",
    "       clean:",
    "         mod:      R/pipeline/clean",
    "         func:     run",
    "         filename: cleaned.qs    # output cache (default: default.qs)",
    "         desc:     Clean raw survey data  # shown in `task --list`",
    "         update:   false         # true = force re-run",
    "         args:                   # forwarded as list to func()",
    "           year:    2024",
    "           drop_na: true",
    "   ",
    "       merge:",
    "         mod:  R/pipeline/merge",
    "         func: run",
    "   ",
    "     model:",
    "       ols:",
    "         mod:  R/models/ols",
    "         func: estimate",
    "   ",
    "   --------------------------------------------------------",
    "",
    "  Fields",
    "  ------",
    "  mod       (required)  Box module path, e.g. R/utils/clean or utils.clean",
    "  func      (required)  Function exported from that module",
    "  filename  (optional)  Cache filename; .qs extension added if omitted",
    "                        Default: default.qs",
    "  desc      (optional)  Description shown by `task --list`",
    "  args      (optional)  Named list of arguments forwarded to func()",
    "  update    (optional)  true = always recompute, ignoring cached output",
    "  task      (optional)  Extra go-task fields (sources, generates, vars ...)",
    "",
    "  Workflow",
    "  --------",
    "  1. Write config.yml in your project root",
    "  2. lbs::task_init_project()  -- create root Taskfile.yml (once)",
    "  3. lbs::task_write_file()    -- generate tasks/r_tasks.yml",
    "  4. task <name>               -- run via go-task CLI",
    "     lbs::task_run(\"<name>\")   -- or run directly from R",
    ""
  )

  cat(paste(guide, collapse = "\n"), "\n")

  if (write_example) {
    dest <- "config.yml"
    if (file.exists(dest)) {
      LOG$warning(glue::glue("'{dest}' already exists. Remove it first to write the example."))
      return(invisible(NULL))
    }

    example <- c(
      "# lbs task configuration",
      "# Run lbs::task_help() for a full field reference.",
      "#",
      "# Each leaf node with 'mod' + 'func' defines one task.",
      "# Task name = colon-joined keys, e.g. data:clean",
      "",
      "data:",
      "  clean:",
      "    mod:      R/pipeline/clean   # box module path",
      "    func:     run                # exported function",
      "    filename: cleaned.qs         # output cache file",
      "    desc:     Clean raw data",
      "    args:",
      "      year: 2024",
      "",
      "model:",
      "  ols:",
      "    mod:  R/models/ols",
      "    func: estimate"
    )

    writeLines(example, dest)
    LOG$info(glue::glue("Example config written to '{dest}'"))
  }

  invisible(NULL)
}
