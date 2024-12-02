
index.search <- function (topic, paths, firstOnly = FALSE) {
  res <- character()
  for (p in paths) {
    if (file.exists(f <- file.path(p, "help", "aliases.rds"))) 
      al <- readRDS(f)
    else if (file.exists(f <- file.path(p, "help", "AnIndex"))) {
      foo <- scan(f, what = list(a = "", b = ""), sep = "\t", 
                  quote = "", na.strings = "", quiet = TRUE)
      al <- structure(foo$b, names = foo$a)
    }
    else next
    f <- al[topic]
    if (is.na(f)) 
      next
    res <- c(res, file.path(p, "help", f))
    if (firstOnly) 
      break
  }
  res
}

getHelpFile <- function (file) 
{
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) 
    stop(gettextf("invalid %s argument", sQuote("file")), 
         domain = NA)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if (!file.exists(paste0(RdDB, ".rdx"))) 
    stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", 
                  sQuote(pkgname)), domain = NA)
  fetchRdDB(RdDB, basename(file))
}

fetchRdDB <- function (filebase, key = NULL) 
{
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]], 
                                           datafile, compressed, envhook)
    if (length(key)) {
      if (key %notin% vars) 
        stop(gettextf("No help on %s found in RdDB %s", 
                      sQuote(key), sQuote(filebase)), domain = NA)
      fetch(key)
    }
    else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, fun)
  if (length(key)) 
    res
  else invisible(res)
}