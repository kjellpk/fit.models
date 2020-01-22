.onAttach <- function(libname, pkgname)
{
  assign("fmreg", list(), envir = e)
  

  fmclass.register(fmclass = "lmfm",
                   classes = c("lm", "rlm"),
                   validation.function = NULL)
                                 
  fmclass.register(fmclass = "glmfm",
                   classes = c("glm"),
                   validation.function = NULL)

  fmclass.register(fmclass = "covfm",
                   classes = c("covClassic", "covRob"),
                   validation.function = NULL)

  invisible()
}


