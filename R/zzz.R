.onAttach <- function(libname, pkgname)
{
  assign("fmreg", list(), envir = e)
  

  fmclass.register(fmclass = "lm.fit.models",
                   classes = c("lm"),
                   validation.function = NULL)
                                 
  fmclass.register(fmclass = "glm.fit.models",
                   classes = c("glm"),
                   validation.function = NULL)

  invisible()
}


