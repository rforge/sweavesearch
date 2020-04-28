needsPackages <- function(pkgs, install = TRUE, update = FALSE, 
			  load = FALSE, attach = FALSE) {
  missing <- c()
  for (p in pkgs) {
    if (!nchar(system.file(package = p)))
      missing <- c(missing, p)
  }
  if (length(missing)) {
    missing <- unique(missing)
    if (any(install)) {
      toinstall <- intersect(missing, pkgs[install])
      install.packages(toinstall)
      for (p in missing)
        if (!nchar(system.file(package = p)))
      	  stop("Did not install: ", p)
    } else
      stop("Missing packages: ", paste(missing, collapse = ", "))
  }
  if (any(update))
    update.packages(oldPkgs = pkgs[update], ask = FALSE, checkBuilt = TRUE)
  for (p in pkgs[load])
    loadNamespace(p)
  for (p in pkgs[attach])
    library(p, character.only = TRUE)
}