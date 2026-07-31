# Regenerates dawnn.Rmd from dawnn.Rmd.orig, which needs TensorFlow and the
# model. When updating the vignette, edit dawnn.Rmd.edit_me, never dawnn.Rmd.
# Commit both outputs.
# Compile the "real" .Rmd with `Rscript vignettes/precompute.R`.

orig_dir <- setwd("vignettes")
on.exit(setwd(orig_dir))

knitr::knit("dawnn.Rmd.edit_me", output = "dawnn.Rmd")
