

library(DependenciesGraphs)
library(CKMRpop)

deps <- envirDependencies("package:CKMRpop")
plot(deps)

# or to browse via ShinyApp:
# launch.app()

