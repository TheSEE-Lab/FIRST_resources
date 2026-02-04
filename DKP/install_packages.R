# Install required packages for Dragon Kill Points app

cat("Installing required R packages...\n")

packages <- c("shiny", "DT", "xml2", "dplyr", "tidyr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(pkg, "is already installed.\n")
  }
}

cat("\nAll packages installed successfully!\n")
cat("You can now run the app with: shiny::runApp()\n")
