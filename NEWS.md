# shinyobjects dev
### New Features
* `output$plot <- renderPlot(plot(x))` transformed to `output$plot <- (plot(x))` (#45)
* `reactiveValues()` now converted to `list()` (#29)
### Bug fixes
* `view_ui()` less picky about input (#29)
* Don't add empty input list unless dummy list in code (#31)
* Rmd evaluated with or without spaces (`eval=F(ALSE)` or `eval = F(ALSE)`) (#26)
### Other
* Underlying code parses expressions instead of strings (#41)
* Reorganie code (renamed files/functions) (#36 #25)

# shinyobjects 0.1.1
### New Featues
* `view_ui()` can now run from selected text in the source editor (#19)
### Bug Fixes
* `convert_selection()` now prompts user for environment if not entered (#16)
### Other
* New logo (#21)

# shinyobjects 0.1.0 
Initial release
