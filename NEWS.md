# shinyobjects 0.2.0
### New Features
* `eventReactive()` now parsed to return a function (#55)
* `output$plot <- renderPlot(plot(x))` transformed to `output$plot <- (plot(x))` (#45)
* `reactiveValues()` now converted to `list()` (#29)
### Bug fixes
* Multi-line `x <- reactive({\n...\n...\n})` statements weren't evaluating inside `runApp()` or `shinyApp()` (#24)
* `dummy_input` removed from objects returned (#50)
* Don't add empty input list unless dummy list in code (#31)
* Rmd evaluated with or without spaces (`eval=F(ALSE)` or `eval = F(ALSE)`) (#26)
### Other
* Logic for extracting `server` code uses call names instead of indexing (#52, 54)
* Underlying code parses expressions instead of strings (#41)
* Reorganize code (renamed files/functions) (#36 #25)

# shinyobjects 0.1.1
### New Features
* `view_ui()` can now run from selected text in the source editor (#19)
* `convert_selection()` now prompts user for environment if not entered (#16)

# shinyobjects 0.1.0 
Initial release