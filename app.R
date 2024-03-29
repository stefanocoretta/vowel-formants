#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)

becker2010_wide <- read_csv(
  "data/becker2010.csv",
  col_types = str_flatten(c("cccccncccncnn", rep("ccnnnn", 14), "_nnnnnn"))
)

vow_names <- becker2010_wide %>%
  select(V1OS:V14F4) %>%
  colnames()
vow_values <- rep(c("vowel", "vowel_ps", "f1", "f2", "f3", "f4"), 14)

spec <- tibble(
  .name = vow_names,
  .value = vow_values
)

becker2010 <- becker2010_wide %>%
  pivot_longer_spec(spec, values_drop_na = TRUE) %>%
  group_by(Language) %>%
  mutate(
    f1_z = (f1 - mean(f1, na.rm = TRUE)) / sd(f1, na.rm = TRUE),
    f2_z = (f2 - mean(f2, na.rm = TRUE)) / sd(f2, na.rm = TRUE),
    f3_z = (f3 - mean(f3, na.rm = TRUE)) / sd(f3, na.rm = TRUE)
  ) %>%
  ungroup()

f1_range <- range(becker2010$f1, na.rm = TRUE)
f2_range <- range(becker2010$f2, na.rm = TRUE)

# Define UI for application that draws a histogram
ui <- page_sidebar(

    title = "Vowel formants",
    
    sidebar = sidebar(
      "Data from Becker-Crystal 2010.",
        selectInput(
          "language", "Language",
          choices = sort(unique(becker2010$Language))
        ),
      checkboxInput("average", "Average multiple values?", value = TRUE)
    ),
    
    card(
      card_header("Vowel plot"),
      plotOutput("vowelPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$vowelPlot <- renderPlot({

      lang_data <- filter(becker2010, Language == input$language)
      
      if (input$average) {
        lang_data <- lang_data |> 
          group_by(vowel) |> 
          summarise(
            f1 = mean(f1, na.rm = TRUE),
            f2 = mean(f2, na.rm = TRUE)
          ) 
      }
      
      lang_data |> 
        ggplot(aes(f2, f1, label = vowel, fill = vowel)) +
        geom_label() +
        scale_x_reverse(limits = c(2850, 500), position = "top") +
        scale_y_reverse(limits = c(1150, 150), position = "right") +
        scale_fill_brewer(type = "qual") +
        theme_dark() +
        labs(x = "F2 (Hz)", y = "F1 (Hz)") +
        theme(legend.position = "none") +
        coord_fixed(ratio = 1.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)