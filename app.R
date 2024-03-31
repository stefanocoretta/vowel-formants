#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)
library(bslib)
library(tidyverse)
library(Polychrome)
library(farver)

# Prep data ----

suppressWarnings({
  becker2010_wide <- read_csv("data/becker2010.csv",
                              col_types = str_flatten(c(
                                "cccccncccncnn", rep("ccnnnn", 14), "_nnnnnn"
                              )))
})

vow_names <- becker2010_wide %>%
  select(V1OS:V14F4) %>%
  colnames()
vow_values <-
  rep(c("vowel", "vowel_ps", "f1", "f2", "f3", "f4"), 14)

spec <- tibble(.name = vow_names,
               .value = vow_values)

becker2010 <- becker2010_wide %>%
  pivot_longer_spec(spec, values_drop_na = TRUE) %>%
  group_by(Language) %>%
  mutate(
    f1_z = (f1 - mean(f1, na.rm = TRUE)) / sd(f1, na.rm = TRUE),
    f2_z = (f2 - mean(f2, na.rm = TRUE)) / sd(f2, na.rm = TRUE),
    f3_z = (f3 - mean(f3, na.rm = TRUE)) / sd(f3, na.rm = TRUE)
  ) %>%
  ungroup() |> 
  drop_na(f1, f2)

fill_pal <- as.character(green.armytage.colors(n = 17))
col_pal <- ifelse(decode_colour(fill_pal, to = "hcl")[, "l"] > 50, "black", "white")

f2_lims <- c(2500, 500)
f1_lims <- c(900, 150)

# UI ----
ui <- page_navbar(
  fillable = FALSE,
  title = "Vowel formants",
  id = "nav",
  ## Sidebar ----
  sidebar = sidebar(
    conditionalPanel(
      "input.nav === 'Languages'",
      "Data from Becker-Kristal 2010.",
      h3("Settings"),
      selectInput("language", "Language",
                  choices = sort(unique(
                    becker2010$Language
                  ))),
      selectInput("dialect", "Variety", sort(unique(
        becker2010$Dialect
      )))
    ),
    conditionalPanel(
      "input.nav === 'Systems'",
      "Data from Becker-Kristal 2010.",
      h3("Settings"),
      selectInput("language", "Language",
                  choices = sort(unique(
                    becker2010$Language
                  ))),
      selectInput("dialect", "Variety", sort(unique(
        becker2010$Dialect
      )))
    )
  ),
  ## Languages ----
  nav_panel(
    "Languages",
    card(card_header("Vowel plot"),
         plotOutput("vowelPlot")),
    card(card_header("Formant means"),
         tableOutput("vowelTable"))
  ),
  ## Systems ----
  nav_panel(
    "Systems",
    "Test"
  )
)

# Server ----
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "dialect", choices = as.character(becker2010$Dialect[becker2010$Language == input$language]))
  })
  
  filter_data <- reactive({
    if (input$dialect != "NA") {
      lang_data <- filter(
        becker2010,
        Language == input$language,
        Dialect == input$dialect
      )
    } else {
      lang_data <- filter(
        becker2010,
        Language == input$language
      )
    }
    
    lang_data <- lang_data |>
      group_by(vowel) |>
      summarise(
        f1 = mean(f1, na.rm = TRUE),
        f2 = mean(f2, na.rm = TRUE),
        f3 = mean(f3, na.rm = TRUE),
        f4 = mean(f4, na.rm = TRUE)
      ) |> 
      arrange(vowel)

    return(lang_data)
  })
  
  # Vowel Plot
  output$vowelPlot <- renderPlot({
    lang_data <- filter_data()
    
    if (max(lang_data$f1) > f1_lims[1]) {
      f1_lims[1] <- max(lang_data$f1) + 10
    }
    if (max(lang_data$f2) > f2_lims[1]) {
      f2_lims[1] <- max(lang_data$f2) + 10
    }
    
    lang_data |>
      ggplot(aes(f2, f1, label = vowel, fill = vowel, colour = vowel)) +
      geom_label(size = 6) +
      scale_x_reverse(limits = f2_lims, position = "top") +
      scale_y_reverse(limits = f1_lims, position = "right") +
      scale_fill_manual(values = alpha(fill_pal, 0.7)) +
      scale_colour_manual(values = col_pal) +
      theme_light() +
      labs(x = "F2 (Hz)", y = "F1 (Hz)") +
      theme(legend.position = "none") +
      coord_fixed(ratio = 2.5)
  })
  
  output$vowelTable <- renderTable({
    lang_data <- filter_data()
    lang_data
  },
  digits = 0)
}

# Run ----
shinyApp(ui = ui, server = server)
