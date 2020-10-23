library(leaflet)

#colnames(RC_database)

# Choices for drop-downs
num_vars <- c(
  "SOC %" = "lyr_soc",
  "SOC Stock" = "lyr_soc_stock",
  "SIC %" = "lyr_sic",
  "SIC Stock" = "lyr_sic_stock"
  # "Centile score" = "centile",
  # "College education" = "college",
  # "Median income" = "income",
  # "Population" = "adultpop"
)

char_vars <- c(
  "Watershed" = "L1"
  # "Centile score" = "centile",
  # "College education" = "college",
  # "Median income" = "income",
  # "Population" = "adultpop"
)


navbarPage("Reynolds Creek Experimental Watershed", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Data explorer"),

        selectInput("color", "Map Analyte", num_vars),
        #selectInput("size", "Classify By", char_vars, selected = "adultpop"),
        column(width=12, fixedRow(
          column(6,
               numericInput("lyr_top_min", "Layer Top Min", value=0)),
          column(6,
                 numericInput("lyr_top_max", "Layer Top Max", value=0)))),
        column(width=12, fixedRow(
          column(6,
                 numericInput("lyr_bot_min", "Layer Bottom Min", value=10)),
          column(6,
                 numericInput("lyr_bot_max", "Layer Bottom Max", value=10)))),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Reynolds Creek CZO'), ' by Derek Pierson.'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(4,
        selectInput("dataset", "Dataset", c("All datasets"="", as.character(unique(RC_database$Dataset))), multiple=TRUE) #structure(state.abb, names=state.name)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(2,
        numericInput("minDepth", "Layer top limit (cm)", min=0, max=1000, value=NA)
      ),
      column(2,
        numericInput("maxDepth", "Layer bottom limit (cm)", min=0, max=1000, value=NA)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
