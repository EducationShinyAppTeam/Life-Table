# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(shinycssloaders)
library(dplyr)

# Global Constants, Functions, and Data ----
load(file = "allData.RData")

fixedColors <- c(
  "UK-Males" = psuPalette[1],
  "UK-Females" = psuPalette[2],
  "US-Males" = psuPalette[3],
  "US-Females" = psuPalette[4],
  "China-Males" = psuPalette[6],
  "China-Females" = psuPalette[7]
)

countryColors <- c(
  "United Kingdom" = psuPalette[2],
  "United States" = psuPalette[4],
  "China" = psuPalette[6]
)

countryLyt <- c(
  "United Kingdom" = "solid",
  "United States" = "longdash",
  "China" = "twodash"
)

fixedLines <- c(
  "UK-Males" = "solid",
  "UK-Females" = "longdash",
  "US-Males" = "dotted",
  "US-Females" = "dashed",
  "China-Males" = "dotdash",
  "China-Females" = "twodash"
)

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "green",
    ### Header ----
    dashboardHeader(
      title = "Life Tables",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown", boastUtils::surveyLink(name = "Life_Table")),
      tags$li(
        class = "dropdown",
        tags$a(href = "https://shinyapps.science.psu.edu/", icon("house")
        )
      )
    ),
    ### Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview",icon = icon("gauge-high")),
        menuItem("Survival Rate", tabName = "survivalRate", icon = icon("wpexplorer")),
        menuItem("Static Pop. Pyramids", tabName = "staticPyr", icon = icon("wpexplorer")),
        menuItem("Time & Pop. Pyramids", tabName = "timePyr", icon = icon("wpexplorer")),
        menuItem("Fecundity Rate", tabName = "fecundityRate", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Body ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          h1("Life Tables"),
          p("In this app, you will explore various ways to visualize life tables
            for 3 different countries", HTML("&mdash;"),"China, the United Kingdom,
            and the United States of America. You'll be able to look at several 
            different metrics associated with life tables (e.g., survival and fecundity
            rates) by country and sex."),
          h2("Instructions"),
          p("Use the left sidebar menu to explore the life tables."),
          tags$ul(
            tags$li(tags$strong("Survival Rate:"), " Click the country and sex
                    combination you prefer to compare, and view the distinct
                    Survival Rate for each country and sex."),
            tags$li(tags$strong("Static Population Pyramids:"), " View and compare
                    Population Pyramids for different countries, as well as the
                    combination of sex and country."),
            tags$li(tags$strong("Time and Population Pyramaids:"), " Click the start
                    button to watch the population change by year in United
                    States and United Kingdom."),
            tags$li(tags$strong("Fecundity Rate:"), " Click the country you
                    want to explore and then view the different Fecundity Rate
                    (per 1,000 women).")
          ),
          div(
            style = "text-align:center;",
            bsButton(
              inputId = "goToSR",
              label = "Survival Rate",
              icon = icon("bolt"),
              size = "large",
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Yuqing Lei in 2019 with the
            support of funding provided by Dr. Stephen Schaeffer. The app was
            updated in 2021 by Dr. Neil J. Hatfield, updated in 2022 by Jing Fu, 
            and updated in 2023 by Taryn McHugh.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/25/2023 by TM.")
          )
        ),
        #### Survival Rate Page ----
        tabItem(
          tabName = "survivalRate",
          h2("Survival Rate"),
          p("The survival rate is the percentage of individuals still alive at
            a certain moment in time. Here, we can visualize the survival rate
            (vertical axis) of individuals in each country by sex at different
            ages (horizontal axis). For example, approximately 75% of females
            from the United States survive to age 75."
          ),
          p("Which sex has better survival rates overall? Which country has
            better overall survival rates? Do your answers change when you look
            at country and sex simultaneously?"),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                checkboxGroupInput(
                  inputId = "survivalSelection",
                  label = "Select the sexes and countries you wish to compare",
                  choices = c(
                    "China-Females" = "China-Females",
                    "China-Males" = "China-Males",
                    "United Kingdom-Females" = "UK-Females",
                    "United Kingdom-Males" = "UK-Males",
                    "United States-Females" = "US-Females",
                    "United States-Males" = "US-Males"
                  )
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("survivalChart1")
            )
          )
        ),
        #### Static Pyramid Page----
        tabItem(
          tabName = "staticPyr",
          h2("Static Population Pyramids"),
          p("Population pyramids are common visualizations for life data. They
            are composed of two histograms, breaking a countries population into
            two sub-collections, typically by sex."),
          tabsetPanel(
            id = "cohortPyramid",
            type = "tabs",
            ##### Country to Country ----
            tabPanel(
              title = "Country to Country",
              br(),
              p("Compare each country's population pyramid to the other countries.
                What do you notice?"),
              fluidRow(
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    radioButtons(
                      inputId = "upperCountry",
                      label = "Select the country for the upper graph",
                      choices = c(
                        "China",
                        "United Kingdom",
                        "United States"
                      ),
                      selected = "China"
                    ),
                    radioButtons(
                      inputId = "lowerCountry",
                      label = "Select the country for the lower graph",
                      choices = c(
                        "China",
                        "United Kingdom",
                        "United States"
                      ),
                      selected = "United Kingdom"
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("upperPyramid"),
                  br(),
                  plotOutput("lowerPyramid")
                )
              )
            ),
            ##### Country & Sex ----
            tabPanel(
              title = "Country & Sex Comparisons",
              br(),
              p("Population pyramids can also be mixed and matched across countries
                and sex. Create various pyramids to compare the different pairings
                of country and sex. What do you notice?"),
              fluidRow(
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    p("Select which Country-Sex pairings you want to compare."),
                    radioButtons(
                      inputId = "cohortLeft",
                      label = "Left side of the cohort pyramid",
                      choices = c(
                        "China-Females" = "China-Females",
                        "China-Males" = "China-Males",
                        "United Kingdom-Females" = "UK-Females",
                        "United Kingdom-Males" = "UK-Males",
                        "United States-Females" = "US-Females",
                        "United States-Males" = "US-Males"
                      ),
                      selected = "China-Females"
                    ),
                    radioButtons(
                      inputId = "cohortRight",
                      label = "Right side of the cohort pyramid",
                      choices = c(
                        "China-Females" = "China-Females",
                        "China-Males" = "China-Males",
                        "United Kingdom-Females" = "UK-Females",
                        "United Kingdom-Males" = "UK-Males",
                        "United States-Females" = "US-Females",
                        "United States-Males" = "US-Males"
                      ),
                      selected = "China-Males"
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("pyramidCountrySex") %>%
                    withSpinner(color = boastPalette[3])
                )
              )
            )
          )
        ),
        #### Time Population Pyramids ----
        tabItem(
          tabName = "timePyr",
          h2("Time and Population Pyramids"),
          p("Connecting population pyramids with time allows us to explore changes
            in a country's population. This allows us to see when there might be
            large increases in the population as well as when a country might
            suffer from catastrophic loss of life."),
          p("Explore both population pyramids for the United Kingdom and the
            United States. What do you notice? What might explain any big changes?"),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "actualCountry",
                  label = "Select a country",
                  choices = c(
                    "United Kingdom",
                    "United States"
                  ),
                  selected = "United States"
                ),
                sliderInput(
                  inputId = "yearInterval",
                  label = "Year",
                  min = 1900,
                  max = 2018,
                  value = 1900,
                  round = TRUE,
                  sep = "",
                  animate = animationOptions(interval = 2000, loop = FALSE)
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("timePyramid"),
              p(tags$em("Note: "), "Interpret the oldest age group as being that
                age or older.")
            )
          )
        ),
        #### Fecundity Rate Page ----
        tabItem(
          tabName = "fecundityRate",
          h2("Fecundity Rate"),
          p("A common metric for life tables is a country's fecundity rate.
            A country's fecundity rate refers to the number of new children born
            in a year. Here, we've shown each country's fecundity rate as the
            number of children born per 1,000 women plotted against the age of
            the mother"),
          p("How does the fecundity rate differ between each country? What might 
            explain these differences?"),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                checkboxGroupInput(
                  inputId = "fecundityCountry",
                  label = "Select the countries you want to compare",
                  choices = c(
                    "China",
                    "United Kingdom",
                    "United States"
                  )
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("fecundityPlot")
            )
          )
        ),
        #### References ----
        tabItem(
          tabName = "references",
          h2("References"),
          p( 
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Availble from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Sali, A., and Attali, D. (2020), shinycssloaders: Add Loading
            Animations to a 'shiny' Ouput While It's Recalculating. (v. 1.0.0)
            [R Package] Available https://CRAN.R-project.org/package=shinycssloaders"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.2). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v1.1.2). [R Package].
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          h3("Data Sources"),
          p("Data for this app was retrived from:"),
          tags$ul(
            tags$li(
              "United States: ", tags$a(
                href = "https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf",
                class = "bodylinks",
                target = "_blank",
                "https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf")
            ),
            tags$li(
              "United Kingdom: ", tags$a(
                href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables",
                class = "bodylinks",
                target = "_blank",
                "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/ lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables"
              )
            ),
            tags$li(
              "China: ", tags$a(
                href = "http://www.stats.gov.cn/english/",
                class = "bodylinks",
                target = "_blank",
                "http://www.stats.gov.cn/english/"
              )
            ) #original link is broken: http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm
          ),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Info Button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Information",
        type = "info",
        text = "Click through each tab on the left to view the survival rates, 
        population pyramids, and the fecundity rates of three different countries."
      )
    }
  )
  
  ## Go Button ----
  observeEvent(
    eventExpr = input$goToSR,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "survivalRate"
      )
    }
  )
  
  ## Survival Rate Plot ----
  observeEvent(
    eventExpr = input$survivalSelection,
    handlerExpr = {
      output$survivalChart1 <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$survivalSelection),
              message = "Please select at least one option to the left"
            )
          )
          survivalData %>%
            dplyr::filter(
              country.sex %in% input$survivalSelection
            ) %>%
            ggplot(
              mapping = aes(
                x = age,
                y = survival,
                color = country.sex,
                linetype = country.sex)
            ) +
            geom_line(linewidth = 2) +
            theme_bw() +
            ylab("Survival Rate") +
            xlab("Age (years)") +
            labs(title = "Survival Rates by Age for Country and Sex") +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.05), add = 0)
            ) +
            scale_color_manual(
              name = "Country & Sex",
              values = fixedColors
            ) +
            scale_linetype_manual(
              name = "Country & Sex",
              values = fixedLines
            ) +
            theme(legend.key.size = unit(2,"cm"))
        },
        alt = "The survival rates at each age for selected countries and sexes"
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  ## Upper Country Pyramid ----
  observeEvent(
    eventExpr = input$upperCountry,
    handlerExpr = {
      output$upperPyramid <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$upperCountry),
              message = "Please select an option for the upper country."
            )
          )
          popPyramid %>%
            ggplot(
              mapping = aes(x = age, fill = countrySex)
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == switch(
                EXPR = input$upperCountry,
                "China" = "China-Females",
                "United Kingdom" = "UK-Females",
                "United States" = "US-Females"
              )),
              mapping = aes(y = -1 * count),
              na.rm = TRUE
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == switch(
                EXPR = input$upperCountry,
                "China" = "China-Males",
                "United Kingdom" = "UK-Males",
                "United States" = "US-Males"
              )),
              mapping = aes(y = count),
              na.rm = TRUE
            ) +
            coord_flip() +
            theme_bw() +
            ylab("Percentage Alive") +
            xlab("Age (years)") +
            labs(fill = "Country & Sex") +
            theme(
              text = element_text(size = 18),
              legend.position = "top"
            ) +
            scale_x_continuous(
              limits = c(0, 101),
              expand = expansion(mult = 0, add = c(0,1)),
              breaks = seq.int(from = 0, to = 100, by = 5),
              minor_breaks = NULL
            ) +
            scale_y_continuous(
              limits = c(-100, 100),
              expand = expansion(mult = 0, add = 5),
              breaks = seq.int(from = -100, to = 100, by = 25),
              labels = c(100, 75, 50, 25, 0, 25, 50, 75, 100)
            ) +
            scale_fill_manual(values = fixedColors)
        },
        alt = paste("Population Pyramids of both genders for", input$upperCountry)
      )
    }
  )
  
  ## Lower Country Pyramid ----
  observeEvent(
    eventExpr = input$lowerCountry,
    handlerExpr = {
      output$lowerPyramid <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$lowerCountry),
              message = "Please select an option for the upper country."
            )
          )
          popPyramid %>%
            ggplot(
              mapping = aes(x = age, fill = countrySex)
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == switch(
                EXPR = input$lowerCountry,
                "China" = "China-Males",
                "United Kingdom" = "UK-Males",
                "United States" = "US-Males"
              )),
              mapping = aes(y = count),
              na.rm = TRUE
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == switch(
                EXPR = input$lowerCountry,
                "China" = "China-Females",
                "United Kingdom" = "UK-Females",
                "United States" = "US-Females"
              )),
              mapping = aes(y = -1 * count),
              na.rm = TRUE
            ) +
            coord_flip() +
            theme_bw() +
            ylab("Percentage Alive") +
            xlab("Age (years)") +
            labs(fill = "Country & Sex") +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) +
            scale_x_continuous(
              limits = c(0, 101),
              expand = expansion(mult = 0, add = c(0,1)),
              breaks = seq.int(from = 0, to = 100, by = 5),
              minor_breaks = NULL
            ) +
            scale_y_continuous(
              limits = c(-100, 100),
              expand = expansion(mult = 0, add = 5),
              breaks = seq.int(from = -100, to = 100, by = 25),
              labels = c(100, 75, 50, 25, 0, 25, 50, 75, 100)
            ) +
            scale_fill_manual(values = fixedColors)
        },
        alt = paste("Population Pyramids of both genders for", input$lowerCountry)
      )
    }
  )
  
  ## Country & Sex Cohort Pyramid ----
  observeEvent(
    eventExpr = c(input$cohortLeft, input$cohortRight),
    handlerExpr = {
      output$pyramidCountrySex <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$cohortLeft) & !is.null(input$cohortRight),
              message = "Please select options for both the left and right sides"
            ),
            need(
              expr = input$cohortLeft != input$cohortRight,
              message = "Please select two different groups to compare"
            )
          )
          popPyramid %>%
            ggplot(
              mapping = aes(x = age, fill = countrySex)
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == input$cohortLeft),
              mapping = aes(y = -1 * count),
              na.rm = TRUE
            ) +
            geom_col(
              data = subset(popPyramid, countrySex == input$cohortRight),
              mapping = aes(y = count),
              na.rm = TRUE
            ) +
            coord_flip() +
            theme_bw() +
            ylab("Percentage Alive") +
            xlab("Age (years)") +
            labs(
              title = paste(input$cohortLeft, "vs.", input$cohortRight),
              fill = "Country & Sex"
            ) +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) +
            scale_x_continuous(
              limits = c(0, 101),
              expand = expansion(mult = 0, add = c(0,1)),
              breaks = seq.int(from = 0, to = 100, by = 5),
              minor_breaks = NULL
            ) +
            scale_y_continuous(
              limits = c(-100, 100),
              expand = expansion(mult = 0, add = 5),
              breaks = seq.int(from = -100, to = 100, by = 25),
              labels = c(100, 75, 50, 25, 0, 25, 50, 75, 100)
            ) +
            scale_fill_manual(values = fixedColors)
        },
        alt = paste("Population Pyramids for", input$cohortLeft, "and",
                    input$cohortRight)
      )
    }
  )
  
  ## Time Population Pyramid ----
  ### Get Data Frame
  timePyrData <- eventReactive(
    eventExpr = input$actualCountry,
    valueExpr = {
      if (input$actualCountry == "United States") {
        usTimePyramid
      } else {
        ## wrong column name
        ukTimePyramid <- rename(ukTimePyramid, female = male,male = female)
      }
    }
  )
  
  ### Update slider ----
  observeEvent(
    eventExpr = input$actualCountry,
    handlerExpr = {
      updateSliderInput(
        inputId = "yearInterval",
        min = min(timePyrData()$year),
        max = max(timePyrData()$year),
        value = min(timePyrData()$year)
      )
    }
  )
  
  ### Create Time Plot ----
  observeEvent(
    eventExpr = c(input$actualCountry, input$yearInterval),
    handlerExpr = {
      output$timePyramid <- renderCachedPlot(
        cacheKeyExpr = {list(input$actualCountry, input$yearInterval)},
        expr = {
          scaley <- if (input$actualCountry == "United Kingdom") {
            scale_y_continuous(
              limits = c(-1e6, 1e6),
              expand = expansion(mult = 0, add = 0),
              breaks = seq.int(from = -1e6, to = 1e6, by = 5e5),
              labels = c("1mil", "0.5mil", "0", "0.5mil", "1mil"))
            } else {
                scale_y_continuous(
                  limits = c(-4e6, 4e6),
                  expand = expansion(mult = 0, add = 0),
                  breaks = seq.int(from = -4e6, to = 4e6, by = 5e5),
                  labels = c("4mil","3.5mil","3mil","2.5mil", "2mil", "1.5mil",
                             "1mil", "0.5mil", "0", "0.5mil", "1mil", "1.5mil",
                             "2mil", "2.5mil","3mil","3.5mil","4mil")
                )
              }
          timePyrData() %>%
            filter(year == input$yearInterval) %>%
            na.omit() %>%
            ggplot(
              mapping = aes(x = age)
            ) +
            geom_col(
              mapping = aes(y = -1 * male),
              na.rm = TRUE,
              fill = psuPalette[1]
            ) +
            geom_col(
              mapping = aes(y = female),
              na.rm = TRUE,
              fill = psuPalette[2]
            ) +
            coord_flip() +
            theme_bw() +
            ylab("Population") +
            xlab("Age (years)") +
            labs(
              title = paste(input$actualCountry, "Population Pyramid for",
                            input$yearInterval),
              caption = "Blue is Males, Red is Females"
            ) +
            theme(
              axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
              text = element_text(size = 18),
              legend.position = "top"
            ) +
            scale_x_continuous(
              expand = expansion(mult = 0, add = c(0,1)),
              breaks = seq.int(from = 0, to = 100, by = 5),
              minor_breaks = NULL
            ) +
            scaley
        },
        alt = paste(input$actualCountry, "Population Pyramid for", input$timeInterval)
      )
    }
  )
  
  ## Fecundity Rate Plot ----
  observeEvent(
    eventExpr = input$fecundityCountry,
    handlerExpr = {
      output$fecundityPlot <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$fecundityCountry),
              message = "Select at least one country to the left"
            )
          )
          fertility %>%
            dplyr::filter(
              country %in% input$fecundityCountry
            ) %>%
            na.omit() %>%
            ggplot(
              mapping = aes(
                x = age,
                y = fertility,
                colour = country,
                linetype = country
              )
            ) +
            geom_path(linewidth = 2, na.rm = TRUE) +
            theme_bw() +
            xlab("Age (years)") +
            ylab("Number of births per 1000 women") +
            labs(
              title = "Fecundity Rate per 1,000 Women",
              color = "Country",
              linetype = "Country"
            ) +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.05), add = 0),
              limits = c(0, max(fertility$fertility))
            ) +
            scale_color_manual(values = countryColors) +
            scale_linetype_manual(values = countryLyt) +
            theme(legend.key.size = unit(2,"cm"))
        },
        alt = "Fecundity rate per 1000 women for selected countries"
      )
    },
    ignoreNULL = FALSE
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
