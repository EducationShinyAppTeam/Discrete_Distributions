# Load Packages ----
# library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(stats)
library(dplyr)
library(ggimage)
library(extraDistr)

# Load additional dependencies and setup functions
WINSCORE <- 5

questionBank <- read.csv("questionbank.csv")


# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Discrete Distributions",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Discrete_Distributions")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Discrete Distributions"),
          p("This app focuses on Bernoulli, Binomial, Hypergeometric,
            Geometric and Negative Binomial distributions.
            It will explore different sample spaces and sample paths for both categories of discrete
            distributions through an interactive graph.
            Also, it will help users to choose the suitable distribution in a real-world
            scenario and show the probability mass function graphs about that distribution."),
          h2("Instructions"),
          tags$ol(
            tags$li("Basic information about these distributions is introduced
                    on the 'Prerequisites' page."),
            tags$li("In the 'Explore' page, users can explore different sample
                    spaces and sample paths by changing the number of Successes/Trials
                    and the Probability of Success with adjustable sliders."),
            tags$li("In the 'Game' page, a real-world scenario is given where users
                    need to choose the suitable distribution.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "Explore",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Yijun Yao in
            2022. The app was further updated by Nathan Pechulis in June 2024.
            Special thanks to Neil for being incredibly helpful with programming
            issues.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 07/10/2024 by NP.")
          )
        ),
        #### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Background and Types of Distributions"),
          p("Expand each of the boxes below to further your understanding about
             discrete distributions by clicking on the plus sign to the right of
            each label."),
          br(),
          box(
            title = tags$strong("Background Information"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(
              tags$strong("Distributions: "), "A probability distribution is a function that maps the probability
              for all potential events associated with a", tags$strong("random variable"), "(function that assigns a number to each
              outcome in any event). Distributions can be either", tags$strong("discrete,"), "where the data takes on only
              specific values (like shoe size that comes in half or full size increments), or", tags$strong("continuous,"), "where the data can take on any
              value within a certain interval (like volume or mass). But, in this app, we will be exclusively examining
              discrete distributions.",
              br(),
              br(),
              tags$strong("PMF: "), "The probability mass function, or PMF, of a discrete distribution is a function
              that describes how likely possible values of a random variable are to occur. The", tags$strong("CDF"), "(cumulative distribution function)
              of a random variable is another helpful function that displays the probability that a random variable is less than or equal to
              a particular value.",
              br(),
              br(),
              tags$strong("Expected Value: "), 'The expected value for a random variable is a measure of
              its central tendency and is a synonym for the mean. It is denoted by "E(X)," and it can be calculated by summing
              over the possible values times the chance of those values.',
              br(),
              br(),
              tags$strong("Sample Path: "), "A sample path is essentially a sequence that demonstrates the possible steps that are
              taken by a variable as each trial of the underlying process occurs."
            )
          ),
          box(
            title = tags$strong("Bernoulli Distribution"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "A Bernoulli variable, X, is a
              discrete variable which has only two outcomes: 1 (success) and 0 (failure).",
              tags$em('p'), "is the probability of 1.",
              br(),
              tags$strong("Notation: "), HTML("X ~ Bern(<em>p</em>)"),
              br(),
              tags$strong("E(X): "), tags$em("p")
            )
          ),
          box(
            title = tags$strong("Binomial Distribution"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "A Binomial variable, X,
              is a discrete variable measuring the number of successes in", tags$em('n'),
              "independent experiments with probability", tags$em('p'), "of success in each
              trial. It is drawn with replacement, which means drawn items
              are replaced in the sample space, so", tags$em('p'), "stays the same for each
              trial and trials are independent and identically distributed (iid).",
              br(),
              tags$strong("Notation: "), HTML("X ~ Bin(<em>n</em>, <em>p</em>)"),
              br(),
              tags$strong("Special Case: "), "Since a Binomial variable is", tags$em("n"),
              "iid Bernoulli variables,", HTML("Bern(<em>p</em>)"), "is equivalent to",
              HTML("Bin(<em>n</em> = 1, <em>p</em>)."),
              br(),
              tags$strong("E(X): "), tags$em("np")
            )
          ),
          box(
            title = tags$strong("Hypergeometric Distribution"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "A Hypergeometric variable, X,
              is a discrete variable which measures the number of successes in",
              tags$em('n'), "draws from a population with", tags$em('N'), "objects
              of which", tags$em('M'), "are successes. It is similar to Binomial,
              but Hypergeometric is drawn without replacement, which means that
              drawn items are not replaced in the sample space and thus the
              probability of success changes for each trial",
              br(),
              tags$strong("Notation: "), HTML("X ~ HG(<em>n</em>, <em>M</em>, <em>N</em>)"),
              br(),
              tags$strong("E(X): "), HTML("<em>nM/N</em>")
            )
          ),
          box(
            title = tags$strong("Geometric Distribution"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "A Geometric variable, X, is a
              discrete variable which describes the number of independent
              trials until the", tags$strong("first"), "success where each trial
              has probability", tags$em('p'), "of success.",
              br(),
              tags$strong("Notation: "), HTML("X ~ Geom(<em>p</em>)"),
              br(),
              tags$strong("E(X): "), HTML("1/<em>p</em>")
            )
          ),
          box(
            title = tags$strong("Negative Binomial Distribution"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "A Negative Binomial variable, X,
              is a discrete variable which describes the number of independent
              trials before the", tags$strong("rth"), "success where each trial has
              probability", tags$em('p'), "of success.",
              br(),
              tags$strong("Notation: "), HTML("X ~ NBin(<em>r</em>, <em>p</em>)"),
              br(),
              tags$strong("Special Case: "), "Since a Negative Binomial variable is
              an extension of the Geometric variable,", HTML('Geom(<em>p</em>)'),
              "is equivalent to", HTML('NBin(<em>r</em> = 1, <em>p</em>).'),
              br(),
              tags$strong("E(X): "), HTML("<em>r/p</em>")
            )
          ),
          box(
            title = tags$strong("Summary"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Binomial"), "and",
              tags$strong("Hypergeometric"),
              "distributions: the number of trials is ", strong("fixed and known"),
              "while the number of successes is the ",
              strong("random variable"), "of interest.",
              br(),
              br(),
              tags$strong("Geometric"), "and",
              tags$strong("Negative Binomial"),
              "distributions: the number of trials is the ",
              strong("random variable"),
              "of interest while the number of successes is",
              strong("fixed and known. ")
            )
          )
        ),
        #### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Exploring Distributions"),
          p("Use the tabs below to move between contexts where the number of trials
            is random and those where the numbrer of successes is random."),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Random: Trials",
              h3("Instructions"),
              p("For this part, the number of trials is what we are interested in
                and the number of successes is fixed, so", strong("Geometric"), "and ",
                strong("Negative Binomial"), "distributions are suitable. Adjust the sliders
                for the number of successes, probability of success, and number of sample
                paths and observe how the graph and sample path(s) respond."
                ),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "numSs",
                      label = "Total number of successes",
                      min = 1,
                      max = 10,
                      step = 1,
                      value = 4
                    ),
                    sliderInput(
                      inputId = "probSuccess",
                      label = "Probability of success",
                      min = 0.1,
                      max = 1,
                      value = 0.3,
                      step = 0.1
                    ),
                    sliderInput(
                      inputId = "samPath1",
                      label = "Number of sample paths",
                      min = 1,
                      max = 3,
                      step = 1,
                      value = 1
                    ),
                    bsButton(
                      inputId = "samButton1",
                      label = "New Sample",
                      icon = icon("retweet"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "trialsPlot", width = "100%"),
                  p(strong("Key ")),
                  p(img(src = "black dot.webp", width = "30px", alt = "black dot"),
                    "Black dots represent the sample space."),
                  p(img(src = "steplines.jpg", width = "30px", alt = "steps"),
                    "Each line represents one sample path, which is
                    influenced by the success number and probability of success."),
                  p(img(src = "diamond.png", width = "20px", alt = "blue diamond"),
                    " The blue diamond represents the expected value.")
                )
              )
            ),
            tabPanel(
              title = "Random: Successes",
              h3("Instructions"),
              p("For this part, the number of successes is what we are interested in
                and the number of trials is fixed, so", strong("Bernoulli"), "and ",
                strong("Binomial"), "distributions are suitable. Adjust the sliders
                for the number of trials, probability of success, and number of sample
                paths and observe how the graph and sample path(s) respond."
              ),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "numTs",
                      label = "Total number of trials",
                      min = 0,
                      max = 10,
                      step = 1,
                      value = 4
                    ),
                    sliderInput(
                      inputId = "probSucc",
                      label = "Probability of success",
                      min = 0.1,
                      max = 1,
                      value = 0.3,
                      step = 0.1
                    ),
                    sliderInput(
                      inputId = "samPath2",
                      label = "Number of sample paths",
                      min = 1,
                      max = 3,
                      step = 1,
                      value = 1
                    ),
                    bsButton(
                      inputId = "samButton2",
                      label = "New Sample",
                      icon = icon("retweet"),
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "successPlot", width = "100%"),
                  p(strong("Key ")),
                  p(img(src = "black dot.webp", width = "30px", alt = "black dot"),
                    "Black dots represent the sample space."),
                  p(img(src = "steplines.jpg", width = "30px", alt = "steps"),
                    "Each line represents one sample path, which is
                    influenced by the success number and probability of success."),
                  p(img(src = "diamond.png", width = "20px", alt = "blue diamond"),
                    " The blue diamond represents the expected value.")
                )
              )
            )
          )
        ),
        #### Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Choosing Distributions Game"),
          p(strong("Instructions: "), "For each
              question, choose the most suitable distribution for that situation.
              The graph of the probability mass function will appear as a hint
              when you click the 'Hint' button. The game is won when you get 5
              correct answers, but keep in mind that the man will fall off of the
              tree after 4 incorrect guesses and the game will be over. If you lose,
              use the 'Restart' button to reset your progress."),
          selectInput(
            inputId = "backSce",
            label = "Background Scenario",
            choices = c('Scenario A', 'Scenario B', 'Scenario C'),
            selected = 'Scenario A'
          ),
          uiOutput(outputId = "scenarioText"),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                h3("Question"),
                br(),
                uiOutput("question"),
                br(),
                radioGroupButtons(
                  inputId = "mc1",
                  label = "Which distribution is relevant for the question?",
                  direction = "vertical",
                  selected = character(0),
                  checkIcon = list(
                    yes = icon("square-check"),
                    no = icon("square")
                  ),
                  choices = list(
                    "Bernoulli",
                    "Binomial",
                    "Hypergeometric",
                    "Geometric",
                    "Binomial Distribution"
                  ),
                  size = "lg",
                  status = "textGame"
                ),
                uiOutput("mark"),
                uiOutput(outputId = "feedback"),
                br(),
                bsButton(
                  inputId = "submit",
                  label = "Submit",
                  size = "large",
                  style = "default",
                  disabled = FALSE
                ),
                br(),
                bsButton(
                  inputId = "nextQuestion",
                  label = "Next Question",
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                bsButton(
                  inputId = "restart",
                  label = "Restart",
                  size = "large",
                  style = "danger",
                  disabled = FALSE
                )
              )
            ),
            column(
              width = 6,
              uiOutput("correct", align = "center"),
              uiOutput("gameProgressTree", align = "center"),
              bsButton(
                inputId = "hint",
                label = "Hint",
                size = "large",
                type = "toggle",
                value = FALSE
              ),
              plotOutput(outputId = "hintPlot", width = "400"),
              tags$script(HTML(
                "$(document).ready(function() {
             document.getElementById('hintPlot').setAttribute('aria-describedby',
            'ariaText')
            })"
              )),
              uiOutput(outputId = "ariaText")
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Eric Bailey (2022). shinyBS: Twitter Bootstrap Components for Shiny.
          (v 0.61.1) [R package]. Available from
          https://CRAN.R-project.org/package=shinyBS"
          ),
          p(class = "hangingindent",
            "Guangchuang Yu (2022). ggimage: Use Image in 'ggplot2'.
            (v 0.3.1.) [R package]. Available from
            https://CRAN.R-project.org/package=ggimage"
            ),
          p(class = "hangingindent",
            "Hadley Wickham, Romain Franlois, Lionel Henry and Kirill Muller (2021).
          dplyr: A Grammar of Data Manipulation. (v 1.0.7.) [R package].
          Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(class = "hangingindent",
            "Robert Carey and Neil Hatfield (2022).
            boastUtils: BOAST Utilities. (v 0.1.12.3.)[R package]
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
            ),
          p(class = "hangingindent",
            "R Core Team (2021). R: A language and environment for statistical
          computing. R Foundation for Statistical Computing, Vienna, Austria.
          [R package] Available from https://www.R-project.org/."
          ),
          p(class = "hangingindent",
            "Tymoteusz Wolodzko (2020). extraDistr: Additional Univariate and
            Multivariate Distributions.(v 1.9.1.)
            Available from https://CRAN.R-project.org/package=extraDistr"
            ),
          p(class = "hangingindent",
            "Victor Perrier, Fanny Meyer and David Granjon (2022).
            shinyWidgets: Custom Inputs Widgets for Shiny. (v 0.7.0.)[R package]
            Available from https://CRAN.R-project.org/package=shinyWidgets"
            ),
          p(class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(class = "hangingindent",
            "Winston Chang and Barbara Borges Ribeiro (2021).
          shinydashboard: Create Dashboards with 'Shiny'.
          (v 0.7.2.)[R package]
          Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          br(),
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

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Instructions:",
        text = 'In the Explore Page, change the number of Successes/Trials and Probability
        to see the changes in the sample space and sample path. On the Game Page, pick a
        scenario and choose the distribution that best matches each question. Click the
        "Hint" button to see a graph of the PMF to help.'
      )
    }
  )

  ## Overview Go Button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    })

  ## Explore page ----
  ### Random Trials ----
  observeEvent(
    eventExpr = c(input$numSs, input$probSuccess, input$samPath1, input$samButton1),
    handlerExpr = {
      #### Simulate Data ----
      trials <- rnbinom(n = 3, size = input$numSs, prob = input$probSuccess) + input$numSs
      # rnbinom get the number of failures, so adding the number of successes on it
      # would give us the number of trials
      # The n argument will generate three paths' worth of information
      Path1 <- data.frame(
        trial = 1:trials[1],
        success = rep(0, trials[1])
      )
      successes1 <- c(sort(sample(x = 1:(nrow(Path1) - 1), size = (input$numSs - 1))),
                      nrow(Path1))

      Path1 <- Path1 %>%
        mutate(
          success = if_else(
            condition = trial %in% successes1,
            true = 1,
            false = 0
          )
        )
      Path1$cSuccess <- cumsum(Path1$success)

      Path2 <- data.frame(
        trial = 1:trials[2],
        success = rep(0, trials[2])
      )

      successes2 <- c(sort(sample(1:(nrow(Path2) - 1),
                                  size = (input$numSs - 1))), nrow(Path2))

      Path2 <- Path2 %>%
        mutate(
          success = if_else(
            condition = trial %in% successes2,
            true = 1,
            false = 0
          )
        )
      Path2$cSuccess <- cumsum(Path2$success)

      Path3 <- data.frame(
        trial = 1:trials[3],
        success = rep(0, trials[3])
      )

      successes3 <- c(sort(sample(1:(nrow(Path3) - 1), size = (input$numSs - 1))), nrow(Path3))

      Path3 <- Path3 %>%
        mutate(
          success = if_else(
            condition = trial %in% successes3,
            true = 1,
            false = 0
          )
        )
      Path3$cSuccess <- cumsum(Path3$success)

      expected <- input$numSs/input$probSuccess

      minVal <- 5
      maxTrials <- ifelse(
        test = max(trials) > expected,
        yes = max(trials) + 1,
        no = ceiling(expected) + 1
      )
      maxTrials <- max(minVal, maxTrials)

      successVector <- c()
      for (i in 1:maxTrials) {
        successVector <- c(successVector, 0:i)
      }

      points <- data.frame(
        success = successVector,
        Trials = rep(x = 1:maxTrials, times = 2:(maxTrials + 1))
      )
      # print(Path1)

      # there is an error about sign of 'by' parameter when success number equals
      # to one and probability is larger than 0.03. That's an important part you
      # need to fix. I really appreciate for your efforts!

      #### Create Plot ----
      output$trialsPlot <- renderPlot(
        expr = {
          a <- ggplot(
            data = points
          ) +
            geom_point(
              mapping = aes(x = success, y = Trials),
              size = 2,
              na.rm = TRUE
            ) +
            annotate(
              geom = "point",
              x = input$numSs,
              y = expected,
              shape = 23,
              fill = "blue",
              color = "blue",
              size = 6
            ) +
            geom_step(
              data = Path1,
              mapping = aes(x = cSuccess, y = trial),
              color = psuPalette[1],
              na.rm = TRUE,
              linewidth = 1
            ) +
            geom_image(
              inherit.aes = FALSE,
              data = data.frame(
                x = 0:input$numSs,
                y = rep(x = (maxTrials + 1.5), times = input$numSs + 1),
                image = rep("www/arrow up.png", times = input$numSs + 1)
              ),
              mapping = aes(x = x, y = y, image = image)
            ) +
            scale_x_continuous(
              breaks = 0:input$numSs,
              limits = c(0,input$numSs)
            ) +
            scale_y_continuous(
              breaks = c(1, seq(from = 5, to = maxTrials, by = 5)),
              minor_breaks = seq(1, maxTrials, 1),
              expand = expansion(mult = c(0,0.05), add = c(1, 0))
            ) +
            labs(title = "Sample Path(s) for Chosen Sample") +
            xlab(label = paste("Success Number out of", input$numSs, "Total")) +
            ylab(label = "Trial Number") +
            theme_bw() +
            theme(
              text = element_text(size = 18)
            )

          if ( input$samPath1 == 2 ) {
            a <- a + geom_step(
              data = Path2,
              mapping = aes(x = cSuccess, y = trial),
              na.rm = TRUE,
              color = psuPalette[2],
              linewidth = 1,
              position = position_nudge( x = 0.02, y = 0.02)
            )
          }

          if ( input$samPath1 == 3 ) {
            a <- a + geom_step(
              data = Path2,
              mapping = aes(x = cSuccess, y = trial),
              na.rm = TRUE,
              color = psuPalette[2],
              linewidth = 1,
              position = position_nudge( x = 0.02, y = 0.02)
            ) +
              geom_step(
                data = Path3,
                mapping = aes(x = cSuccess, y = trial),
                na.rm = TRUE,
                color = psuPalette[3],
                linewidth = 1,
                position = position_nudge( x = -0.02, y = -0.02)
              )
          }
          a
        },
        alt = "Here is the graph of sample space which includes sample path and
        expected value"
      )
    }
  )

  ### Random Successes ----
  observeEvent(
    eventExpr = c(input$numTs, input$probSucc, input$samPath2, input$samButton2),
    handlerExpr = {
      #### Create Plot ----
      output$successPlot <- renderPlot(
        expr = {
          samPath <- data.frame(
            trial = seq.int(from = 0, to = input$numTs, by = 1),
            success1 = c(0, runif(n = input$numTs, min = 0, max = 1)),
            success2 = c(0, runif(n = input$numTs, min = 0, max = 1)),
            success3 = c(0, runif(n = input$numTs, min = 0, max = 1))
          ) %>%
            dplyr::mutate(
              success1 = case_when(
                trial == 0 ~ 0,
                TRUE ~ if_else(
                  condition = success1 <= input$probSucc,
                  true = 1,
                  false = 0
                )
              ),
              success2 = case_when(
                trial == 0 ~ 0,
                TRUE ~ if_else(
                  condition = success2 <= input$probSucc,
                  true = 1,
                  false = 0
                )
              ),
              success3 = case_when(
                trial == 0 ~ 0,
                TRUE ~ if_else(
                  condition = success3 <= input$probSucc,
                  true = 1,
                  false = 0
                )
              )
            )
          samPath$sumSuccess1 <- cumsum(samPath$success1)
          samPath$sumSuccess2 <- cumsum(samPath$success2)
          samPath$sumSuccess3 <- cumsum(samPath$success3)

          temp <- c()
          for (i in 0:input$numTs) {
            temp <- c(temp, i:input$numTs)
          }
          samSpace <- data.frame(
            ys = rep(x = 0:input$numTs, times = (1 + input$numTs):1),
            xs = temp
          )

          b <- ggplot(
            data = samSpace
            ) +
            geom_point(mapping = aes(x = xs, y = ys), size = 2, na.rm = TRUE) +
            annotate(
              geom = "point",
              x = input$numTs,
              y = input$numTs*input$probSucc,
              shape = 23,
              fill = "blue",
              color = "blue",
              size = 6
            ) +
            scale_x_continuous(
              breaks = 0:input$numTs,
              minor_breaks = NULL
            ) +
            scale_y_continuous(
              breaks = 0:input$numTs,
              minor_breaks = NULL
            ) +
            labs(title = "Sample Path(s) for Chosen Sample") +
            xlab(label = paste("Trial Number out of", input$numTs, "Total")) +
            ylab(label = "Success Number") +
            theme_bw() +
            theme(
              text = element_text(size = 18)
            )

          for (i in 1:input$samPath2) {
            b <- b + geom_step(
              data = samPath,
              mapping = aes(
                x = .data[["trial"]],
                y = .data[[paste0("sumSuccess",i)]]
              ),
              color = psuPalette[i],
              na.rm = TRUE,
              position = position_nudge(
                x = if_else(i == 1, 0, if_else(i == 2, 0.02, -0.02)),
                y = if_else(i == 1, 0, if_else(i == 2, 0.02, -0.02))
              ),
              linewidth = 1)
          }
          b
        },
        alt = "Here is the graph of sample space which includes sample path and
        expected value"
      )
    }
  )

  ## Game page ----
  ### Set Up Game Variables ----
  gameQuestions <- reactiveVal(
    label = "Game Questions",
    value = slice_sample(questionBank, n = nrow(questionBank))
  )
  gameVariables <- reactiveValues(
    correct = 0,
    mistakes = 0,
    sceneAQ = 1,
    sceneBQ = 1,
    sceneCQ = 1,
    currentIndex = NULL
  )

  ### Scenario Selection ----
  observeEvent(
    eventExpr = input$backSce,
    handlerExpr = {
      #### Display Scenario Text ----
      sceneText <- gameQuestions() %>%
        filter(scenario == input$backSce) %>%
        dplyr::select(scenarioText)
      output$scenarioText <- renderUI(
        expr = {
          p(tags$strong(sceneText$scenarioText[1]))
        }
      )

      #### Set Current Index for Scenario Change ----
      gameVariables$currentIndex <- switch(
        EXPR = input$backSce,
        `Scenario A` = gameVariables$sceneAQ,
        `Scenario B` = gameVariables$sceneBQ,
        `Scenario C` = gameVariables$sceneCQ
      )

      #### Adjust feedback and buttons -----
      output$mark <- renderIcon()
      output$feedback <- renderUI({NULL})

      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )

      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )

      updateButton(
        session = session,
        inputId = "hint",
        label = "Show hint",
        value = FALSE
      )
    }
  )

  ### Display Question ----
  output$question <- renderUI(
    expr = {
      questionText <- gameQuestions() %>%
        filter(scenario == input$backSce)
      p(questionText$question[gameVariables$currentIndex])
    }
  )

  ### Display Shuffled Answers ----
  observeEvent(
    eventExpr = c(gameVariables$currentIndex, input$backSce),
    handlerExpr = {
      shuffledLetters <- sample(x = LETTERS[1:5], size = 5, replace = FALSE)
      shuffledChoices <- as.character(
        gameQuestions()[gameVariables$currentIndex, shuffledLetters]
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "mc1",
        selected = character(0),
        choices = list(
          shuffledChoices[1],
          shuffledChoices[2],
          shuffledChoices[3],
          shuffledChoices[4],
          shuffledChoices[5]
        ),
        checkIcon = list(
          yes = icon("square-check"),
          no = icon("square")
        ),
        status = "textGame",
        size = "lg"
      )
    }
  )

  ### Next Question ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "hint",
        label = "Show hint",
        value = FALSE
      )
      #### Check open questions ----
      filteredQuestions <- gameQuestions() %>%
        filter(scenario == input$backSce)
      if (gameVariables$currentIndex < nrow(filteredQuestions)) {
        ##### Iterate scene count ----
        if (input$backSce == "Scenario A") {
          gameVariables$sceneAQ <- gameVariables$sceneAQ + 1
        } else if (input$backSce == "Sencario B") {
          gameVariables$sceneBQ <- gameVariables$sceneBQ + 1
        } else {
          gameVariables$sceneCQ <- gameVariables$sceneCQ + 1
        }

        ##### Iterate Current Index -----
        gameVariables$currentIndex <- gameVariables$currentIndex + 1

        ##### Update Buttons and Fields ----
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        output$mark <- renderIcon()
        output$feedback <- renderUI({NULL})
        output$hintPlot <- renderUI({NULL})
        output$PlotText <- renderUI({NULL})
      } else {
        #### No Open Questions ----
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "warning",
          text = "You've used all of the questions in this scenario,
            please go to another one to continue."
        )
      }
    }
  )

  ### Submit Button ----
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      #### Update buttons ----
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = FALSE
      )
      #### Filter Game Questions ----
      filteredQuestions <- gameQuestions() %>%
        filter(scenario == input$backSce)

      #### Check Answer ----
      if (input$mc1 == filteredQuestions$ansValue[gameVariables$currentIndex]) {
        output$mark <- renderIcon(icon = "correct", width = 50)
        gameVariables$correct <- gameVariables$correct + 1
        output$feedback <- renderUI({p("Correct!")})
      } else {
        output$mark <- renderIcon(icon = "incorrect", width = 50)
        gameVariables$mistakes <- gameVariables$mistakes + 1
        output$feedback <- renderUI({
          p(filteredQuestions$feedback[gameVariables$currentIndex])
        })
      }

      #### Game Over Check ----
      if (gameVariables$correct >= WINSCORE) {
        sendSweetAlert(
          session = session,
          title = "You Win!",
          type = "success",
          text = "You have won the game! Congrats!"
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      } else if (gameVariables$mistakes  >= 4) {
        sendSweetAlert(
          session = session,
          title = "You lost.",
          type = "error",
          text = "You have lost the game. Please try again.",
          closeOnClickOutside = FALSE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      }
    })

  ### Reset button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
      #### Clear Feedback ----
      output$mark <- renderIcon()
      output$feedback <- renderUI({NULL})

      #### Reset Game Variables ----
      gameVariables$correct <- 0
      gameVariables$mistakes <- 0
      gameVariables$sceneAQ <- 1
      gameVariables$sceneBQ <- 1
      gameVariables$sceneCQ <- 1
      gameVariables$currentIndex <- 1

      #### Reshuffle Questions ----
      gameQuestions(slice_sample(questionBank, n = nrow(questionBank)))

      #### Adjust Buttons ----
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "hint",
        label = "Show hint",
        value = FALSE
      )
    }
  )

  ### Display hint ----
  observeEvent(
    eventExpr = input$hint,
    handlerExpr = {
      # input$hint toggles between TRUE/FALSE, starting with FALSE
      # Show hint on input$hint == TRUE
      if (!input$hint) {
        output$hintPlot <- renderPlot({NULL})
        output$ariaText <- renderUI({NULL})
        updateButton(
          session = session,
          inputId = "hint",
          label = "Show hint"
        )
      } else {
        updateButton(
          session = session,
          inputId = "hint",
          label = "Hide hint"
        )

        #### Filter questions ----
        filteredQuestions <- gameQuestions() %>%
          filter(scenario == input$backSce)

        #### Core hint plot ----
        coreHintPlot <- ggplot() +
          theme_bw() +
          labs(
            title = "PMF Graph",
            x = filteredQuestions$xlab[gameVariables$currentIndex],
            y = filteredQuestions$ylab[gameVariables$currentIndex]
          ) +
          scale_x_continuous(
            expand = expansion(mult = c(0.15, 0))
          ) +
          scale_y_continuous(
            expand = expansion(add = c(0, 0.05))
          ) +
          theme(
            text = element_text(size = 20),
            axis.title.y = element_text(size = 14)
          )

        #### Make hint plot ----
        if (filteredQuestions$fun[gameVariables$currentIndex] == "dbern") {
         currentHintPlot <- coreHintPlot +
            stat_function(
              fun = str2lang(filteredQuestions$fun[gameVariables$currentIndex]),
              geom = str2lang(filteredQuestions$geom[gameVariables$currentIndex]),
              args = str2lang(filteredQuestions$args[gameVariables$currentIndex]),
              xlim = str2lang(filteredQuestions$xlim[gameVariables$currentIndex]),
              n = filteredQuestions$n[gameVariables$currentIndex],
              na.rm = TRUE,
              color = "skyblue",
              linewidth = 1.5
            )
        } else {
          currentHintPlot <- coreHintPlot +
            stat_function(
              fun = str2lang(filteredQuestions$fun[gameVariables$currentIndex]),
              geom = str2lang(filteredQuestions$geom[gameVariables$currentIndex]),
              args = str2lang(filteredQuestions$args[gameVariables$currentIndex]),
              xlim = str2lang(filteredQuestions$xlim[gameVariables$currentIndex]),
              n = filteredQuestions$n[gameVariables$currentIndex],
              na.rm = TRUE,
              fill = "skyblue"
            )
        }

        #### Display hint plot ----
        output$hintPlot <- renderPlot(
          expr = currentHintPlot,
          alt = "Graph of PMF"
        )

        #### Display hint aria text -----
        output$ariaText <- renderUI(
          expr = {
            p(tags$strong("Graph Caption: "),
              filteredQuestions$aria[gameVariables$currentIndex])
          }
        )
      }
    }
  )

  ### Display score ----
  output$correct <- renderUI({
    paste("Number of questions answered correctly:", gameVariables$correct)
  })

  ### Cartoon Display ----
  output$gameProgressTree <- renderUI({
    img(src = "Cell01.jpg")
    if (gameVariables$mistakes == 0) {
      img(
        src = "Cell01.jpg",
        width = "100%",
        alt = "The man is on the top branch"
      )
    } else if (gameVariables$mistakes == 1) {
      img(
        src = "Cell02.jpg",
        width = "100%",
        alt = "The man has fallen one branch"
      )
    } else if (gameVariables$mistakes == 2) {
      img(
        src = "Cell03.jpg",
        width = "100%",
        alt = "The man has fallen another branch, only one remaining"
      )
    } else if (gameVariables$mistakes == 3) {
      img(
        src = "Cell04.jpg",
        width = "100%",
        alt = "The man has fallen to the last branch"
      )
    } else if (gameVariables$mistakes >= 4) {
      img(
        src = "Cell05.jpg",
        width = "100%",
        alt = "The man has fallen to the ground"
      )
    }
  })

 }

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)