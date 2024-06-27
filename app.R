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
# source("global.R")

bank <- read.csv("questionbank.csv")
bank2 <- read.csv("questionbank2.csv")
bank3 <- read.csv("questionbank3.csv")



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
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Discrete Distributions"), # This should be the full name.
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
            div(class = "updated", "Last Update: 6/27/2024 by NP.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Background and Types of Distributions"),
          br(),
          box(
            title = "Background Information",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(
              tags$strong("Distributions: "), "A probability distribution is a function that maps the probability
              for all potential events associated with a", tags$strong("random variable"), "(function that assigns a number to each 
              outcome in an event). Distributions can be either", tags$strong("discrete,"), "where the data takes on only
              specific values (like shoe sizing that comes in half or full size increments), or", tags$strong("continuous,"), "where the data can take on any 
              decimal value within a certain interval (like volume or mass). But, in this app, we will be exclusively examining 
              discrete distributions.",
              br(),
              br(),
              tags$strong("PMF: "), "The probability mass function, or PMF, of a discrete distribution is a function
              that describes how likely possible values in a random variable are to occur. The", tags$strong("CDF"), "(cumulative distribution function)
              of a random variable is another helpful function that displays the probability that a random variable is less than or equal to
              than a particular value.",
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
            title = "Bernoulli Distribution",
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
            title = "Binomial Distribution",
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
            title = "Hypergeometric Distribution",
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
            title = "Geometric Distribution",
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
            title = "Negative Binomial Distribution",
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
            title = "Summary",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
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
        
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Exploring Distributions"),
          tabsetPanel(
            type = "tabs",
            tabPanel( 
              title = "Random: Trials",
              h3("Instructions"),
              p("For this part, the Trial Number is what we are interested in 
                and the Success Number is fixed, so", strong("Geometric"), "and ", 
                strong("Negative Binomial"), "distributions are suitable. Adjust the sliders
                for the number of successes, probability of success, and number of sample
                paths and observe how the graph and sample paths respond."
                ),
              br(),
              sidebarLayout(
                sidebarPanel(
                  sliderInput(
                    inputId = "numSs",
                    label = "Number of Successes",
                    min = 1,
                    max = 10,
                    step = 1,
                    value = 4
                  ),
                  sliderInput(
                    inputId = "probSuccess",
                    label = "Probability of Success",
                    min = 0.1,
                    max = 1,
                    value = 0.3,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "samPath1",
                    label = "Number of Sample Paths",
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
                ),
                mainPanel(
                  plotOutput(outputId = "trialsPlot", width = "100%"),
                  p(strong("Key: ")),
                  p(img(src = "black dot.webp", width = "30px"),
                        "Black dots represent the sample space."),
                  p(img(src = "steplines.jpg", width = "30px"),
                    "Each line represents one sample path, which is 
                    influenced by the success number and probability of success."),
                  p(img(src = "rhombus.webp", width = "20px"),
                    " The blue diamond represents the expected value.")
                )
              )
            ),
            tabPanel( 
              title = "Random: Successes",
              h3("Instructions"),
              p("For this part, the Success Number is what we are interested in 
                and the Trial Number is fixed, so", strong("Bernoulli"), "and ", 
                strong("Binomial"), "distributions are suitable. Adjust the sliders
                for the number of trials, probability of success, and number of sample
                paths and observe how the graph and sample paths respond."
              ),
              br(),
              sidebarLayout(
                sidebarPanel(
                  sliderInput(
                    inputId = "numTs",
                    label = "Number of Trials",
                    min = 0,
                    max = 10,
                    step = 1,
                    value = 4
                  ),
                  sliderInput(
                    inputId = "probSucc",
                    label = "Probability of Success",
                    min = 0.1,
                    max = 1,
                    value = 0.3,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "samPath2",
                    label = "Number of Sample Paths",
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
                ),
                mainPanel(
                  plotOutput(outputId = "successPlot", width = "100%"),
                  p(strong("Key: ")),
                  p(img(src = "black dot.webp", width = "30px"),
                    "Black dots represent the sample space."),
                  p(img(src = "steplines.jpg", width = "30px"),
                    "Each line represents one sample path, which is 
                    influenced by the success number and probability of success."),
                  p(img(src = "rhombus.webp", width = "20px"),
                    " The blue diamond represents the expected value.")
                )
              )
            )
          )
        ),
        #### Set up a Game Page ----
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
            inputId = "backSce", label = "Background Scenario",
            choices = c('Scenario A', 'Scenario B', 'Scenario C'),
            selected = 'Scenario A'
          ),
          conditionalPanel(
            condition = "input.backSce == 'Scenario A'",
              p(tags$strong("There are 30 balls in a box and 6 of them are red."))
          ),
          conditionalPanel(
            condition = "input.backSce == 'Scenario B'",
              p(tags$strong("In Pennsylvania, 10% of vehicles order a Big Mac in the 
                drive thru window of McDonalds."))
          ),
          conditionalPanel(
            condition = "input.backSce == 'Scenario C'",
              p(tags$strong("40% of the people in a college town have type A blood.
                There is a Red Cross Station where people from the town donate blood."))
          ),
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
                    yes = icon("check-square")
                  ),
                  choices = list(
                    "Bernoulli",
                    "Binomial",
                    "Hypergeometric",
                    "Geometric",
                    "Binomial Distribution"
                  ),
                  width = "100%",
                  justified = TRUE,
                  individual = FALSE
                ),
                uiOutput("mark"),
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
                size = "large"
            ),
            plotOutput(outputId = "Plot", width = "400"),
            tags$script(HTML(
              "$(document).ready(function() {
             document.getElementById('Plot').setAttribute('aria-describedby',
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
  
  ## Prereq's "Go" Button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    })
  
  ## set up the explore page ----
  ### random trials ----
  
  observeEvent(
    eventExpr = c(input$numSs, input$probSuccess, input$samPath1, input$samButton1),
    handlerExpr = {
      trials <- rnbinom(3, size = input$numSs, prob = input$probSuccess) + input$numSs 
      # rnbinom get the number of failures, so adding the number of successes on it 
      # would give us the number of trials
      Path1 <- data.frame(
        trial = 1:trials[1],
        success = rep(0, trials[1])
      )
      successes1 <- c(sort(sample(1:(nrow(Path1) - 1), size = (input$numSs - 1))), nrow(Path1))
      
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
              size = 1
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
            xlab(label = "Success Number") +
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
              size = 1,
              position = position_nudge( x = 0.02, y = 0.02)
            )
          }
          
          if ( input$samPath1 == 3 ) {
            a <- a + geom_step(
              data = Path2,
              mapping = aes(x = cSuccess, y = trial),
              na.rm = TRUE,
              color = psuPalette[2],
              size = 1,
              position = position_nudge( x = 0.02, y = 0.02) 
            ) + 
              geom_step(
                data = Path3,
                mapping = aes(x = cSuccess, y = trial),
                na.rm = TRUE,
                color = psuPalette[3],
                size = 1,
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
  
  ### random Successes ----
  
  observeEvent(
    eventExpr = c(input$numTs, input$probSucc, input$samPath2, input$samButton2),
    handlerExpr = {
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
            ys = rep(x = 0:input$numTs, times = (1+input$numTs):1),
            xs = temp
          )
          
          b <- ggplot( 
            data = samSpace) +
            geom_point(aes(x = xs, y = ys), size = 2, na.rm= TRUE) + 
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
            xlab(label = "Trial Number") +
            ylab(label = "Success Number") +
            theme_bw() + 
            theme(
              text = element_text(size = 18)
            )
          
          for (i in 1:input$samPath2){ 
            b <- b + geom_step(data = samPath, 
                               aes_string(x = "trial", 
                                          y = as.name(paste0("sumSuccess",i))),
                               color = psuPalette[i], 
                               na.rm = TRUE,
                               position = position_nudge(
                                 x = if_else( 
                                   i == 1, 0, if_else(i == 2, 0.02, -0.02)
                                   ),
                                 y = if_else( 
                                   i == 1, 0, if_else(i == 2, 0.02, -0.02))
                                 ),
                               size = 1)
          }
          b
        },
        alt = "Here is the graph of sample space which includes sample path and 
        expected value"
      )
    }
  )
  ## set up the game page ---- 
  ### Set Up Game Variables ----
  
  shuffledProbIDs <- sample(
    x = seq_len(nrow(bank)),
    size = nrow(bank),
    replace = FALSE
  )
  
  shuffledProbIDs_2 <- sample(
    x = seq_len(nrow(bank2)),
    size = nrow(bank2),
    replace = FALSE
  )
  
  shuffledProbIDs_3 <- sample(
    x = seq_len(nrow(bank3)),
    size = nrow(bank3),
    replace = FALSE
  )
  
  scoring <- reactiveValues(
    correct = 0,
    mistakes = 0,
    id = 1,
    questionNum = shuffledProbIDs[1]
  )
  
  scoring_2 <- reactiveValues(
    correct = 0,
    mistakes = 0,
    id = 1,
    questionNum = shuffledProbIDs_2[1]
  )
  
  scoring_3 <- reactiveValues(
    correct = 0,
    mistakes = 0,
    id = 1,
    questionNum = shuffledProbIDs_3[1]
  )
  
  ansChoices <- eventReactive(
    eventExpr = scoring$id,
    valueExpr = {
      order <- sample(x = LETTERS[1:5], size = 5, replace = FALSE)
      shuffled = c(
        bank[scoring$questionNum, order[1]],
        bank[scoring$questionNum, order[2]],
        bank[scoring$questionNum, order[3]],
        bank[scoring$questionNum, order[4]],
        bank[scoring$questionNum, order[5]]
      )
      if (scoring$id >= 1) {
        return(shuffled)
      } else {
        return("error in shuffling answer choices")
      }
    }
  )
  
  #### mark at the beginning
  output$mark <- renderUI({
    img(src = NULL, width = 30)
  })
  
  # output$Feedback <- renderUI({
  #   img(src = NULL, width = 30)
  # })
  
  ### Display a question ----
  observeEvent(
    eventExpr = input$backSce,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "restart",
        disabled = FALSE
      )
      output$question <- renderUI({
        updateRadioGroupButtons(
          session = session,
          inputId = "mc1",
          selected = character(0),
          choices = list(
            ansChoices()[1],
            ansChoices()[2],
            ansChoices()[3],
            ansChoices()[4],
            ansChoices()[5]
          ),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square") #unsure
          ),
          status = "textGame" 
        )
        if (input$backSce == 'Scenario A') {
          withMathJax(bank[scoring$questionNum, "question"])
        } else if (input$backSce == "Scenario B") {
          withMathJax(bank2[scoring_2$questionNum, "question"])
        } else if (input$backSce == "Scenario C") {
          withMathJax(bank3[scoring_3$questionNum, "question"])
        }
      })
      output$mark <- renderIcon()
      output$Plot <- renderUI({
        return(NULL)
      })
      output$PlotText <- renderUI({
        return(NULL)
      })
      scoring$id = 1
      scoring_2$id = 1
      scoring_3$id = 1
    }
  )

  
  ### Submit Button ----
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
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
      ### Get correct answer
      cAnswer <- bank[scoring$questionNum, "ansValue"] # problem what is scoring
      cAnswer_2 <- bank2[scoring_2$questionNum, "ansValue"] 
      cAnswer_3 <- bank3[scoring_3$questionNum, "ansValue"] 
      ### correct or wrong mark
      if (input$backSce == 'Scenario A') {
        output$mark <- renderIcon(
          icon = ifelse(
            test = is.null(input$mc1),
            yes = "incorrect",
            no = ifelse(
              test = input$mc1 == cAnswer,
              yes = "correct",
              no = "incorrect"
            )
          ),
          width = 50
        )
      } else if (input$backSce == "Scenario B") {
        output$mark <- renderIcon(
          icon = ifelse(
            test = is.null(input$mc1),
            yes = "incorrect",
            no = ifelse(
              test = input$mc1 == cAnswer_2,
              yes = "correct",
              no = "incorrect"
            )
          ),
          width = 50
        )
      } else if (input$backSce == "Scenario C") {
        output$mark <- renderIcon(
          icon = ifelse(
            test = is.null(input$mc1),
            yes = "incorrect",
            no = ifelse(
              test = input$mc1 == cAnswer_3,
              yes = "correct",
              no = "incorrect"
            )
          ),
          width = 50
        )
      }
      ### Scoring
      if (input$backSce == 'Scenario A') {
        if (is.null(input$mc1) || length(input$mc1) == 0 || input$mc1 != cAnswer) {
          scoring$mistakes <- scoring$mistakes + 1
        } else {
          scoring$correct <- scoring$correct + 1
        }
      } else if (input$backSce == "Scenario B") {
        if (is.null(input$mc1) || length(input$mc1) == 0 || input$mc1 != cAnswer_2) {
          scoring$mistakes <- scoring$mistakes + 1
        } else {
          scoring$correct <- scoring$correct + 1
        }
      } else if (input$backSce == "Scenario C") {
        if (is.null(input$mc1) || length(input$mc1) == 0 || input$mc1 != cAnswer_3) {
          scoring$mistakes <- scoring$mistakes + 1
        } else {
          scoring$correct <- scoring$correct + 1
        }
      }
      
      ### Game Over Check
      if (scoring$correct >= 5) {
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
      } else if (scoring$mistakes  >= 4) {
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
      

  ### Next Question Button ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      hintPressed(FALSE)
      ######## scenario A
      if (scoring$id < nrow(bank)) {
        scoring$id <- scoring$id + 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      } else {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "info",
          text = "You've used all of the questions in this scenario, 
          please go to another one to test yourself."
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(bank)),
          size = nrow(bank),
          replace = FALSE
        )
        scoring$id <- 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      }
      
      ######## scenario B
      if (scoring_2$id < nrow(bank2)) {
        scoring_2$id <- scoring_2$id + 1
        scoring_2$questionNum <- shuffledProbIDs_2[scoring_2$id]
      } else {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "info",
          text = "You've used all of the questions in this scenario,
          please go to another one to test yourself."
        )
        shuffledProbIDs_2 <- sample(
          x = seq_len(nrow(bank2)),
          size = nrow(bank2),
          replace = FALSE
        )
        scoring_2$id <- 1
        scoring_2$questionNum <- shuffledProbIDs_2[scoring_2$id]
      }
      
      ######## scenario C
      if (scoring_3$id < nrow(bank3)) {
        scoring_3$id <- scoring_3$id + 1
        scoring_3$questionNum <- shuffledProbIDs_3[scoring_3$id]
      } else {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "info",
          text = "You've used all of the questions in this scenario, 
          please go to another one to test yourself."
        )
        shuffledProbIDs_3 <- sample(
          x = seq_len(nrow(bank3)),
          size = nrow(bank3),
          replace = FALSE
        )
        scoring_3$id <- 1
        scoring_3$questionNum <- shuffledProbIDs_3[scoring_3$id]
      }
      
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      output$mark <- renderIcon()
      output$Plot <- renderUI({
        return(NULL)
      })
      output$PlotText <- renderUI({
        return(NULL)
      })
    })

  ### Reset button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
      hintPressed(FALSE)
      ### scenario A
      if (scoring$id < nrow(bank)) {
        scoring$id <- 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = FALSE
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(bank)),
          size = nrow(bank),
          replace = FALSE
        )
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      } else {
        scoring$id <- 1
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      }
      ### scenario B
      if (scoring_2$id < nrow(bank2)) {
        scoring_2$id <- 1
        scoring_2$questionNum <- shuffledProbIDs[scoring_2$id]
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = FALSE
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(bank2)),
          size = nrow(bank2),
          replace = FALSE
        )
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      } else {
        scoring_2$id <- 1
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      }
      ### scenario C
      if (scoring_3$id < nrow(bank3)) {
        scoring_3$id <- 1
        scoring_3$questionNum <- shuffledProbIDs[scoring_3$id]
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = FALSE
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(bank3)),
          size = nrow(bank3),
          replace = FALSE
        )
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      } else {
        scoring_3$id <- 1
        output$mark <- renderIcon()
        scoring$correct <- 0
        scoring$mistakes <- 0
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
        output$Plot <- renderUI({
          return(NULL)
        })
        output$PlotText <- renderUI({
          return(NULL)
        })
      }
    })
  
  hintPressed <- reactiveVal(FALSE)
  
  ### display hint ----
  observeEvent(
    eventExpr = input$hint,
    handlerExpr = {
      hintPressed(TRUE)
      if (input$backSce == 'Scenario A') {
        output$Plot <- renderPlot(
          alt = "Graph of PMF",
          {
          if ( bank[scoring$questionNum, "ansValue"] == 'Binomial') {
            ggplot() +
              stat_function(
                fun = dbinom,
                geom = 'bar',
                args = list(size = 8 , prob = 0.2),
                xlim = c(0, 8),
                na.rm = TRUE,
                n = 9,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of red balls",
                y = "Prob. of a specific number of red balls"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                plot.title = element_text(size = 20, hjust = 0.5),
                axis.title = element_text(size = 13)
              )
          } else if ( bank[scoring$questionNum, "ansValue"] == 'Negative Binomial') {
            ggplot() +
              stat_function(
                fun = dnbinom,
                geom = 'bar',
                args = list(size = 3 , prob = 0.2),
                xlim = c(0, 55),
                na.rm = TRUE,
                n = 56,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of balls drawn out before we get three red balls",
                y = "Prob. of seeing three red balls in a specific trial number"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank[scoring$questionNum, "ansValue"] == 'Geometric') {
            ggplot() +
              stat_function(
                fun = dgeom,
                geom = 'bar',
                args = list(prob = 0.2),
                xlim = c(0, 33),
                na.rm = TRUE,
                n = 34,
                fill = "skyblue"
              ) +
              theme_bw() + 
              labs(
                title = "PMF Graph",
                x = "Number of balls drawn out before we get the first red ball",
                y = "Prob. of seeing the first red ball in a specific trial number"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank[scoring$questionNum, "ansValue"] == 'Bernoulli') {
            ggplot() +
              stat_function(
                fun = dbern,
                geom = 'line',
                args = list(prob = 0.2),
                xlim = c(0, 1),
                na.rm = TRUE,
                n = 2, 
                color = "skyblue",
                size = 1.5
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of times drawn out",
                y = "Prob. of getting a red ball in one trial"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0.1), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank[scoring$questionNum, "ansValue"] == 'Hypergeometric') {
            ggplot() +
              stat_function(
                fun = dhyper,
                geom = 'bar',
                args = list(m = 6, n = 24, k = 8),
                xlim = c(0, 6),
                na.rm = TRUE,
                n = 7,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of red balls drawn",
                y = "Prob. of getting specific amount of red balls out of eight draws"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          }
        })
      } 
      if (input$backSce == "Scenario B") {
        output$Plot <- renderPlot(
          alt = "Graph of PMF",
          {
          if ( bank2[scoring_2$questionNum, "ansValue"] == 'Binomial') {
            ggplot() +
              stat_function(
                fun = dbinom,
                geom = 'bar',
                args = list(size = 7, prob = 0.1),
                xlim = c(0, 8),
                na.rm = TRUE,
                n = 9,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of cars that order a Big Mac",
                y = "Prob. of that amount of vehicles ordering a Big Mac in 7 cars"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank2[scoring_2$questionNum, "ansValue"] == 'Negative Binomial') {
            ggplot() +
              stat_function(
                fun = dnbinom,
                geom = 'bar',
                args = list(size = 3, prob = 0.1),
                xlim = c(0, 95),
                na.rm = TRUE,
                n = 96,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of vehicles",
                y = "Prob. of 3 cars ordering a Big Mac in a fixed number of vehicles"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank2[scoring_2$questionNum, "ansValue"] == 'Geometric') {
            ggplot() +
              stat_function(
                fun = dgeom,
                geom = 'bar',
                args = list(prob = 0.1),
                xlim = c(0, 45),
                na.rm = TRUE,
                n = 46,
                fill = "skyblue"
              ) +
              theme_bw() + 
              labs(
                title = "PMF Graph",
                x = "Number of vehicles",
                y = "Prob. of seeing first car order a Big Mac in fixed number of vehicles"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0.1), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank2[scoring_2$questionNum, "ansValue"] == 'Bernoulli') {
            ggplot() +
              stat_function(
                fun = dbern,
                geom = 'line',
                args = list(prob = 0.1),
                xlim = c(0, 1),
                na.rm = TRUE,
                n = 2,
                color = "skyblue",
                size = 1.5
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of vehicles",
                y = "Prob. of seeing a car order a Big Mac"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0.1), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank2[scoring_2$questionNum, "ansValue"] == 'Hypergeometric') {
            ggplot() +
              stat_function(
                fun = dhyper,
                geom = 'bar',
                args = list(m = 5, n = 2, k = 4),
                xlim = c(0, 6),
                na.rm = TRUE,
                n = 7,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of cars that order a Big Mac",
                y = "Prob. of seeing fixed number of cars order Big Mac in four vehicles"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(-0.05,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          }
        })
      } 
      if (input$backSce == "Scenario C") {
        output$Plot <- renderPlot(
          alt = "Graph of PMF",
          {
          if ( bank3[scoring_3$questionNum, "ansValue"] == 'Binomial') {
            ggplot() +
              stat_function(
                fun = dbinom,
                geom = 'bar',
                args = list(size = 10, prob = 0.4),
                xlim = c(0, 10),
                na.rm = TRUE,
                n = 11,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of type A blood donors",
                y = "Prob. of a specific number of type A blood donors in 10 donors"
              ) +
              scale_x_continuous(
                breaks = seq(from = 0, to = 10, by = 2),
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank3[scoring_3$questionNum, "ansValue"] == 'Negative Binomial') {
            ggplot() +
              stat_function(
                fun = dnbinom,
                geom = 'bar',
                args = list(size = 4, prob = 0.4),
                xlim = c(0, 25),
                na.rm = TRUE,
                n = 26,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of donors",
                y = "Prob. of 4 type A blood donors going to the station"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank3[scoring_3$questionNum, "ansValue"] == 'Geometric') {
            ggplot() +
              stat_function(
                fun = dgeom,
                geom = 'bar',
                args = list(prob = 0.4),
                xlim = c(0, 18),
                na.rm = TRUE,
                n = 19,
                fill = "skyblue"
              ) +
              theme_bw() + 
              labs(
                title = "PMF Graph",
                x = "Number of donors",
                y = "Prob. of the first type A blood donor going to the station"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank3[scoring_3$questionNum, "ansValue"] == 'Bernoulli') {
            ggplot() +
              stat_function(
                fun = dbern,
                geom = 'line',
                args = list(prob = 0.4),
                xlim = c(0, 1),
                na.rm = TRUE,
                n = 2,
                color = "skyblue",
                size = 1.5
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of donors",
                y = "Prob. of a donor having type A blood"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0.1), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          } else if ( bank3[scoring_3$questionNum, "ansValue"] == 'Hypergeometric') {
            ggplot() +
              stat_function(
                fun = dhyper,
                geom = 'bar',
                args = list(m = 5, n = 7, k = 5),
                xlim = c(0, 6),
                na.rm = TRUE,
                n = 7,
                fill = "skyblue"
              ) +
              theme_bw() +
              labs(
                title = "PMF Graph",
                x = "Number of type A blood donors",
                y = "Prob. of seeing specific number of type A blood donors in 5 donors"
              ) +
              scale_x_continuous(
                expand = expansion(mult = c(0.15,0), add = 0)
              ) + 
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0,0.05))
              ) +
              theme(
                axis.title = element_text(size = 13),
                plot.title = element_text(size = 20, hjust = 0.5)
              )
          }
        })
      }
    }
  )
  observeEvent(
    eventExpr = input$backSce,
    handlerExpr = {
      hintPressed(FALSE)
    }
  )
  
  # ARIA plot captions
  output$ariaText <- renderUI({
    if (hintPressed()) {
      if (input$backSce == 'Scenario A') {
        if (bank[scoring$questionNum, "ansValue"] == 'Binomial') {
          p(tags$strong("Graph Caption: "),
            bank[scoring$questionNum, "aria"])
        } else if (bank[scoring$questionNum, "ansValue"] == 'Negative Binomial') {
          p(tags$strong("Graph Caption: "),
            bank[scoring$questionNum, "aria"])
        } else if (bank[scoring$questionNum, "ansValue"] == 'Geometric') {
          p(tags$strong("Graph Caption: "),
            bank[scoring$questionNum, "aria"])
        } else if (bank[scoring$questionNum, "ansValue"] == 'Bernoulli') {
          p(tags$strong("Graph Caption: "),
            bank[scoring$questionNum, "aria"])
        } else if (bank[scoring$questionNum, "ansValue"] == 'Hypergeometric') {
          p(tags$strong("Graph Caption: "),
            bank[scoring$questionNum, "aria"])
        }
      } else if (input$backSce == 'Scenario B') {
          if (bank2[scoring_2$questionNum, "ansValue"] == 'Binomial') {
            p(tags$strong("Graph Caption: "),
              bank2[scoring_2$questionNum, "aria"])
          } else if (bank2[scoring_2$questionNum, "ansValue"] == 'Negative Binomial') {
            p(tags$strong("Graph Caption: "),
              bank2[scoring_2$questionNum, "aria"])
          } else if (bank2[scoring_2$questionNum, "ansValue"] == 'Geometric') {
            p(tags$strong("Graph Caption: "),
              bank2[scoring_2$questionNum, "aria"])
          } else if (bank2[scoring_2$questionNum, "ansValue"] == 'Bernoulli') {
            p(tags$strong("Graph Caption: "),
              bank2[scoring_2$questionNum, "aria"])
          } else if (bank2[scoring_2$questionNum, "ansValue"] == 'Hypergeometric') {
            p(tags$strong("Graph Caption: "),
              bank2[scoring_2$questionNum, "aria"])
          }
      } else if (input$backSce == 'Scenario C') {
          if (bank3[scoring_3$questionNum, "ansValue"] == 'Binomial') {
            p(tags$strong("Graph Caption: "),
              bank3[scoring_3$questionNum, "aria"])
          } else if (bank3[scoring_3$questionNum, "ansValue"] == 'Negative Binomial') {
            p(tags$strong("Graph Caption: "),
              bank3[scoring_3$questionNum, "aria"])
          } else if (bank3[scoring_3$questionNum, "ansValue"] == 'Geometric') {
            p(tags$strong("Graph Caption: "),
              bank3[scoring_3$questionNum, "aria"])
          } else if (bank3[scoring_3$questionNum, "ansValue"] == 'Bernoulli') {
            p(tags$strong("Graph Caption: "),
              bank3[scoring_3$questionNum, "aria"])
          } else if (bank3[scoring_3$questionNum, "ansValue"] == 'Hypergeometric') {
            p(tags$strong("Graph Caption: "),
              bank3[scoring_3$questionNum, "aria"])
          }
      }
    }
  })
  
  ### Display score ----
  output$correct <- renderUI({
    paste("Number of questions answered correctly:", scoring$correct)
  })
  
  ### Cartoon Display ----
  output$gameProgressTree <- renderUI({
    img(src = "Cell01.jpg")
    if (scoring$mistakes == 0) {
      img(
        src = "Cell01.jpg",
        width = "100%",
        alt = "The man is on the top branch"
      )
    } else if (scoring$mistakes == 1) {
      img(
        src = "Cell02.jpg",
        width = "100%",
        alt = "The man has fallen one branch"
      )
    } else if (scoring$mistakes == 2) {
      img(
        src = "Cell03.jpg",
        width = "100%",
        alt = "The man has fallen another branch, only one remaining"
      )
    } else if (scoring$mistakes == 3) {
      img(
        src = "Cell04.jpg",
        width = "100%",
        alt = "The man has fallen to the last branch"
      )
    } else if (scoring$mistakes >= 4) {
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

