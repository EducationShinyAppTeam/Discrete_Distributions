# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(stats)
library(statip)
library(dplyr)
library(ggimage)

# Load additional dependencies and setup functions
# source("global.R")

bank <- read.csv("questionbank.csv")
bank$Feedback <- as.character(bank$Feedback)


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
          p("This app would focus on Bernoulli, Binomial, Hypergeometric, 
            Geometric and Negative Binomial distribution.
            This app would explore different sample spaces and sample paths. 
            Also, it would help users to choose suitable distribution in actual
            life scenario and show the Probability Mass Function graphs about 
            that distribution."),
          h2("Instructions"),
          tags$ol(
            tags$li("Basic information about these distributions are introduced 
                    in the Prerequisites Page."),
            tags$li("In the Exploration Page, users can explore different sample 
                    spaces and sample paths by changing the number of Success
                    or Trials and the probability of success."),
            tags$li("In the Game Page, an actual life scenario is given. Users 
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
            2022. Special Thanks to Neil for being incredibly 
            helpful with programming issues.",
            br(),
            div(class = "updated", "Last Update: 7/26/2022 by YY.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Distributions"),
          br(),
          box(
            title = "Bernoulli Distribution",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "It is a discrete probability 
              distribution which has only two outcomes -- Success or Failure(which
              always be coded Success 1 and Failure 0). 
              p is the probability of Success. X represents for the probability of 
              getting the success in one trial.",
              br(),
              tags$strong("Notation: "), "X ~ Bern(p)"
            )
          ),
          box(
            title = "Binomial Distribution",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "It is a discrete probability 
              distribution with success probability p and n independent 
              experiments. X represents for the number of successes.",
              br(),
              tags$strong("Notation: "), "X ~ Bin(n, p)",
              br(),
              tags$strong("Special Case: "), "Bern(p) is equil to Bin(n = 1, p)",
              br(),
              tags$strong("E(x): "), "np"
            )
          ),
          box(
            title = "Hypergeometric Distribution",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "It is a discrete probability 
              distribution which describes the probability of X successes in 
              n draws which are", strong("not independent"), ", without replacement,
              which means ", strong("probability of success changes"), ", from 
              a population size of N that contains m of objects with success.
              X represents the number of successes.",
              br(),
              tags$strong("Notation: "), "X ~ HG(n, m, N)",
              br(),
              tags$strong("E(x): "), "nm/N"
            )
          ),
          box(
            title = "Geometric Distribution",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "It is a discrete probability 
              distribution which describes the probability that the occurrence of", 
              strong("first success"), "after X independent trials, each with
              success probability p. X represents for the number of trials.",
              br(),
              tags$strong("Common Notation: "), "X ~ Geom(p)",
              br(),
              tags$strong("E(x): "), "1/p"
            )
          ),
          box(
            title = "Negative Binomial Distribution",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(
              tags$strong("Description: "), "It is a discrete probability 
              distribution which describes the probability that the occurrence of", 
              strong("the rth success"), "after X independent trials, each with
              success probability p. X represents for the number of trials.",
              br(),
              tags$strong("Common Notation: "), "X ~ NBin(r, p)",
              br(),
              tags$strong("Special Case: "), "Geom(p) is equal to NBin(r = 1, p)",
              br(),
              tags$strong("E(x): "), "r/p"
            )
          ),
          box(
            title = "Summary",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(
              "In", tags$strong("binomial"), "and", tags$strong("hypergeometric"),
              ": the number of trials is ", strong("fixed and known"), 
              ", and the number of successes is ",
              strong("random variable"), "(of interest).",
              br(),
              "In", tags$strong("geometric"), "and", 
              tags$strong("negative binomial"), ": the number of trials is ", 
              strong("random variable"), "and the number of successes is", 
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
          tabsetPanel(
            type = "tabs",
            tabPanel( 
              title = "Random: Trials",
              h3("Instructions"),
              p("For this part, the Trial Number is what we are interested in 
                and Success Number is fixed."),
              p("So, ", strong("Geometric"), "distribution and ", strong("Negative
                 Binomial"), "distribution are suitable for this page."),
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
                    label = "Number of Sample Path",
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
                        "Black dots represents Sample Space."),
                  p(img(src = "steplines.jpg", width = "30px"),
                    "Each line represents one Sample Path which is 
                    influenced by the Success Number and Probability of Success"),
                  p(img(src = "rhombus.webp", width = "20px"),
                    " The blue diamond represents expected value")
                )
              )
            ),
            tabPanel( 
              title = "Random: Successes",
              h3("Instructions"),
              p("For this part, the Success number is what we are interested in
                and the Trial Number is fixed."),
              p("So, ", strong("Bernoulli"), "distribution and ", strong(
                "Binomial"), "distribution are suitable for this page."),
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
                    label = "Number of Sample Path",
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
                    "Black dots represents Sample Space."),
                  p(img(src = "steplines.jpg", width = "30px"),
                    "Each line represents one Sample Path which is 
                    influenced by the Success Number and Probability of Success"),
                  p(img(src = "rhombus.webp", width = "20px"),
                    " The blue diamond represents expected value")
                )
              )
            )
          )
        ),
        #### Set up a Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Choosing distribution game"),
          p(strong("Instructions: "), "For each 
              question, choose the most suitable distribution for that situation.
              The Probability Mass Function graph shown up in the right 
              would help you choose the correct one."),
          br(),
          p(strong("Background Scenario: ")),
          wellPanel(
            style = "background-color: #FFFFFF",
            p("There are 30 balls in a box. 6 of them are red which means that
              the probability of getting a red ball from the box is 0.2. Here
              are some questions based on several supposed situations.")
          ),
          p(strong("Questions: ")),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                uiOutput("question"),
                br(),
                radioGroupButtons(
                  inputId = "mc1",
                  label = "Which distribution is suitable for the question?",
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
                uiOutput("Feedback"),
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
                label = "Show PMF graph",
                size = "default"
            ),
            plotOutput("Plot", width = "400")
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
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
        text = "In Explore Page, change number of Success/Trials and probability
        to see the changes in the sample space and sample path. In Game Page, choose 
        the suitable distribution for each question with a PMF graph hint."
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
      trials <- rnbinom(3, size = input$numSs, prob = input$probSuccess)+ input$numSs 
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
      
      output$trialsPlot <- renderPlot(
        expr = {
          
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
          
          maxTrials <- ifelse(
            test = max(trials) > expected,
            yes = max(trials)+1,
            no = ceiling(expected) + 1
          )

          successVector <- c()
          for (i in 1:maxTrials){
            successVector <- c(successVector, 0:i)
          }

          points <- data.frame(
            success = successVector,
            Trials = rep(x = 1:maxTrials, times = 2:(maxTrials+1))
          )
          
      
          a <- ggplot(
            data = points
            ) +
            geom_point(
              mapping = aes(x = success, y = Trials), 
              size = 2,
              na.rm = TRUE
            ) +
            geom_point(
              mapping = aes(x = input$numSs, y = expected),
              shape = 23,
              fill = "blue",
              color = "blue",
              size = 3
            ) +
            geom_step(
              data = Path1,
              aes(x = cSuccess, y = trial),
              color = psuPalette[1],
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
              breaks = c(1, seq(from = 5,to = maxTrials, by = 5)),
              minor_breaks = seq(1, maxTrials, 1),
              expand = expansion(mult = c(0,0.05), add = c(1, 0))
            ) +
            xlab(label = "Success Number") +
            ylab(label = "Trial Number") +
            theme_bw() +
            theme(
              text = element_text(size = 12)
            )
          
          if (input$samPath1 == 2){
            a <- a + geom_step(
              data = Path2,
              aes(x = cSuccess, y = trial),
              color = psuPalette[2],
              size = 1,
              position = position_nudge( x= 0.02, y = 0.02)
            )
          }
          
          if (input$samPath1 == 3){
            a <- a + geom_step(
              data = Path2,
              aes(x = cSuccess, y = trial),
              color = psuPalette[2],
              size = 1,
              position = position_nudge( x= 0.02, y = 0.02) 
            ) + 
              geom_step(
                data = Path3,
                aes(x = cSuccess, y = trial),
                color = psuPalette[3],
                size = 1,
                position = position_nudge( x= -0.02, y = -0.02) 
              )
          }
          a
        }
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
            geom_point(
              mapping = aes( x = input$numTs, y = input$numTs*input$probSucc),
              shape = 23,
              fill = "blue",
              color = "blue",
              size = 3
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
                               position = position_nudge(
                                 x = if_else( 
                                   i == 1, 0, if_else(i==2, 0.02, -0.02)
                                   ),
                                 y = if_else( 
                                   i == 1, 0, if_else(i==2, 0.02, -0.02))
                                 ),
                               size = 1)
          }
          b
        }
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
  
  scoring <- reactiveValues(
    correct = 0,
    mistakes = 0,
    id = 1,
    questionNum = shuffledProbIDs[1]
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
  
  output$Feedback <- renderUI({
    img(src = NULL, width = 30)
  })
  
  ### Display a question ----
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
        yes = icon("check-square")
      ),
      status = "game"
    )
    withMathJax(bank[scoring$questionNum, "question"])
  })
  
  # ### hint Button ----
  # ObserveEvent(
  #   eventExpr = input$hint,
  #   handlerExpr = {
  #     
  #   }
  # )
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
      ### correct or wrong mark
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
      ### Scoring
      if (is.null(input$mc1) || length(input$mc1) == 0 || input$mc1 != cAnswer) {
        scoring$mistakes <- scoring$mistakes + 1
      } else {
        scoring$correct <- scoring$correct + 1
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
      #### feedback ----
      output$Feedback <- renderUI({
        if ( any(input$mc1 == cAnswer )) {
          HTML(paste("Congrats !", bank[scoring$questionNum, "Feedback"], 
                     collapse = "\n"))
        } else {
          HTML(paste("Don't give up, try again !", 
                     bank[scoring$questionNum, "Feedback"], collapse = "\n"))
        }
      })
    })
      

  ### Next Question Button ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      if (scoring$id < nrow(bank)) {
        scoring$id <- scoring$id + 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      } else {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          type = "error",
          text = "You've used all of the questions; shuffling the question bank."
        )
        shuffledProbIDs <- sample(
          x = seq_len(nrow(bank)),
          size = nrow(bank),
          replace = FALSE
        )
        scoring$id <- 1
        scoring$questionNum <- shuffledProbIDs[scoring$id]
      }
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
      output$mark <- renderIcon()
      output$Feedback <- renderUI({
        img(src = NULL, width = 30)
      })
      output$Plot <- renderUI({
        return(NULL)
      })
    })

  ### Reset button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
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
      # scoring$correct <- 0
      scoring$mistakes <- 0
      output$Feedback <- renderUI({
        img(src = NULL, width = 30)
      })
    })
  
  ### display hint ----
  observeEvent(
    eventExpr = input$hint,
    handlerExpr = {
      output$Plot <- renderPlot({
          if ( bank[scoring$questionNum, "ansValue"] == 'Binomial') {
            plot(0:5, dbinom(0:5, size = 8, prob = 0.2), type = "h", ylab = "")
          }
          if ( bank[scoring$questionNum, "ansValue"] == 'Negative Binomial') {
            plot(dnbinom(0:30, size = 3, prob = 0.2), type = 'h', ylab ="")
          }
          if ( bank[scoring$questionNum, "ansValue"] == 'Geometric') {
            plot(dgeom(0:30, 0.2), type = 'h', ylab = "")
          }
          if ( bank[scoring$questionNum, "ansValue"] == 'Bernoulli') {
            plot(dbern(0:30, prob = 0.2), type = "o", ylab = "")
          }
          if ( bank[scoring$questionNum, "ansValue"] == 'Hypergeometric') {
            plot(dhyper(0:5, 5, 25, 8), type = "h", ylab = "")
          }
      }
      )
    }
  )
  
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
    } else if (scoring$mistakes == 4) {
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

