library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bslib)

#Building the ui for the app
ui <- navbarPage(
  title = "The Role of Prediction Error in Reconsolidation",
  theme = bs_theme(
    bootswatch = "cosmo",  # Choose a predefined theme like "flatly", "cosmo", "cerulean", etc.
    primary = "#FFD1DC",    # Customize primary color
    secondary = "#FFD1DC",  # Customize secondary color
    base_font = font_google("Roboto"),  # Use Google Fonts
    code_font = font_google("Courier Prime")
  ),
  
  #Page to tell about the data
  tabPanel(
    title = tagList(icon("database"), "Data Source"),
    sidebarLayout(
      sidebarPanel(
        h4("About the Data"),
        p("The data used in this research was obtained from the Open Science Framework (OSF) repository, 
           the dataset is titled 'The Role of Prediction Error in the Reconsolidation of Episodic Memories'. 
           Contributed by Diane H Moon, Holley Bowen, and Sara D. Davis. The dataset is available at https://osf.io/4wrf9/.")
      ),
      mainPanel(
        h3("Data Description"),
        p("This dataset contains two studies used to better understand the reconsolidation of episodic memories."),
        h4("Study 1"),
        p("Study 1 recorded how much prediction error certain cues elicited. Participants were recruited via 
           Amazon Mechanical Turk (MTurk) using CloudResearch and Prolific."),
        h4("Study 2"),
        p("Study 2 used the cues normed from Study 1 to test memory recall of participants in a 
           three-day structured experimental paradigm. The particpants consisted of undergraduates and others
          collected around the Dallas area.")
      )
    )
  ),
  
  #Explaining the methods
  tabPanel(
    title = tagList(icon("flask"),"Methods"),
    sidebarLayout(
      sidebarPanel(
        h4("Overview of Methods"),
        p("The purposes of study 1 and study 2 align and compliment each other. The information found and normed from study 1
          was used to inform the expirements in study 2. Both studies looked at many factors, such as age, gender, employment status, weather
          on days of expirements etc. However, the main pieces of data we will utilize in this presentation are outlined to the right!")
      ),
      mainPanel(
        h3("Study Methods"),
        h4("Study 1"),
        p("In this study, participants were recruited via Amazon Mechanical Turk (MTurk) using CloudResearch and Prolific platforms."), 
        p("Participants were presented with a variety of reactivation cues, in the form of videos, designed to trigger episodic 
        memory responses. Participants were tasked with providing self-reported measures on three key dimensions: 
         arousal (emotional activation caused by the cue), valence (whether the cue evoked positive or negative emotions), 
         and familiarity (how recognizable the cue was)."),
        p("Measures were collected through an online survey format, 
         where participants recorded their responses after viewing or interacting with the cues."),
        p("Surprise, as inferred from these measures, was used as a proxy for prediction error—the mismatch between participants' 
         expectations and the actual content of the cues. This approach allowed researchers to quantify and standardize 
         reactivation cues based on their prediction error levels."),
        h4("Study 2"),
        p("Study 2 built on the results from Study 1 by applying the standardized reactivation cues in a memory reconsolidation experiment."), 
        p("Participants, recruited from undergraduate populations and the local community in Dallas, engaged in a structured, three-day 
           experimental paradigm. On each day, participants were presented with reactivation cues to trigger memory recall. 
           Their task was to attempt reconsolidation by recalling and updating episodic memories."),
        p("During the experiment, the researchers 
           recorded participants' responses, focusing on many key measures such as confidence (how certain they were in their answers) 
           and correctness (accuracy of recall). Researchers used these and other factors to quantify the mean error of each of the cues used.
           This design allowed the researchers to test how varying levels of prediction error, 
           identified through surprise in Study 1, influenced memory reconsolidation processes over multiple sessions.")
      )
    )
  ),
  
  #Research questions + hypothesis + plan
  tabPanel(
    title = tagList(icon("question-circle"),"Research Questions"),
    sidebarLayout(
      sidebarPanel(
        h4("Research Focus and Motivations"),
        p("Although this data provides many insights into the factors that effect memory recall and reconsolidation, this presentation focuses on
          the affect of surprise the cues illicted and its effect on the mean error of the cues. Furthermore arousal, familiarity, and valence are 
          all also utilized in answering research questions. Although there were many more factors in this data, these are the most suited to our
          quesitons."),
        p("The Research questions I chose for this project were motivated by a curiosity in memory. I personally visualize in my head for most mental operations, so I was curious of the effects a medium like videos could have. In general I wanted to know how the surprise caused by videos may increase or decrease how well we can remember things. Also, as this source did not explain much, I wanted to know more about how the 'surprise' feature was effected by the other responses videos illicited.")
      ),
      mainPanel(
        h3("Big Question"),
        p("How does the surprise rating of cues affect the reactivation and reconsolidation of memories?"),
        h4("Hypothesis"),
        p("Higher surprise in reactivation cues will result in more error when recalling memories."),
        h4("Plan"),
        tags$ul(
          tags$li("The plan for for adressing this question is to use data from both study 1 and study 2."),
          tags$li("Study 2 has columns
          labeled MEQ1-19. These represent the mean error of each of the video cues in the second study's expirement.
          Once these have been separated and averaged, we move to study 1."),
          tags$li("For each of the cues, 1-19, in study 1 the
          average surprise rating at the end of the study will be calculated. Finally the average surprise ratings of each
          of these video cues will be ploted against the mean error of the same cues in study 2 to discover if there
          is a trend as surprise ratings go up."),
          tags$li("If the hypothesis is correct, as surprise levels rise, so will the mean
          error of the cues."),
        ),
        hr(),
        h3("Fork Question"),
        p("How do self-reports (arousal, valence, familiarity) affect surprise?"),
        h4("Hypothesis"),
        p("Arousal ratings are hypothesized to correlate closely with surprise ratings (e.g., higher arousal leads to higher surprise while lower arousal leads to lower surprise)."),
        h4("Plan"),
        tags$ul(
          tags$li("The plan for adressing this fork question is to plot the three main categories of self reporting: arousal, familiarity, and valence."),
          tags$li("The frequencey
          at which they were each rated at ceratin surprise levels will be examined by the shape of these plots. The aim of this
          is to see how much and in what way certain categories effect the surprise rating of a cue."),
          tags$li("For example,
          if the hypothesis is true the higher arousal ratings will be more frequently associated with high surprise.")
        ),
      )
    )
  ),
  
  # Big Question Plot Page
  tabPanel(
    title = tagList(icon("chart-line"),"Big Question Plot"),
    sidebarLayout(
      sidebarPanel(
        h4("Plot Info"),
        p("This plot depicts the surprise normed from study 1 against mean error found in study 2 for each of the video cues.
          The surprise ratings were originally rated from 1-5 (discrete). The mean error was also between 1-5 discrete,
          meaning no normalizing was needed."),
        h5("Below is a table more clearly showing the surprise and mean error values of each of the video cues."),
        h4("Video Data Table"),
        tableOutput("video_table")
      ),
      
      mainPanel(
        h4("Big Question Plot"),
        plotOutput("big_question_plot") ,
        h4("Interpretation"),
        p("This plot shows a trend upwards. As the average surprise increases, the mean error also increases. 
          However, how much the mean error increases is sporatic. For example Video 4 and 16 are .01 surprise
          apart but vary .38 in error. While video 1 and 14 are .03 apart and vary by .12 in mean error. Furthermore, although the relationship is distinct, it is not very strong. The line has a small slope and
          many points are distant from the line of best fit.") 
      )
    )
  ),
  
  # Fork Plot Page
  tabPanel(
    title = tagList(icon("line-chart"),"Fork Plot"),
    sidebarLayout(
      sidebarPanel(
        h4("Plot Controls"),
        selectInput(
          inputId = "selected_variable",
          label = "Select a variable to plot against Surprise:",
          choices = c("Arousal Level" = "Arousal_1", 
                      "Familiarity Level" = "Familiarity_1", 
                      "Valence Level" = "Valence_1"),
          selected = "Arousal_1"
        )
      ),
      
      mainPanel(
        h4("Interactive Plot"),
        plotOutput("interactive_plot"),
        h4("Interpretation"),
        textOutput("plot_interpretation") 
      )
    )
  ),
  
  #Final interpretations and findings
  tabPanel(
    title = tagList(icon("check-circle"),"Final Interpretations"),
    sidebarLayout(
      sidebarPanel(
        h4("Research Interpretations"),
        p("This section discusses the final interpretations on the research questions, as well as the implications
          and potential for future research.")
      ),
      mainPanel(
        h3("Big Question Interpretation"),
        p("The plot of the data showed an increase in mean error for the cues as the surprise rating of the cues
          increased. However the slope of the line of best fit was low, indicating it may not be as strong an effect as I had presumed. This answers the question that there is a correlation between the surprise level of reactivation cues and the accuracy
          of memory recall. However it was not as clear was anticipated, and could benefit from more research."),
        h3("Fork Interpretations"),
        p("All of the self-report categories tended to be categroized most frequently as higher surprise, however
          they all had a mean very close to 3. Fields like arousal and 
          familarity had a more intense effect on surprise than valence level. However, the observation that the different report measures mostly had similar averages indicates they did not vary as much as anticipated. Finally, the hypothesis that the arousal would correlate one to one, ie arousal 1 = surprise of 1, was proven incorrect. The arousal had means very close to 3 with distributions more even than expected."),
        hr(),
        h3("Future Research"),
        p("The lack of alignment with some of my hypothesis leads me to ideas for future research. I believe one good idea for future research would be to do research over a longer period of time than the 3 days of this expirement. Furthermore, my analysis of the self-report measures for my fork question leads me to believe research with more affect measures as well as a more uniform distribution of surprise would make for more reliable data.")
      ),
    )
  ),
  
  tabPanel(
    "References",
    mainPanel(
      tags$div(
        tags$h3("References"),
        tags$ul(
          tags$li(
            HTML("<strong>shiny</strong>: RStudio and Inc. (2021). <em>shiny: Web Application Framework for R.</em> R package version 1.7.1. <a href='https://shiny.rstudio.com' target='_blank'>https://shiny.rstudio.com</a>")
          ),
          tags$li(
            HTML("<strong>ggplot2</strong>: Hadley Wickham (2016). <em>ggplot2: Elegant Graphics for Data Analysis.</em> Springer-Verlag New York. ISBN 978-3-319-24277-4. <a href='https://ggplot2.tidyverse.org' target='_blank'>https://ggplot2.tidyverse.org</a>")
          ),
          tags$li(
            HTML("<strong>dplyr</strong>: Hadley Wickham, Romain François, Lionel Henry, and Kirill Müller (2021). <em>dplyr: A Grammar of Data Manipulation.</em> R package version 1.0.7. <a href='https://dplyr.tidyverse.org' target='_blank'>https://dplyr.tidyverse.org</a>")
          ),
          tags$li(
            HTML("<strong>tidyr</strong>: Hadley Wickham (2021). <em>tidyr: Tidy Messy Data.</em> R package version 1.1.4. <a href='https://tidyr.tidyverse.org' target='_blank'>https://tidyr.tidyverse.org</a>")
          ),
          tags$li(
            HTML("<strong>stringr</strong>: Hadley Wickham (2019). <em>stringr: Simple, Consistent Wrappers for Common String Operations.</em> R package version 1.4.0. <a href='https://stringr.tidyverse.org' target='_blank'>https://stringr.tidyverse.org</a>")
          ),
          tags$li(
            HTML("<strong>OSF Source</strong>: Moon, D. H., Bowen, H., & Davis, S. D. (2021). <em>The role of prediction error in the reconsolidation of episodic memories.</em> Open Science Framework. Retrieved from <a href='https://osf.io/4wrf9/' target='_blank'>https://osf.io/4wrf9/</a>")
          )
        )
      )
    )
    
    
  )
  
  
  
  
)
#Displaying all plots and graphics
server <- function(input, output) {
  
  # Create the interactive fork plot
  output$interactive_plot <- renderPlot({
    selected_var <- input$selected_variable 
    
    # Calculating mean for the interactive plot
    summary_stats <- study_1 %>%
      group_by_at(selected_var) %>%
      summarise(
        mean_surprise = mean(Surprise_1, na.rm = TRUE)
      )
    
    # Create the violin plot with means and connecting line
    study_1 %>%
      ggplot(aes_string(x = sprintf("factor(%s)", selected_var), y = "Surprise_1", fill = sprintf("factor(%s)", selected_var))) +
      geom_violin(alpha = .7) +  # Setting alpha to .8 for aesthetics
      # Plotting the means of each othe rating levels
      geom_point(data = summary_stats, aes_string(x = sprintf("factor(%s)", selected_var), y = "mean_surprise"), 
                 color = "black", size = 3, shape = 18) +
      # Connecting means with line so its earier to see whats the trend
      geom_line(data = summary_stats, aes_string(x = sprintf("factor(%s)", selected_var), y = "mean_surprise", group = 1), 
                color = "black", size = 1) +
      labs(
        title = paste("Surprise Ratings by", gsub("_1", "", selected_var)),
        x = gsub("_1", " Level", selected_var),
        y = "Surprise Rating",
        fill = gsub("_1", " Level", selected_var)
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold",, size=16),         # Bold title
        axis.title.x = element_text(face = "bold", size=15),      # Bold x-axis label
        axis.title.y = element_text(face = "bold", size=15),      # Bold y-axis label
        legend.title = element_text(face = "bold", size=13),      # Bold legend title
        plot.caption = element_text(face = "italic", size = 12)     # Italic caption (optional)
      )
  })
  
  combined2 <- reactive({
    # Averaging the surprise for the cues from study 1
    Surprises <- study_1 %>%
      group_by(Video) %>%
      summarise(Surp = mean(Surprise_1, na.rm = TRUE))
    
    # Compute MEQ averages from the expirements in study 2
    cueAverages <- study_2 %>%
      summarise(across(matches("^MEQ[1-9]$|^MEQ1[0-9]$"), ~mean(.x, na.rm = TRUE), .names = "{.col}_avg")) %>%
      pivot_longer(cols = matches("_avg$"), names_to = "Video", values_to = "MEQ_Avg") %>%
      mutate(Video = as.double(str_extract(Video, "\\d+")))
    
    # Join Surprise and MEQ averages to plot them against each oter later
    combined2 <- cueAverages %>%
      inner_join(Surprises, by = "Video")
  })
  
  # Render the Big Question plot
  output$big_question_plot <- renderPlot({
    
    # Plot MEQ_Avg vs Surprise averages
    combined2() %>%
      ggplot(aes(x = Surp, y = MEQ_Avg, color = as.factor(Video))) +
      geom_point(size = 5) +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
      labs(
        title = "Relationship Between Surprise and Cue Error Averages",
        x = "Average Surprise",
        y = "Cue Error",
        color = "Video Cue",
        caption = "Moon, D. H., Bowen, H., & Davis, S. D. (2021).\n The role of prediction error in the reconsolidation of episodic memories. Resource created on September 16, 2021. Open Science Framework. Retrieved from https://osf.io/4wrf9/."
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold",, size=16),         # Bold title
        axis.title.x = element_text(face = "bold", size=15),      # Bold x-axis label
        axis.title.y = element_text(face = "bold", size=15),      # Bold y-axis label
        legend.title = element_text(face = "bold", size=13),      # Bold legend title
        plot.caption = element_text(face = "italic", size = 12)     # Italic caption (optional)
      )
  })
  
  # Create table of surprise and error values
  output$video_table <- renderTable({
    combined2() %>%
      select(Video, Surp, MEQ_Avg) %>%
      rename(
        `Video Cue` = Video,
        `Average Surprise` = Surp,
        `Mean Error (MEQ)` = MEQ_Avg
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Render the interpretation dynamically based on the selected variable
  output$plot_interpretation <- renderText({
    selected_var <- input$selected_variable
    interpretation <- switch(
      selected_var,
      "Arousal_1" = "This plot shows the distribution of arousal reports to the surprise rating they were categorized as. The plot being wider
      at any point indicates more frequency. The plots for the arousal levels of 2-4 were all most frequently categorized as 4.
      This indicates that arousals of 2-4 tend to be categorized as more surprising. Furthermore even arousal of 1 was most frequently categorized as 3, 
      indicating that arousal has an intense effect on surprise categorization.",
      "Familiarity_1" = "The violin plots being thick on average indicate that the familiarity reports
      were more evenly distributed among surprise categorizations. However, the lowest categorization was a familiarity
      of 3 being labeled as surprise of 3, while all other familiarities were most frequently categorized higher, showing
      that familiarity also lends to higher surprise.",
      "Valence_1" = "Valence had a closer correlation to the lower ratings of surprise. A valence of 1 was frequently a surprise of 1. The line between the averages for the values between 1-5 show a more clear increase than other report measures. Many report measures had means very close to each other, while valence had lower means for valence of 1 and 2."
    )
    interpretation
  })
}


shinyApp(ui = ui, server = server)
