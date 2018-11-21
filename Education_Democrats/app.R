library(shiny)
library(tidyverse)
library(knitr)
library(fs)
library(graphics)


download.file(url = "https://goo.gl/ZRCBda",
              destfile = "Polling.zip",
              quiet = TRUE,
              mode = "wb")

polling <- unzip("Polling.zip")

#dir means directory and ls means list. .id will say its source
upshot <- fs::dir_ls("2018-live-poll-results-master/data") %>%
  map_dfr(read_csv, .id = "source") %>%
  filter(str_detect(source, "sen")) %>%
  #making state variable which takes 51 and 52 character
  mutate(state = toupper(str_sub(source, 51, 52))) %>%
  #making district variable which takes 53 and 54 character. will be letters for non house
  mutate(district = str_sub(source, 53, 54)) %>%
  #making office variables using districts that have letters
  mutate(office = case_when(district == "se" ~ "Senate",
                            district == "go" ~ "Other",
                            TRUE ~ "House")) %>%
  #making wave variable using the 5th last character
  mutate(wave = str_sub(source, -5,-5))

upshot2 <- upshot %>%
  select(response, educ, final_weight, state) %>%
  mutate(response = recode(response, 
                           "3" = "NoParty",
                           "4" = "NoParty",
                           "5" = "NoParty",
                           "6" = "NoParty")) %>%
  
  mutate(educ = recode(educ, 
                       "High school" = "HS Grad or Less",
                       "Grade school" = "HS Grad or Less",
                       "Some college or trade school" = "Some College/Assoc Degree",
                       "Bachelors\' degree" = "College Grad",
                       "Graduate or Professional Degree" = "College Grad")) %>% 
  group_by(state, educ, response) %>%
  summarise(final_weight1 = sum(final_weight)) %>%
  filter(educ != "[DO NOT READ] Refused") %>%
  spread(key = response, value = final_weight1) 

upshot2[is.na(upshot2)] <- 0

upshot2 <- upshot2 %>%
  mutate(tfinal_weight = NoParty + Dem + Rep + Und )%>%
  mutate(rep_adv = (Rep - Dem) / tfinal_weight) 



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Democrat Response by Education (Senate House)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "state",
                     label = "State",
                    choices = upshot2$state #,
                    #selected = upshot2$state
                    )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("pieplot")),
                    tabPanel("Summary", verbatimTextOutput("summary")) 
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pieplot <- renderPlot({
      # generate bins based on input$bins from ui.R
     upshot4 <- upshot2 %>%
     filter(state == input$state)
     pie(upshot4$Dem, labels = upshot4$educ, main="Pie Chart of Democratic Response")
     
    
   })
   
   output$summary <- renderPrint({
     "The data uses Senate polls from the Upshot Github data. The pie chart will compare democratic responses by education among states."
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

