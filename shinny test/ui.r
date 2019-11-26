library(shiny)

shinyUI(pageWithSidebar(
     
# Titulo de la cabecera     
headerPanel(h2("Body Mass Index and Daily Calories Requirement")),
     
# Sidebar Entrar los datos
sidebarPanel(
     h3("About this app"),
     p("This aplication will calculate your BMI and Daily Calories requirement based in 
        Harris-Benedict equation, specifically the revision made by Mifflin & St. Jeor (1990)"),
     h3("Input your measeures"),
     
     # Ingreso de los datos
     numericInput("edad", "Age (years)", 27, min=0, max=99, step=1),
     numericInput("talla", "Height (centimeters)", 160, min=0, max=230, step=1),
     numericInput("peso", "Weight (kilograms)", 50, min=0, max=200, step=1),
     radioButtons("sexo", "Gender", c("Male"=1, "Female"=2)),
     radioButtons("act", "Physical activity", 
                        c("Little to None excercise" = 1, 
                          "Ligth excercise, 1-3 days per week "= 2, 
                          "Moderate excercise, 3-5 days per week" = 3,
                          "Heavy exercise, 6-7 days per week" = 4,
                          "Very heavy exercise (twice per day, extra heavy workouts)" = 5)),
     
     submitButton("Submit")
     ),


# Main panel Aca van los resultados
mainPanel(
     h3("Your results"),
     # p("Age"), verbatimTextOutput("edad"),
     # p("Heigth"), verbatimTextOutput("talla"),
     # p("Weigth"), verbatimTextOutput("peso"),
     # p("Gender"), verbatimTextOutput("sexo"),
     # p("Activity"), verbatimTextOutput("act"),
     
     h4("Your BMI is"),
     verbatimTextOutput("bmi"),
     
     p("This means"),
     verbatimTextOutput("fat"), br(),
     
     h4("Your Basal Metabolic Rate (BMR)"),
     p("This rate is your base requirement if you do absolute non exercise"),
     verbatimTextOutput("BMR"), br(),

     h4("Your adjusted metabolic requirement"),
     p("This rate adjust your calories requirement up to your physical activity"),
     verbatimTextOutput("cal"),
     
     br(), br(),br(), br(), br(),
     
     strong("References"), br(),
     a("http://www.ncbi.nlm.nih.gov/pubmed/2305711"),br(),
     a("https://en.wikipedia.org/wiki/Harris-Benedict_equation")
     
     
     )

))