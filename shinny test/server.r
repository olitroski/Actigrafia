library(shiny)

shinyServer(
     function(input, output) {

          # output$edad = renderPrint({input$edad})
          # output$talla = renderPrint({input$talla})
          # output$peso = renderPrint({input$peso})
          # output$sexo = renderPrint({input$sexo})
          # output$act = renderPrint({input$act})
          
          
          # Output de BMI insitu con la funcion
          output$bmi = renderPrint({ 
               # IMCcalc(input$peso, input$talla) 
               input$peso/(input$talla/100)^2
               })
          
          
          # Resultado del BMI
          output$fat = renderPrint({
               if (input$peso/(input$talla/100)^2 < 18.5) {
                    "You have Under Weigth"
               } else if (input$peso/(input$talla/100)^2 < 25) {
                 "You have a normal Weigth" 
                    } else if (input$peso/(input$talla/100)^2 < 30) {
                      "You have Over Weigth" 
                         } else {"You have an obsese BMI status"}
          })
          
          
          # Tasa metabolica basal
          output$BMR = renderPrint({
               if (input$sexo == 1) {
                    (10*input$peso) + (6.25*input$talla) - (5*input$edad) + 5
                    } else {(10*input$peso) + (6.25*input$talla) - (5*input$edad) - 161}    
          })
          
          
          # Tasa metabolica basal
          output$cal = renderPrint({
          tmb <- if (input$sexo == 1) {
                    (10*input$peso) + (6.25*input$talla) - (5*input$edad) + 5
               } else {(10*input$peso) + (6.25*input$talla) - (5*input$edad) - 161}
               
          if (input$act == 1) {
               tmb*1.2
          } else if (input$act == 2) {
               tmb*1.375
          } else if (input$act == 3) {
               tmb*1.55
          } else if (input$act == 4) {
               tmb*1.725
          } else { tmb*1.9 }
               
          })     

          
# Final bracket
}
)






















