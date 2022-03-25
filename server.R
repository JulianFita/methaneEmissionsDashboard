server <- function(input,
                   output,
                   session)
{
  df <- reactive({df <- methaneEmissions.longer %>%
    filter(country %in% input$country)})
  
  df2 <- reactive({df2 <- methaneEmissions.longer %>%
    filter(country %in% input$country2) %>%
    filter(sector %in% input$sector)})
  
  df3 <- reactive({df <- methaneEmissions.longer %>%
    filter(sector %in% input$sectorOverall)})
  
  # rgb(207,57,92)
  
  # me
  output$me <- renderUser({dashboardUser(name = 'Julian Gabriel Fita',
                                         image = 'https://media-exp1.licdn.com/dms/image/C4D03AQHjUxSkCl4EKQ/profile-displayphoto-shrink_800_800/0/1647629252480?e=2147483647&v=beta&t=8QwobcXjHjXNABcpNVcPp87I7ozDjAIY6q3eLho9He0',
                                         title = 'Data Analyst',
                                         subtitle = 'Author',
                                         footer = p("I'm a data analyst and I hope you enjoy this dashboard. If you want to contact me you can use my social networks or write me an email to: JulianGabrielFita@gmail.com", class = "text-center"),
                                         fluidRow(dashboardUserItem(width = 4,
                                                                    socialButton(href = 'https://github.com/JulianFita',
                                                                                 icon = icon('code-branch'))),
                                                  dashboardUserItem(width = 4,
                                                                    socialButton(href = 'https://www.linkedin.com/in/julian-gabriel-fita-924b16199/',
                                                                                 icon = icon('linkedin-in'))),
                                                  dashboardUserItem(width = 4,
                                                                    socialButton(href = 'https://www.kaggle.com/fit4kz',
                                                                                 icon = icon('kaggle')))))})
  
  #linePlot
  output$linePlot <- renderHighchart({df() %>%
      filter(country %in% input$country) %>%
      hchart('line',
             hcaes(x = year,
                   y = value,
                   group = sector)) %>%
      hc_colors(list('#150509', '#3e0f1a', '#67192b', '#90233c', '#b92d4d', '#cf395c', '#d75b78', '#e1849a', '#ebadbb')) %>%
      hc_title(text = 'What are the amounts that each sector emits?') %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text ='Value (MTCO2e)')) %>%
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, 
                 borderWidth = 2,
                 sort = TRUE,
                 table = TRUE,
                 sorted = TRUE) %>%
      hc_add_theme(hc_my_theme) %>%
      hc_exporting(enabled = TRUE)})
  
  #linePlotSector
  output$linePlotSector <- renderHighchart({df2() %>%
      filter(country %in% input$country2) %>%
      filter(sector %in% input$sector) %>%
      hchart('line',
             name = 'Methane Emissions',
             hcaes(x = year,
                   y = value)) %>%
      hc_colors('#CF395C') %>%
      hc_title(text = list(paste('What are the amounts emitted by the', unique(df2()$sector), 'sector?', sep = ' '))) %>%
      hc_xAxis(title = list(text = 'Year')) %>%
      hc_yAxis(title = list(text ='Value (MTCO2e)')) %>%
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, 
                 borderWidth = 2,
                 sort = TRUE,
                 table = TRUE,
                 sorted = TRUE) %>%
      hc_add_theme(hc_my_theme) %>%
      hc_exporting(enabled = TRUE)})
  
  # overall
  output$overall <- renderHighchart({methaneEmissions.longer %>%
      filter(sector %in% input$sectorOverall) %>%
      group_by(year) %>%
      summarise(value = sum(value)) %>%
      hchart('line',
             name = 'Methane Emissions',
             hcaes(x = year,
                   y = value)) %>%
      hc_colors('#CF395C') %>%
      hc_title(text = 'Methane Emissions') %>%
      hc_subtitle(text = 'How has the methane emission varied over the years?') %>%
      hc_add_theme(hc_my_theme)})
  
  # Methane Emission Overall
  output$methaneEmissionsoverall <- renderHighchart({methaneEmissions.longer %>%
      drop_na() %>%
      group_by(year, sector) %>%
      summarise(value = sum(value), .groups = 'drop') %>%
      hchart('line',
             hcaes(x = year,
                   y = value,
                   group = sector)) %>%
      hc_colors(list('#150509', '#3e0f1a', '#67192b', '#90233c', '#b92d4d', '#cf395c', '#d75b78', '#e1849a', '#ebadbb')) %>%
      hc_title(text = 'Sum of all Methane emissions by sectors') %>%
      hc_subtitle(text = 'Amount of emissions from each sector over the years') %>%
      hc_tooltip(crosshairs = TRUE,
                 shared = TRUE, 
                 borderWidth = 2,
                 sort = TRUE,
                 table = TRUE) %>%
      hc_add_theme(hc_my_theme)})
    
    
  
  # Methane Emissions overall Pie
  output$overallPie <- renderHighchart({methaneEmissions.longer %>%
      drop_na() %>%
      group_by(sector) %>%
      summarise(value = sum(value)) %>%
      hchart('pie',
             name = 'Methane Emissions',
             hcaes(x = sector,
                   y = value)) %>%
      hc_colors(list('#150509', '#3e0f1a', '#67192b', '#90233c', '#b92d4d', '#cf395c', '#d75b78', '#e1849a', '#ebadbb')) %>%
      hc_title(text = 'Total amounts of Methane Emissions') %>%
      hc_subtitle(text = 'Which sectors are the most emissions ?')})
  
  # Value's Box
  # Agriculture
  output$Agriculture <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Agriculture') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Agriculture', icon = icon('tractor'), color = 'maroon')})
  
  # Land-Use Change and Forestry
  output$LandUseChangeForestry <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Land-Use Change and Forestry') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Land-Use Change & Forestry', icon = icon('tree'), color = 'maroon')})
  
  # Waste
  output$Waste <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Waste') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), ' Waste', icon = icon('dumpster'), color = 'maroon')})
  
  # Energy
  output$Energy <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Energy') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Energy', icon = icon('bolt'), color = 'fuchsia')})
  
  # Other Fuel Combustion
  output$OtherFuelCombustion <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Other Fuel Combustion') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Other Fuel Combustion', icon = icon('gas-pump'), color = 'fuchsia')})
  
  # Fugitive Emissions
  output$FugitiveEmissions <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Fugitive Emissions') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Fugitive Emissions', icon = icon('fire'), color = 'fuchsia')})
  
  # Total excluding LUCF
  output$TotalexcludingLUCF <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Total excluding LUCF') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Total excluding LUCF', icon = icon('calculator'), color = 'red')})
  
  # Industrial Processes
  output$IndustrialProcesses <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Industrial Processes') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Industrial Processes', icon = icon('industry'), color = 'red')})
  
  # Total including LUCF
  output$TotalincludingLUCF <- renderValueBox({df <- methaneEmissions.longer %>%
    filter(country %in% input$country) %>%
    filter(sector %in% 'Total including LUCF') %>%
    group_by(sector) %>%
    summarise(value = max(value))
  valueBox(max(df$value), 'Total including LUCF', icon = icon('calculator'), color = 'red')})
  
  
  # Description
  # Context
  output$description <- renderUI({HTML(paste('Methane is emitted during the production and transport of coal, natural gas, and oil.
                                             Methane emissions also result from livestock and other agricultural practices, land use and by the decay of organic waste in municipal solid waste landfills.
                                             Methane is the second most important greenhouse gas. It is more potent than CO2 because the radiative forcing produced per molecule is greater.
                                             In addition, the infrared window is less saturated in the range of wavelengths of radiation absorbed by Methane, so more molecules may fill in the region.
                                             However, it exists in far lower concentrations than CO2 in the atmosphere, and its concentrations by volume in the atmosphere are generally measured in parts per billion (ppb) rather than ppm.
                                             Methane also has a considerably shorter residence time in the atmosphere than CO2 (the residence time for Methane is roughly 10 years, compared with hundreds of years for CO2)'))})
  
  # Metadata
  output$metadata <- renderUI({HTML(paste('<b>country:</b> Name of the country',
                                          '<b>sector:</b> Sector from which Methane is produced:',
                                          ' - Agriculture',
                                          ' - Energy',
                                          ' - Waste',
                                          ' - Industrial Processes',
                                          ' - Land Use Change and Forestry',
                                          ' - Fugitive Emissions',
                                          ' - Other Fuel Combustion',
                                          ' - Total Excluding LUCF = Agriculture + Energy + Waste + Industrial Processes',
                                          ' - Total Including LUCF = Total Excluding LUCF + Land Use Change and Forestry',
                                          '<b>gas:</b> CH4',
                                          '<b>unit:</b> MTCO2e - Metric tons of carbon dioxide',
                                          '<b>year:</b> Data for specific year (1990~2018)',
                                          sep="<br/>"))})
  
  # Summary
  output$summary <- renderUI({HTML(paste('In this dashboard we will see the amounts of methane emissions produced by each country represented by each sector.',
                                         'Top 3 countries that emitted the most over the years are:',
                                         '- China: 94579.41 MTCO2e',
                                         '- Russia: 82673.46 MTCO2e',
                                         '- United States: 68102.69 MTCO2e',
                                         'The country that has emitted the most methane in 2018 was:',
                                         '- China: 4456.11 MTCO2e',
                                         sep = "<br/>"))})
  
  # Tables
  # Table
  output$table <- renderDataTable({methaneEmissions.longer %>%
      select(year, country, sector, value) %>%
      arrange(desc(value))})
                                          

}

shinyApp(ui, server)
