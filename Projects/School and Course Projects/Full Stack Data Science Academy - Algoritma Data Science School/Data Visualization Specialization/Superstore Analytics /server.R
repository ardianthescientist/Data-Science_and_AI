function(input, output, session) {
  #----overview----
  output$overview_total_sales <- renderUI({
    selected_year <- input$overview_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    sales_total <- sum(filtered_data$Sales)
    
    profit_total <- sum(filtered_data$Profit)
    infoBox(
      width = 3,
      "TOTAL Sales:",
      value = paste("$", format(as.integer(sales_total), big.mark = ",")),
      color = "navy",
      icon = icon("credit-card")
    )
  })
  output$overview_total_profit <- renderUI({
    selected_year <- input$overview_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    profit_total <- sum(filtered_data$Profit)
    
    profit_total <- sum(filtered_data$Profit)
    infoBox(
      width = 3,
      "TOTAL Profit:",
      value = paste("$", format(as.integer(profit_total), big.mark = ",")),
      color = "navy",
      icon = icon("sack-dollar")
    )
  })
  output$overview_total_customer <- renderUI({
    selected_year <- input$overview_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    n_customer <- length(unique(filtered_data$Customer.ID))
    infoBox(
      width = 3,
      "TOTAL Customers:",
      value = n_customer,
      color = "navy",
      icon = icon("users")
    )
  })
  output$overview_total_order <- renderUI({
    selected_year <- input$overview_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    infoBox(
      width = 3,
      "TOTAL Orders:",
      value = nrow(filtered_data),
      color = "navy",
      icon = icon("cart-shopping")
    )
  })
  
  output$overview_order_monthly_trend_by_year <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Order.Month) %>%
      summarise(total_order = n()) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Month.Name,
                             "\n", format(total_order, big.mark = ","), " orders",
                             sep = ""))
    
    max_order <- max(plot_data$total_order)
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_order = median(total_order))
    
    plot_order_monthly_trend_by_year <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_order, text = tooltip)) +
      geom_line(aes(y = avg_order, text = paste("Median:\n", format(avg_order, big.mark = ","), "orders", sep = " "), col = "Median"),
                size = 1,
                group = 1,
                show.legend = T) +  # Show the legend for the pink line
      geom_line(aes(y = total_order, col = "Total Order"),
                size = 1,
                group = 2) +
      geom_point(size = 3, col = "#041675") +
      labs(
        title = glue("Orders"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_order) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      scale_color_manual(values = c("Total Order" = "#52cbff", "Median" = "pink")) +
      theme_minimal()
    
    plotly <- ggplotly(plot_order_monthly_trend_by_year, tooltip = "text")
    
    plotly <- plotly %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2, title = ""))
    
    plotly
    
  })
  
  output$overview_sales_monthly_trend_by_year <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Order.Month) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Month.Name,
                             "\n$", format(round(total_sales, 2), big.mark = ","),
                             sep = ""))
    
    max_sales <- max(plot_data$total_sales)
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_sales))
    
    plot_sales_monthly_trend_by_year <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_sales, text = tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales, 2), big.mark = ","), sep = " "), col = "Median"),
                size = 1,
                group = 1,
                show.legend = T) +  # Show the legend for the pink line
      geom_line(aes(y = total_sales, col = "Total Sales"),
                size = 1,
                group = 2) +
      geom_point(size = 3, col = "#041675") +
      labs(
        title = glue("Sales"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      scale_color_manual(values = c("Total Sales" = "#52cbff", "Median" = "pink")) +
      theme_minimal()
    
    plotly <- ggplotly(plot_sales_monthly_trend_by_year, tooltip = "text")
    
    plotly <- plotly %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2, title = ""))
    
    plotly
    
  })
  
  output$overview_profit_monthly_trend_by_year <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Order.Month) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             formated_profit = format(round(total_profit, 2), big.mark = ","),
             formated_profit = ifelse(total_profit >= 0, paste("+$", formated_profit, sep = ""), paste("-$", substr(formated_profit, 4, nchar(formated_profit)), sep = "")),
             profit_tooltip = paste(Month.Name,
                                    "\n",
                                    formated_profit, sep = ""))
    
    max_sales <- max(plot_data$total_profit)
    min_sales <- min(plot_data$total_profit)
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_profit))
    
    plot_profit_monthly_trend_by_year <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_profit, text = profit_tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales, 2), big.mark = ","), sep = " "), col = "Median"),
                size = 1,
                group = 1) +
      labs(
        title = glue("Profit"),
        x = NULL,
        y = NULL
      ) +
      geom_line(aes(y = total_profit, col = "Total Profit"), size = 1, group = 2) +
      geom_point(size = 3, col = "#041675") +
      ylim(min_sales, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      scale_color_manual(values = c("Total Profit" = "#52cbff", "Median" = "pink")) +
      theme_minimal()
    
    plotly <- ggplotly(plot_profit_monthly_trend_by_year, tooltip = "text")
    
    plotly <- plotly %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2, title = ""))
    
    plotly
    
  })
  
  output$overview_segment_distribution <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Segment, Order.Year) %>%
      summarise(total_order = n()) %>% 
      ungroup() %>%
      mutate(tooltip = glue("{total_order} orders"))
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- plot_data %>%
      filter(Order.Year == selected_year)
    
    plot_segment_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Segment,
                                         values = ~total_order, textinfo = "label+percent",text = ~paste(total_order, "orders"),
                                         hoverinfo = "text", marker = list(colors = c("#1d4a82", "#3f6ebf", "#95d5f5"))) %>%
      layout(
        title = "Segment Purchase Distributions",
        showlegend = FALSE
      )
    
    plot_segment_distribution
    
  })
  
  output$overview_category_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Category, Order.Year) %>% 
      summarise(total_order = n()) %>% 
      ungroup()
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- filter(plot_data, Order.Year == selected_year)
    
    plot_category_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Category,
                                          values = ~total_order, textinfo = "label+percent", text = ~paste(total_order, "orders"), hoverinfo = "text",
                                          marker = list(colors = c("#3f6ebf", "#1d4a82", "#95d5f5"))) %>%
      layout(
        title = "Category Purchase Distributions",
        showlegend = FALSE
      )
    
    plot_category_distribution
  })
  
  output$overview_ship_mode_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Ship.Mode, Order.Year) %>% 
      summarise(total_order = n()) %>% 
      ungroup()
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- filter(plot_data, Order.Year == selected_year)
    
    plot_ship_mode_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Ship.Mode,
                                          values = ~total_order, textinfo = "label+percent", text = ~paste(total_order, "orders"), hoverinfo = "text",
                                          marker = list(colors = c("#78bfe3", "#b6e8fa", "#3f6ebf", "#1d4a82"))) %>%
      layout(
        title = "Ship Mode Purchase Distributions",
        showlegend = FALSE
      )
    
    plot_ship_mode_distribution
  })
  
  output$overview_region_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Region, Order.Year) %>% 
      summarise(total_order = n()) %>% 
      ungroup()
    
    selected_year <- input$overview_select_year
    
    filtered_plot_data <- filter(plot_data, Order.Year == selected_year)
    
    plot_region_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Region,
                                           values = ~total_order, textinfo = "label+percent", text = ~paste(total_order, "orders"), hoverinfo = "text",
                                           marker = list(colors = c("#78bfe3", "#3f6ebf", "#b6e8fa", "#1d4a82"))) %>%
      layout(
        title = "Region Purchase Distributions",
        showlegend = FALSE
      )
    
    plot_region_distribution
  })
  
  #----sales----
  output$sales_total_sales <- renderUI({
    selected_year <- input$sales_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    sales_total <- sum(filtered_data$Sales)
    
    infoBox(
      width = 3,
      "TOTAL Sales:",
      value = paste("$", format(as.integer(sales_total), big.mark = ",")),
      color = "navy",
      icon = icon("credit-card")
    )
  })
  output$sales_total_profit <- renderUI({
    selected_year <- input$sales_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    profit_total <- sum(filtered_data$Profit)
    
    infoBox(
      width = 3,
      "TOTAL Profit:",
      value = paste("$", format(as.integer(profit_total), big.mark = ",")),
      color = "navy",
      icon = icon("sack-dollar")
    )
  })
  output$sales_total_customer <- renderUI({
    selected_year <- input$sales_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    n_customer <- length(unique(filtered_data$Customer.ID))
    infoBox(
      width = 3,
      "TOTAL Customers:",
      value = n_customer,
      color = "navy",
      icon = icon("users")
    )
  })
  output$sales_total_order <- renderUI({
    selected_year <- input$sales_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    infoBox(
      width = 3,
      "TOTAL Orders:",
      value = nrow(filtered_data),
      color = "navy",
      icon = icon("cart-shopping")
    )
  })
  
  output$sales_monthly_trend_by_year <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Order.Month) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Month.Name,
                             "\n$", format(round(total_sales, 2), big.mark = ","),
                             sep = ""))
    
    max_sales <- max(plot_data$total_sales)
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_sales))
    
    plot_sales_monthly_trend_by_year <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_sales, text = tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales, 2), big.mark = ","), sep = " "), col = "Median"),
                size = 1,
                group = 1,
                show.legend = T) +
      geom_line(aes(y = total_sales, col = "Total Sales"),
                size = 1,
                group = 2) +
      geom_point(size = 3, col = "#041675") +
      labs(
        title = glue("Sales"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      scale_color_manual(values = c("Total Sales" = "#52cbff", "Median" = "pink")) +
      theme_minimal()
    
    plotly <- ggplotly(plot_sales_monthly_trend_by_year, tooltip = "text")
    
    plotly <- plotly %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2, title = ""))
    
    plotly
  })
  
  output$sales_segment_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Segment, Order.Month) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Segment,
                             "\n", Month.Name,
                             "\n$", format(round(total_sales), big.mark = ","),
                             sep = ""))
    
    max_sales <- max(plot_data$total_sales)
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_sales))
    
    plot_sales_segment_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_sales, text = tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Segment == "Consumer"), aes(y = total_sales, col = "Consumer"),
                size = 1,
                group = 2) +
      geom_line(data = filter(filtered_plot_data, Segment == "Corporate"), aes(y = total_sales, col = "Corporate"),
                size = 1,
                group = 3) +
      geom_line(data = filter(filtered_plot_data, Segment == "Home Office"), aes(y = total_sales, col = "Home Office"),
                size = 1,
                group = 4) +
      labs(
        title = glue("Segment Sales Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal()
    
      plotly <- ggplotly(plot_sales_segment_trend, tooltip = "text")
      
      plotly <- plotly %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.2,
            title = list(text = "")
          )
        )
      
      plotly
  })
  
  output$sales_category_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Category, Order.Month) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Category,
                             "\n", Month.Name,
                             "\n$", format(round(total_sales), big.mark = ","),
                             sep = ""))
    
    max_sales <- max(plot_data$total_sales)
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_sales))
    
    plot_sales_category_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_sales, text = tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Category == "Office Supplies"), aes(y = total_sales, col = "Office Supplies"),
                size = 1,
                group = 2) +
      geom_line(data = filter(filtered_plot_data, Category == "Technology"), aes(y = total_sales, col = "Technology"),
                size = 1,
                group = 3) +
      geom_line(data = filter(filtered_plot_data, Category == "Furniture"), aes(y = total_sales, col = "Furniture"),
                size = 1,
                group = 4) +
      labs(
        title = glue("Category Sales Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal()
    
    plotly <- ggplotly(plot_sales_category_trend, tooltip = "text")
    
    plotly <- plotly %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          title = list(text = "")
        )
      )
    
    plotly
  })
  
  output$sales_region_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Region, Order.Month) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Region,
                             "\n", Month.Name,
                             "\n$", format(round(total_sales), big.mark = ","),
                             sep = ""))
    
    max_sales <- max(plot_data$total_sales)
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_sales))
    
    plot_sales_region_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_sales, text = tooltip)) +
      geom_line(aes(y = avg_sales, text = paste("Median:\n$", format(round(avg_sales), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1) +
      geom_line(data = filter(filtered_plot_data, Region == "East"), aes(y = total_sales, col = "East"),
                size = 1,
                group = 3) +
      geom_line(data = filter(filtered_plot_data, Region == "West"), aes(y = total_sales, col = "West"),
                size = 1,
                group = 2) +
      geom_line(data = filter(filtered_plot_data, Region == "Central"), aes(y = total_sales, col = "Central"),
                size = 1,
                group = 4) +
      geom_line(data = filter(filtered_plot_data, Region == "South"), aes(y = total_sales, col = "South"),
                size = 1,
                group = 5) +
      labs(
        title = glue("Region Sales Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(0, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    plotly <- ggplotly(plot_sales_region_trend, tooltip = "text")
    
    plotly <- plotly %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          title = list(text = "")
        )
      )
    
    plotly
  })
  
  output$sales_category_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Category, Order.Year) %>% 
      summarise(total_sales = sum(Sales)) %>% 
      ungroup()
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_sales = format(round(total_sales), big.mark = ",")
      )
    
    plot_category_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Category,
                                          values = ~total_sales, textinfo = "label+percent", text = ~paste("Total $", formatted_sales, sep = ""), hoverinfo = "text",
                                          marker = list(colors = c("#3f6ebf", "#1d4a82", "#95d5f5"))) %>%
      layout(
        title = "Category Sales Distributions",
        showlegend = FALSE
      )
    
    plot_category_distribution
  })
  
  output$sales_segment_distribution <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Segment, Order.Year) %>%
      summarise(total_sales = sum(Sales)) %>% 
      ungroup() %>%
      mutate(tooltip = glue("${total_sales}"))
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_sales = format(round(total_sales), big.mark = ",")
      )
    
    plot_segment_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Segment,
                                          values = ~total_sales, textinfo = "label+percent", text = ~paste("Total $", formatted_sales, sep = ""), hoverinfo = "text",
                                          marker = list(colors = c("#3f6ebf", "#1d4a82", "#95d5f5"))) %>%
      layout(
        title = "Segment Sales Distributions",
        showlegend = FALSE
      )
    
    plot_segment_distribution
    
  })
  
  output$sales_region_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Region, Order.Year) %>% 
      summarise(total_sales = sum(Sales)) %>% 
      ungroup()
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_sales = format(round(total_sales), big.mark = ",")
      )
    
    plot_region_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Region,
                                          values = ~total_sales, textinfo = "label+percent", text = ~paste("Total $", formatted_sales, sep = ""), hoverinfo = "text",
                                        marker = list(colors = c("#78bfe3", "#3f6ebf", "#b6e8fa", "#1d4a82"))) %>%
      layout(
        title = "Region Sales Distributions",
        showlegend = FALSE
      )
    
    plot_region_distribution
  })
  
  #----profit----
  output$profit_total_sales <- renderUI({
    selected_year <- input$profit_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    sales_total <- sum(filtered_data$Sales)
    
    infoBox(
      width = 3,
      "TOTAL Sales:",
      value = paste("$", format(as.integer(sales_total), big.mark = ",")),
      color = "navy",
      icon = icon("credit-card")
    )
  })
  output$profit_total_profit <- renderUI({
    selected_year <- input$profit_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    profit_total <- sum(filtered_data$Profit)
    
    infoBox(
      width = 3,
      "TOTAL Profit:",
      value = paste("$", format(as.integer(profit_total), big.mark = ",")),
      color = "navy",
      icon = icon("sack-dollar")
    )
  })
  output$profit_total_customer <- renderUI({
    selected_year <- input$profit_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    n_customer <- length(unique(filtered_data$Customer.ID))
    infoBox(
      width = 3,
      "TOTAL Customers:",
      value = n_customer,
      color = "navy",
      icon = icon("users")
    )
  })
  output$profit_total_order <- renderUI({
    selected_year <- input$profit_select_year
    filtered_data <- filter(ecom, Order.Year == selected_year)
    infoBox(
      width = 3,
      "TOTAL Orders:",
      value = nrow(filtered_data),
      color = "navy",
      icon = icon("cart-shopping")
    )
  })
  
  output$profit_monthly_trend_by_year <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Order.Month) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             formated_profit = format(round(total_profit, 2), big.mark = ","),
             formated_profit = ifelse(total_profit >= 0, paste("+$", formated_profit, sep = ""), paste("-$", substr(formated_profit, 4, nchar(formated_profit)), sep = "")),
             profit_tooltip = paste(Month.Name,
                                    "\n",
                                    formated_profit, sep = ""))
    
    max_sales <- max(plot_data$total_profit)
    min_sales <- min(plot_data$total_profit)
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_sales = median(total_profit))
    
    plot_profit_monthly_trend_by_year <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_profit, text = profit_tooltip)) +
      geom_line(aes(y = avg_sales,
                    text = paste("Median:\n$", format(round(avg_sales, 2), big.mark = ","), sep = " "),
                    col = "Median"),
                size = 1,
                show.legend = TRUE,
                group = 1) +
      labs(
        title = glue("Profit"),
        x = NULL,
        y = NULL
      ) +
      geom_line(size = 1, aes(y = total_profit, col = "Total Profit"), group = 2, show.legend = TRUE) +
      geom_point(size = 3, col = "#041675", show.legend = FALSE) +
      ylim(min_sales, max_sales) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      scale_color_manual(values = c("Total Profit" = "#52cbff", "Median" = "pink")) +
      theme_minimal()
    
    plotly <- ggplotly(plot_profit_monthly_trend_by_year, tooltip = "text")
    
    plotly<- plotly %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.2, title = ""))
    
    plotly
    
  })
  
  output$profit_segment_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Segment, Order.Month) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Segment,
                             "\n", Month.Name,
                             "\n$", format(round(total_profit), big.mark = ","),
                             sep = ""))
    
    max_profit <- max(plot_data$total_profit)
    min_profit <- min(plot_data$total_profit)
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_profit = median(total_profit))
    
    plot_profit_segment_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_profit, text = tooltip)) +
      geom_line(aes(y = avg_profit, text = paste("Median:\n$", format(round(avg_profit), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1,
                show.legend = TRUE) +
      geom_line(data = filter(filtered_plot_data, Segment == "Consumer"), aes(y = total_profit, col = "Consumer"),
                size = 1,
                group = 2,
                show.legend = TRUE) +
      geom_line(data = filter(filtered_plot_data, Segment == "Corporate"), aes(y = total_profit, col = "Corporate"),
                size = 1,
                group = 3,
                show.legend = TRUE) +
      geom_line(data = filter(filtered_plot_data, Segment == "Home Office"), aes(y = total_profit, col = "Home Office"),
                size = 1,
                group = 4,
                show.legend = TRUE) +
      labs(
        title = glue("Segment Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(min_profit, max_profit) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal()
    
    plot_profit_segment_trend <- ggplotly(plot_profit_segment_trend, tooltip = "text")
    
    plot_profit_segment_trend <- plot_profit_segment_trend %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          title = list(text = "") 
        )
      )
    
    plot_profit_segment_trend
    
  })
  
  output$profit_category_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Category, Order.Month) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Category,
                             "\n", Month.Name,
                             "\n$", format(round(total_profit), big.mark = ","),
                             sep = ""))
    
    max_profit <- max(plot_data$total_profit)
    min_profit <- min(plot_data$total_profit)
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_profit = median(total_profit))
    
    plot_profit_category_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_profit, text = tooltip)) +
      geom_line(aes(y = avg_profit, text = paste("Median:\n$", format(round(avg_profit), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Category == "Office Supplies"), aes(y = total_profit, col = "Office Supplies"),
                size = 1,
                group = 2,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Category == "Technology"), aes(y = total_profit, col = "Technology"),
                size = 1,
                group = 3,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Category == "Furniture"), aes(y = total_profit, col = "Furniture"),
                size = 1,
                group = 4,
                show.legend = T) +
      labs(
        title = glue("Category Profit Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(min_profit, max_profit) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal()
    
    plot_profit_category_trend <- ggplotly(plot_profit_category_trend, tooltip = "text")
    
    plot_profit_category_trend <- plot_profit_category_trend %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          title = list(text = "") 
        )
      )
    
    plot_profit_category_trend
    
  })
  
  output$profit_region_trend <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Order.Year, Region, Order.Month) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(Month.Name = month.name[Order.Month],
             tooltip = paste(Region,
                             "\n", Month.Name,
                             "\n$", format(round(total_profit), big.mark = ","),
                             sep = ""))
    
    max_profit <- max(plot_data$total_profit)
    min_profit <- min(plot_data$total_profit)
    
    selected_year <- input$sales_select_year
    
    filtered_plot_data <- plot_data %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(avg_profit = median(total_profit))
    
    plot_profit_region_trend <- ggplot(filtered_plot_data, aes(x = Order.Month, y = total_profit, text = tooltip)) +
      geom_line(aes(y = avg_profit, text = paste("Median:\n$", format(round(avg_profit), big.mark = ","), sep = " ")),
                size = 1,
                col = "pink",
                group = 1,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Region == "East"), aes(y = total_profit, col = "East"),
                size = 1,
                group = 3,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Region == "West"), aes(y = total_profit, col = "West"),
                size = 1,
                group = 2,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Region == "Central"), aes(y = total_profit, col = "Central"),
                size = 1,
                group = 4,
                show.legend = T) +
      geom_line(data = filter(filtered_plot_data, Region == "South"), aes(y = total_profit, col = "South"),
                size = 1,
                group = 5,
                show.legend = T) +
      labs(
        title = glue("Region Profit Trends"),
        x = NULL,
        y = NULL
      ) +
      ylim(min_profit, max_profit) +
      scale_x_continuous(breaks = seq(min(filtered_plot_data$Order.Month), max(filtered_plot_data$Order.Month), by = 2),
                         labels = function(x) month.name[x]) +
      theme_minimal()
    
    plot_profit_region_trend <- ggplotly(plot_profit_region_trend, tooltip = "text")
    
    plot_profit_region_trend <- plot_profit_region_trend %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          title = list(text = "") 
        )
      )
    
    plot_profit_region_trend
  })
  
  
  output$profit_segment_distribution <- renderPlotly({
    plot_data <- ecom %>%
      group_by(Segment, Order.Year) %>%
      summarise(total_profit = sum(Profit)) %>% 
      ungroup() %>%
      mutate(tooltip = glue("${total_profit}"))
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>%
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_profit = format(round(total_profit), big.mark = ',')
      )
    
    plot_segment_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Segment,
                                         values = ~total_profit, textinfo = "label+percent", text = ~paste("Total $", formatted_profit, sep = ""), hoverinfo = "text",
                                         marker = list(colors = c("#3f6ebf", "#1d4a82", "#95d5f5"))) %>%
      layout(
        title = "Segment Profit Distributions",
        showlegend = FALSE
      )
    
    plot_segment_distribution
  })
  
  output$profit_category_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Category, Order.Year) %>% 
      summarise(total_profit = sum(Profit)) %>% 
      ungroup()
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>%
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_profit = format(round(total_profit), big.mark = ',')
      )
    
    plot_category_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Category,
                                          values = ~total_profit, textinfo = "label+percent", text = ~paste("Total $", formatted_profit, sep = ""), hoverinfo = "text",
                                          marker = list(colors = c("#3f6ebf", "#1d4a82", "#95d5f5"))) %>%
      layout(
        title = "Category Profit Distributions",
        showlegend = FALSE,
        hovermode = "hoverinfo"
      )
    
    plot_category_distribution
  })
  
  output$profit_region_distribution <- renderPlotly({
    plot_data <- ecom %>% 
      group_by(Region, Order.Year) %>% 
      summarise(total_profit = sum(Profit)) %>% 
      ungroup()
    
    selected_year <- input$profit_select_year
    
    filtered_plot_data <- plot_data %>%
      filter(Order.Year == selected_year) %>% 
      mutate(
        formatted_profit = format(round(total_profit), big.mark = ',')
      )
    
    plot_region_distribution <- plot_ly(filtered_plot_data, type = "pie", labels = ~Region,
                                        values = ~total_profit, textinfo = "label+percent", text = ~paste("Total $", formatted_profit, sep = ""), hoverinfo = "text",
                                        marker = list(colors = c("#78bfe3", "#3f6ebf", "#b6e8fa", "#1d4a82"))) %>%
      layout(
        title = "Region Profit Distributions",
        showlegend = FALSE,
        hovermode = "hoverinfo"
      )
    
    plot_region_distribution
  })
  
  #----product----
  output$product_total_sales <- renderUI({
    selected_year <- input$product_select_year
    selected_segment <- input$product_select_segment
    
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    if (selected_segment != "All"){
      filtered_data <- filter(filtered_data, Segment == selected_segment)
    }
    
    sales_total <- sum(filtered_data$Sales)
    
    infoBox(
      width = 3,
      "TOTAL Sales:",
      value = paste("$", format(as.integer(sales_total), big.mark = ",")),
      color = "navy",
      icon = icon("credit-card")
    )
  })
  output$product_total_profit <- renderUI({
    selected_year <- input$product_select_year
    selected_segment <- input$product_select_segment
    
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    if (selected_segment != "All"){
      filtered_data <- filter(filtered_data, Segment == selected_segment)
    }
    
    profit_total <- sum(filtered_data$Profit)
    
    infoBox(
      width = 3,
      "TOTAL Profit:",
      value = paste("$", format(as.integer(profit_total), big.mark = ",")),
      color = "navy",
      icon = icon("sack-dollar")
    )
  })
  output$product_total_customer <- renderUI({
    selected_year <- input$product_select_year
    selected_segment <- input$product_select_segment
    
    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    if (selected_segment != "All"){
      filtered_data <- filter(filtered_data, Segment == selected_segment)
    }
    
    n_customer <- length(unique(filtered_data$Customer.ID))
    infoBox(
      width = 3,
      "TOTAL Customers:",
      value = n_customer,
      color = "navy",
      icon = icon("users")
    )
  })
  output$product_total_order <- renderUI({
    selected_year <- input$product_select_year
    selected_segment <- input$product_select_segment

    filtered_data <- filter(ecom, Order.Year == selected_year)
    
    if (selected_segment != "All"){
      filtered_data <- filter(filtered_data, Segment == selected_segment)
    }
    
    infoBox(
      width = 3,
      "TOTAL Orders:",
      value = nrow(filtered_data),
      color = "navy",
      icon = icon("cart-shopping")
    )
  })
  
  output$product_top10_sales <- renderPlotly({
    selected_year <- input$product_select_year
    selected_segment <- input$product_select_segment
    
    plot_data <- ecom %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(Product.Name = iconv(Product.Name, "ASCII", "UTF-8", sub = ""))
    
    if (selected_segment != "All"){
      plot_data <- filter(plot_data, Segment == selected_segment)
    }
    
    plot_data <- plot_data %>% 
      group_by(Product.ID) %>%  # Group only by Product.ID
      summarise(Product.Name = first(Product.Name),
                Category = first(Category),
                Sub.Category = first(Sub.Category),
                total_sales = sum(Sales)) %>% 
      arrange(desc(total_sales)) %>% 
      mutate(
        formatted_sales = format(round(total_sales, 2), big.mark = ","),
        tooltip = glue("{Product.Name}
                   Category: {Sub.Category} in {Category}
                   Total sales: ${formatted_sales}"),
        short_name = paste(str_sub(Product.Name, end = 15), "...", sep = "")
      )
    
    top10 <- plot_data %>% head(10)
    
    plot_top10_product <- ggplot(top10, aes(y = reorder(Product.ID, total_sales), x = total_sales, text = tooltip)) +
      geom_bar(stat = "identity", size = 1, aes(fill = total_sales), show.legend = F) +
      scale_fill_gradient(low = "#1d4a82", high = "#78bfe3") +
      labs(
        title = "Top 10 Products by Sales",
        y = NULL,
        x = "Total Sales"
      ) +
      theme_minimal()
    
    ggplotly(plot_top10_product, tooltip = "text")
  })
  
  output$product_top10_profit <- renderPlotly({
    selected_year <- input$product_select_year %>% as.integer()
    selected_segment <- input$product_select_segment
    
    plot_data <- ecom %>% 
      filter(Order.Year == selected_year) %>% 
      mutate(Product.Name = iconv(Product.Name, "ASCII", "UTF-8", sub = ""))
    
    if (selected_segment != "All"){
      plot_data <- filter(plot_data, Segment == selected_segment)
    }
    
    plot_data <- plot_data %>% 
      group_by(Product.ID) %>%  # Group only by Product.ID
      summarise(Product.Name = first(Product.Name),
                Category = first(Category),
                Sub.Category = first(Sub.Category),
                total_profit = sum(Profit)) %>% 
      arrange(desc(total_profit)) %>% 
      mutate(
        formatted_profit = format(round(total_profit, 2), big.mark = ","),
        tooltip = glue("{Product.Name}
                   Category: {Sub.Category} in {Category}
                   Total profits: ${formatted_profit}"),
        short_name = paste(str_sub(Product.Name, end = 15), "...", sep = "")
      )
    
    top10 <- plot_data %>% head(10)
    
    plot_top10_product <- ggplot(top10, aes(y = reorder(Product.ID, total_profit), x = total_profit, text = tooltip)) +
      geom_bar(stat = "identity", size = 2, aes(fill = total_profit), show.legend = FALSE) +
      scale_fill_gradient(low = "#1d4a82", high = "#78bfe3") +
      labs(
        title = "Top 10 Products by Profit",
        y = NULL,
        x = "Total Profit"
      ) +
      theme_minimal()
    
    ggplotly(plot_top10_product, tooltip = "text")
  })
  
  #----data----
  output$data_table_giant <- renderDataTable({
    datatable(data = ecom,
              options = list(scrollX = TRUE, scrollY = TRUE))
    
  })
}

