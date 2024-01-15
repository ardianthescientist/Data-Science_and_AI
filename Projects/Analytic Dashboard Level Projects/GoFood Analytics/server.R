function(input, output, session) {
  
  # overview: plot rank kategory merchant berdasarkan total order
  output$plot_gofood_category_rank <- renderPlotly({
    gofood_by_category <- gofood %>% 
      group_by(merchant_category) %>% 
      summarise(count=n()) %>% 
      ungroup() %>% 
      arrange(-count) %>% 
      mutate(
        plotly_tooltip = glue(
          "{merchant_category}
      {count} orders"
        )
      )
    
    plot_gofood_category_rank <- ggplot(gofood_by_category,
                                        aes(y=count, x=reorder(merchant_category, count), text=plotly_tooltip)) +
      geom_col(aes(fill=count), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = "MERCHANT CATEGORY RANKING",
        x = NULL,
        y = "Total orders",
      ) +
      theme_minimal()
    
    ggplotly(plot_gofood_category_rank, tooltip="text")
  })
  
  # overview: plot top 15 merchant berdasarkan total order
  output$plot_gofood_top15_merchant <- renderPlotly({
    gofood_by_merchant <- gofood %>% 
      group_by(merchant_name, kelurahan_merchant, merchant_category) %>% 
      summarise(count=n()) %>% 
      ungroup() %>% 
      arrange(-count) %>% 
      mutate(
        plotly_tooltip = glue(
          "{merchant_name}
      {count} orders
      Kategori: {merchant_category}
      Alamat: Kelurahan {kelurahan_merchant}"
        )
      )
    
    n_merchant <- 15
    
    plot_gofood_top_merchant <- ggplot(head(gofood_by_merchant, n_merchant),
                                       aes(x=count, y=reorder(merchant_name, count), text=plotly_tooltip)) +
      geom_col(aes(fill=count), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = glue("TOP {n_merchant} MERCHANTS by TOTAL ORDER"),
        y = NULL,
        x = "Total orders",
      ) +
      scale_x_continuous(breaks = seq(0, max(gofood_by_merchant$count), by = 1)) +
      theme_minimal()
    
    ggplotly(plot_gofood_top_merchant, tooltip="text")
  })
  
  # overview: plot aktivitas order customer untuk setiap jam
  output$plot_gofood_order_by_hour <- renderPlotly({
    # Create a complete dataset with all hours from 0 to 23
    all_hours <- data.frame(order_hour = 0:23)
    
    # Summarize the data
    gofood_order_by_hour <- gofood %>% 
      group_by(order_hour) %>% 
      summarise(count = n()) %>% 
      ungroup() %>% 
      complete(order_hour = all_hours$order_hour, fill = list(count = 0)) %>%  # Left join to fill missing hours with 0 count
      arrange(order_hour)
    
    plot_gofood_order_by_hour <- ggplot(gofood_order_by_hour,
                                        aes(x = order_hour, y = count, text = glue("Pukul {order_hour}\n{count} orders"))) +
      geom_line(size = 1, col = "lightgreen", group = 1) +
      geom_point(col = "darkgreen", size = 3) +
      labs(
        title = "CUSTOMER ORDER ACTIVITIES",
        y = "Total orders",
        x = "Hour"
      ) +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, 1)) +
      scale_y_continuous(limits = c(0, NA)) # NA allows ggplot to calculate the upper limit automatically
    
    ggplotly(plot_gofood_order_by_hour, tooltip = "text")
  })
  
  
  # merchant: slider input dinamis
  output$dynamic_slider_input <- renderUI({
    if (input$selected_merchant_category == "All") {
      max_value <- length(unique(gofood$merchant_name))
    } else {
      filtered_gofood <- gofood[gofood$merchant_category == input$selected_merchant_category,]
      max_value <- length(unique(filtered_gofood$merchant_name))
    }
    
    max_value <- min(max_value, 20)
    
    sliderInput("input_n_merchant", "Top berapa merchant?", min = 1, max = max_value, value = 10, step = 1)
  })
  
  # merchant: plot top n order merchant berdasarkan kategori
  output$plot_gofood_top_n_merchant_total_order_by_category <- renderPlotly({
    selected_category <- input$selected_merchant_category
    n_merchant <- input$input_n_merchant
    
    if (selected_category == "All") {
      filtered_gofood <- gofood
      title_text <- glue("TOP {n_merchant} MERCHANTS for ALL CATEGORIES")
    } else {
      filtered_gofood <- filter(gofood,
                                merchant_category == selected_category)
      title_text <- glue("TOP {n_merchant} MERCHANTS in '{selected_category}' CATEGORY")
    }
    
    gofood_merchant_total_order_by_category <- filtered_gofood %>%
      group_by(merchant_name, merchant_category) %>%
      summarise(count=n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      head(n_merchant) %>% 
      mutate(
        plotly_tooltip = glue(
          "{merchant_name}
      {count} orders")
      )
    
    plot_gofood_top_n_merchant_total_order_by_category <- ggplot(data=gofood_merchant_total_order_by_category,
                                                                 aes(x=count,
                                                                     y=reorder(merchant_name, count),
                                                                     text=plotly_tooltip)) +
      geom_col(aes(fill=count), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = title_text,
        subtitle = "by total orders",
        y = NULL,
        x = "Total orders"
      ) +
      scale_x_continuous(breaks=seq(0, max(gofood_merchant_total_order_by_category$count), by=1)) +
      theme_minimal()
    
    ggplotly(plot_gofood_top_n_merchant_total_order_by_category, tooltip="text")
  })
  
  # merchant: plot top n transaksi merchant berdasarkan kategori
  output$plot_gofood_top_n_merchant_total_transaction_by_category <- renderPlotly({
    selected_category <- input$selected_merchant_category
    n_merchant <- input$input_n_merchant
    
    if (selected_category == "All") {
      filtered_gofood <- gofood
      title_text <- glue("TOP {n_merchant} MERCHANTS for ALL CATEGORIES")
    } else {
      filtered_gofood <- filter(gofood,
                                merchant_category == selected_category)
      title_text <- glue("TOP {n_merchant} MERCHANTS in '{selected_category}' CATEGORY")
    }
    
    gofood_merchant_total_transaction_by_category <- filtered_gofood %>%
      group_by(merchant_name) %>%
      summarise(sum_amount = sum(harga_produk)) %>%
      ungroup() %>%
      arrange(desc(sum_amount)) %>%
      head(n_merchant) %>%
      mutate(
        comma_sum_amount = format(sum_amount, big.mark = ","),
        plotly_tooltip = glue(
          "{merchant_name}
      Total transaction: Rp{comma_sum_amount}")
      )
    
    plot_gofood_top_n_merchant_total_transaction_by_category <- ggplot(data = gofood_merchant_total_transaction_by_category,
                                                                       aes(x = sum_amount,
                                                                           y = reorder(merchant_name, sum_amount),
                                                                           text = plotly_tooltip)) +
      geom_col(aes(fill=sum_amount), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = title_text,
        y = NULL,
        x = "Total transactions"
      ) +
      scale_x_continuous(
        labels = function(x) paste0("Rp", format(x, big.mark=",", scientific=F))
      ) +
      theme_minimal()
    
    ggplotly(plot_gofood_top_n_merchant_total_transaction_by_category, tooltip="text")
  })
  
  # merchant: plot segmentasi kecamatan merchant
  output$plot_gofood_merchant_by_kecamatan <- renderPlotly({
    gofood_by_kecamatan <- gofood %>% 
      group_by(kecamatan_merchant) %>% 
      summarise(count = n()) %>% 
      ungroup()
    
    plot_gofood_merchant_by_kecamatan <- ggplot(gofood_by_kecamatan,
                                                aes(x = reorder(kecamatan_merchant, count),
                                                    y = count,
                                                    text = glue("{kecamatan_merchant}
                                                                {count} merchants"))) +
      geom_col(aes(fill=count), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = "MERCHANT SEGMENTATION by KECAMATAN",
        x = NULL,
        y = "Total orders"
      ) +
      theme_minimal()
    
    ggplotly(plot_gofood_merchant_by_kecamatan, tooltip = "text")
  })
  
  # customer: plot aktivitas order customer untuk setiap jam berdasarkan kategori
  output$plot_gofood_category_order_by_hour <- renderPlotly({
    selected_category <- input$selected_customer_category
    
    if (selected_category == "All") {
      filtered_gofood <- gofood
      title_text <- glue("CUSTOMER ACTIVITY for ALL CATEGORIES")
    } else {
      filtered_gofood <- filter(gofood, merchant_category == selected_category)
      title_text <- glue("CUSTOMER ACTIVITY in '{selected_category}' CATEGORY")
    }
    
    gofood_order_by_hour <- filtered_gofood %>% 
      group_by(order_hour) %>% 
      summarise(count = n()) %>% 
      ungroup() %>% 
      complete(order_hour = 0:23, fill = list(count = 0)) %>% # Ensure all hours are represented with 0 counts for missing data
      arrange(order_hour)
    
    plot_gofood_order_by_hour <- ggplot(gofood_order_by_hour,
                                        aes(x = order_hour, y = count, text = glue("Pukul {order_hour}\n{count} orders"))) +
      geom_line(size = 1, col = "lightgreen", group = 1) +
      geom_point(col = "darkgreen", size = 3) +
      labs(
        title = title_text,
        y = "Total orders",
        x = "Hour"
      ) +
      scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, by = 1)) +
      scale_y_continuous(limits = c(0, NA), breaks = seq(0, 21, by = 3)) +
      theme_minimal()
    
    ggplotly(plot_gofood_order_by_hour, tooltip = "text")
  })
  
  
  # customer: plot segmentasi umur customer berdasarkan kategori
  output$plot_gofood_customer_by_age <- renderPlotly({
    selected_category <- input$selected_customer_category
    
    if (selected_category == "All") {
      filtered_gofood <- gofood
      title_text <- glue("CUSTOMER AGE DISTRIBUTION for ALL CATEGORIES")
    } else {
      filtered_gofood <- filter(gofood,
                                merchant_category == selected_category)
      title_text <- glue("CUSTOMER AGE DISTRIBUTION in '{selected_category}' CATEGORY")
    }
    
    plot_gofood_customer_by_age <- ggplot(filtered_gofood, aes(x = customer_age,
                                                               fill = after_stat(count),
                                                               text = paste(glue("Age {x}"), "\n", after_stat(count),"customers"))) +
      geom_histogram(binwidth=5, show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = title_text,
        x = "Age",
        y = "Total Customers"
      ) +
      scale_x_continuous(breaks=seq(min(filtered_gofood$customer_age), max(filtered_gofood$customer_age), by=2)) +
      theme_minimal()
    
    ggplotly(plot_gofood_customer_by_age, tooltip="text")
  })
  
  # customer: plot segmentasi kecamatan customer
  output$plot_gofood_customer_by_kecamatan <- renderPlotly ({
    gofood_by_kecamatan <- gofood %>% 
      group_by(kecamatan_customer) %>% 
      summarise(count = n()) %>% 
      ungroup()
    
    plot_gofood_customer_by_kecamatan <- ggplot(gofood_by_kecamatan,
                                                aes(x = reorder(kecamatan_customer, count),
                                                    y = count,
                                                    text = glue("{kecamatan_customer}
                                                                {count} customers"))) +
      geom_col(aes(fill=count), show.legend=F) +
      scale_fill_gradient(low="darkgreen", high="lightgreen") +
      labs(
        title = "CUSTOMER SEGMENTATION by KECAMATAN",
        x = NULL,
        y = "Total orders"
      ) +
      theme_minimal()
    
    ggplotly(plot_gofood_customer_by_kecamatan, tooltip = "text")
  })
  
  # data: tabel data gofood
  output$table_data_gojek <- renderDataTable({
    columns_to_anonymize <- c("alamat_customer", "customer_birthdate", "customer_age", "kelurahan_customer")

    datatable(
      data = gofood %>% 
        mutate_at(vars(columns_to_anonymize), ~ "Private Information"),
      options = list(scrollX=T, scrollY=F))
  })
}
