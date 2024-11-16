#!/usr/bin/env Rscript
pkg <- c("lattice")
suppressMessages(invisible(lapply(pkg, require, character.only = T, quietly = T)))

get_stas_demo <- function() {
  file <- sub("/modules", "", paste0(get_file_path(), "/test/stastistics_demo.csv"))
  df <- read.csv(file, header = T)
  return(df)
}

desc_summarize <- function(data) {
  group <- colnames(data)[1]
  value <- colnames(data)[2]
  exp <- eval(parse(text = paste(value, "~", group)))
  Summarize(exp, data = data)
}

run_desc <- function(input, output, data) {
  # Descriptive Statistics
  output$descriptive_summary <- renderPrint(
    exec_func(args = data, func = desc_summarize, except = cat("", sep = ""))
  )

  output$descriptive_barplot <- renderPlot(
    width = 450, height = 450,
    exec_func(args = data, func = plot_histogram)
  )

  output$descriptive_boxplot <- renderPlot(
    width = 450, height = 450,
    exec_func(args = data, func = plot_boxplot)
  )
}

run_normality_test <- function(input, output, data) {
  # Normality Test
  normality_method <- reactive(input$s_nor_method)

  output$normality_summary <- renderPrint({
    if (is.null(data)) {
      cat("", sep = "")
    } else {
      data_group <- base::split(data, data[, 1])
      result <- paste0("Normality test", "\n")
      for (i in seq(length(data_group))) {
        group_name <- as.character(data_group[[i]][, 1][1])
        result <- paste0(result, "Group: ", group_name)
        if (normality_method() == "Shapiro-Wilk test (3<n<50)") {
          shapiro_result <- shapiro.test(data_group[[i]][, 2])
          words <- paste0(
            ", Shapiro-Wilk normality test W = ",
            shapiro_result$statistic,
            " p-value = ", shapiro_result$p.value, "\n"
          )
          result <- paste0(result, words)
        } else if (normality_method() == "Kolmogorov-Smirnov test") {
          ks.result <- ks.test(data_group[[i]][, 2], pnorm,
            mean = mean(data_group[[i]][, 2]),
            sd = sd(data_group[[i]][, 2])
          )
          words <- paste0(
            ", Kolmogorov-Smirnov normality test D = ",
            ks.result$statistic,
            " p-value = ", ks.result$p.value, "\n"
          )
          result <- paste0(result, words)
        }
      }
      cat(result, sep = "\n", append = T)
    }
  })
  qqplot_line <- function(x) {
    par(pin = c(4, 4))
    qqnorm(x)
    qqline(x)
  }
  output$normality_plot <- renderPlot({
    if (is.null(data)) {
      return(NULL)
    }
    qqplot_line(data[, 2])
  })
}

run_hetero_test <- function(input, output, data) {
  # Heteroscedasticity test
  output$hetero_result <- renderPrint({
    if (is.null(data)) {
      cat("", sep = "")
      warning("None file")
    } else {
      data[, 1] <- as.factor(data[, 1])
      cols <- colnames(data)
      exp <- eval(parse(text = paste(cols[2], "~", cols[1])))
      leveneTest(exp, data = data)
    }
  })
}

run_sign_test <- function(input, output, data) {
  data[, 1] <- as.factor(data[, 1])
  group <- colnames(data[1])
  value <- colnames(data[2])
  type <- input$m_type
  # type <- "parametric"
  if (type == "parametric") {
    formula <- as.formula(paste(value, "~", group, "-1", sep = " "))
  } else if (type == "nonparametric") {
    formula <- as.formula(paste(value, "~", group, sep = " "))
  }

  method <- input$m_method
  # method <- "Least Squares Means"
  # mcompareresult <- lm(formula, data=data)
  if (method == "Least Squares Means") {
    model <- lm(formula, data = data)
  } else if (method == "Kruskal-Wallis test for equal variances") {
    model <- kruskal.test(formula, data = data)
  } else if (method == "Welch's anova for unequal variances") {
    model <- oneway.test(formula, data = data, var.equal = FALSE)
  }

  output$sign_summary <- renderPrint(
    if (is.null(data)) {
      cat("", sep = "")
    } else if (type == "parametric") {
      summary(model)
    } else if (type == "nonparametric") {
      model
    }
  )
  if (type == "parametric") {
    output$mcompare_plot <- renderPlot(
      width = 450, height = 450,
      if (is.null(data)) {
        return(NULL)
      } else {
        Info <- summary(model)
        Coef <- as.data.frame(Info$coefficients[, 1:2])
        Coef <- within(Coef, {
          lower <- Estimate - qt(p = 0.90, df = Info$df[2]) * `Std. Error`
          upper <- Estimate + qt(p = 0.90, df = Info$df[2]) * `Std. Error`
          group <- rownames(Coef)
        })
        ggplot(Coef, aes(x = Estimate, y = group)) +
          geom_point() +
          geom_errorbarh(aes(xmin = lower, xmax = upper), height = .3) +
          ggtitle(paste(value, "by", group, "calculated from regression model"))
      }
    )
  } else if (type == "nonparametric") {
    output$mcompare_plot <- renderPlot(
      ggplot(data, aes(x = Month, y = Temp, fill = Month)) +
        geom_boxplot()
    )
  }


  output$hoctest <- renderPrint(
    if (is.null(data)) {
      cat("", sep = "")
    } else if (method == "Least Squares Means") {
      lsm <- lsmeans(model, group)
      contrast(lsm, method = "pairwise", interaction = T)
    } else if (method == "Kruskal-Wallis test for equal variances") {
      dunntest <- dunnTest(formula, data = data, method = "bh")
      out <- capture.output(dunntest)
      cat("### Dunn test\n", out, sep = "\n")
      # Dunn (1964) Kruskal-Wallis multiple comparison
      # p-values adjusted with the Benjamini-Hochberg method.
    } else if (method == "Welch's anova for unequal variances") {
      # oneway(data[,1], y = data[,2], posthoc = 'games-howell')
      out <- rstatix::games_howell_test(data, formula,
        conf.level = 0.95,
        detailed = FALSE
      )
      cat("Games-Howell Post-Hoc Test\n")
      out
    }
  )
}
