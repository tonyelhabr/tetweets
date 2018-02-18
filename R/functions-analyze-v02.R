
validate_params <- function(params_input) {
  # if (is.null(params_input$names) & !is.null(params_input$filepath_tweets)) {
  if (is.null(params_input$names_main) &
      !is.null(params_input$filepath_tweets)) {
    message("`names` will be inferred from the provided data.")
  }
  if (is.null(params_input$filepath_tweets) &
      !params_input$download) {
    params_input$download <- TRUE
  }

  if (is.null(params_input$names_main)) {
    stop("`names_main` must not be NULL.", call. = FALSE)
  }

  if (!length(setdiff(params_input$name_main, params_input$names_main)) == 0) {
    stop("`name_main` must be in `names_main`.", call. = FALSE)
  }

  # if (is.null(names(params_input$color_main))) {
  #   stop("`color_main` must be named.", call. = FALSE)
  # }
  #
  # if (is.null(names(params_input$colors_main))) {
  #   stop("`colors_main` must be named.", call. = FALSE)
  # }

  if (length(params_input$names_main) != length(params_input$colors_main)) {
    stop("`names_main` and `colors_main` must have the same length.",
         call. = FALSE)
  }

  if (length(params_input$names_main) > params_input$num_main_max) {
    stop("`names_main` should not be greater than `num_main_max`.",
         call. = FALSE)
  }

  invisible(params_input)
}

# Setup. ----
# NOTE: Unfortunately, it doesn't seem like get_timelines() and search_tweets2()
# really work as I initially intended. Some like of loop with get_timeline() and search_tweet()
# will be necessary.
process_params <- function(params_input) {
  if (!is.null(params_input$filepath_tweets)) {
    data <-
      # try(rtweet::read_twitter_csv(params_input$filepath_tweets),
      #     silent = TRUE)
      try(readRDS(params_input$filepath_tweets),
          silent = TRUE)
    if (inherits(data, "try-error")) {
      stop(
        sprintf(
          "Could not retrieve data from `filepath_tweets` %s.",
          params_input$filepath_tweets
        ),
        call. = FALSE
      )
    }

    data <-
      data %>%
      filter(name %in% c(params_input$names_main))

    # TODO: Need to validate length of params_input$names vs. screen params_input$names,
    # that name_main is in params_input$names, etc.
    if (is.null(params_input$names)) {
      if (params_input$augmented) {
        colname <- params_input$colname_augmented
      } else {
        colname <- "screen_name"
      }
      params_input$names <-
        data %>%
        distinct(!!rlang::sym(colname)) %>%
        arrange(!!rlang::sym(colname)) %>%
        pull(!!rlang::sym(colname))
    } else {
      # params_input$names <- params_input$names
    }
  } else if (params_input$download) {
    if (is.null(screen_names)) {
      stop(sprintf("Please provide `screen_names`."), call. = FALSE)
    }

    if (method == "timeline") {
      data <- try(rtweet::get_timelines(params_input$screen_names))
    } else if (method == "search") {
      data <- try(rtweet::search_tweets(params_input$screen_names))
    }
    if (inherits(data, "try-error")) {
      stop(sprintf("Could not download data."), call. = FALSE)
    }
    params_input$names <- params_input$names
  } else {
    stop("An unexpected combination of inputs was provided.", call. = FALSE)
  }
  out <-
    c(list(data = data, names = params_input$names),
      params_input)
  out
}

get_names_grid <- function(names) {
  names_grid <-
    bind_cols(x = names, y = names) %>%
    tidyr::complete(x, y)
  if(length(names) > 1) {
    names_grid <- names_grid %>% filter(x != y)
  } else {
    names_grid <- names_grid %>% mutate(y = paste0(x, "2"))
  }

  out <-
    names_grid %>%
    mutate(xy = paste0(x, "_", y)) %>%
    mutate(i = row_number())
  out
}

# NOTE: `time` column creation is replicated from https://buzzfeednews.github.io/2018-01-trump-twitter-wars/.
# Other columns are replicated from https://juliasilge.com/blog/ten-thousand-data/.
# NOTE: Also see https://github.com/mkearney/rstudioconf_tweets/blob/master/README.Rmd.
round_time <- function(x, sec) {
  as.POSIXct(hms::hms(as.numeric(x) %/% sec * sec))
}

clean_tweets <-
  function(data,
           trim_cols = TRUE,
           cols_extra = "name") {
    cols_keep <-
      c(
        "status_id",
        "created_at",
        "user_id",
        "screen_name",
        "text",
        "display_text_width",
        "reply_to_status_id",
        "is_quote",
        "is_retweet",
        "favorite_count",
        "retweet_count",
        "hashtags",
        "symbols",
        "urls_url",
        "urls_expanded_url",
        "media_expanded_url",
        "ext_media_expanded_url"
      )

    out <-
      data %>%
      select(one_of(c(cols_extra, cols_keep))) %>%
      mutate_if(is.list, funs(as.character)) %>%
      mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
      mutate(timestamp = lubridate::with_tz(timestamp, "America/Chicago")) %>%
      # mutate(time = round_time(timestamp, 60 * 60)) %>%
      mutate(time = lubridate::hour(timestamp) + lubridate::minute(timestamp) / 60)

    out
  }

# Modified from a SO answer.
compute_elapsed_time <- function(date_start, date_end, type) {
  if (type == "years" | type == "months") {
    date_start <- as.POSIXlt(date_start)
    date_end <- as.POSIXlt(date_end)
    if (type == "years") {
      out <- (date_end$year - date_start$year) - 1
    } else if (type == "months") {
      out <-
        12 * (date_end$year - date_start$year) + (date_end$mon - date_start$mon) - 1
    }
  } else if (type == "days" | type == "hours") {
    out <-
      (difftime(date_end, date_start, units = type) - 1) %>%
      round(0) %>%
      as.numeric()
  }
  out
}

compute_data_timefilter <-
  function(data, colnames_group = "name") {
    data_proc <-
      data %>%
      group_by(!!!rlang::syms(colnames_group)) %>%
      arrange(timestamp) %>%
      mutate(date_start = first(timestamp),
             date_end = last(timestamp)) %>%
      slice(1) %>%
      ungroup() %>%
      select(name, date_start, date_end) %>%
      mutate(
        yyyy_elapsed = compute_elapsed_time(date_start, date_end, "years"),
        mm_elapsed = compute_elapsed_time(date_start, date_end, "months"),
        dd_elapsed = compute_elapsed_time(date_start, date_end, "days"),
        hh_elapsed = compute_elapsed_time(date_start, date_end, "hours")
      )
    out <-
      list(
        data = data_proc,
        date_start = max(data_proc$date_start),
        date_end = min(data_proc$date_end)
      )
    out
  }

# This is "original" processing needed to trim data appropriately/dynamically
# given an unknown data set.
trim_data_bytime <-
  function(data,
           col_time = "timestamp",
           start = NULL,
           end = NULL) {
    # browser()
    out <-
      data %>%
      filter(!!rlang::sym(col_time) < end,
             !!rlang::sym(col_time) >= start)
    out
  }

# NOTE: This function takes a gg object as an input
# (so it should/can be preceded by a pipe, and, if followed by other ggplot2 commands,
#  succedeeded by a `+`.)
add_viz_bytime_elements <-
  function(viz,
           geom = c("bar", "hist"),
           colors = NULL,
           lab_subtitle = NULL) {
    geom <- match.arg(geom)
    viz_labs <-
      labs(
        x = NULL,
        y = NULL,
        title = "Count Over Time",
        subtitle = lab_subtitle
      )
    viz_theme <-
      temisc::theme_te_b_facet() +
      theme(panel.grid.major.x = element_blank()) +
      # theme(legend.position = "bottom", legend.title = element_blank())
      theme(legend.position = "none")

    if (geom == "bar") {
      viz <-
        viz +
        geom_bar(aes(y = ..count.., fill = name))
    } else if (geom == "hist") {
      viz <-
        viz +
        geom_histogram(aes(y = ..count.., fill = name), bins = 30)
    }
    viz <-
      viz +
      scale_fill_manual(values = colors) +
      facet_wrap(~ name, ncol = 1, strip.position = "right") +
      viz_labs +
      viz_theme
    viz
  }

add_data_kind_data <- function(data) {
  out <-
    data %>%
    mutate(
      hashtag = if_else(!is.na(hashtags), 1, 0),
      link = if_else(
        !is.na(media_expanded_url) & !is.na(ext_media_expanded_url),
        1,
        0
      ),
      rt = if_else(is_retweet, 1, 0),
      quote = if_else(is_quote, 1, 0),
      reply = if_else(!is.na(reply_to_status_id), 1, 0)
    ) %>%
    mutate(type =
             case_when(
               rt == TRUE ~ "RT",
               reply == TRUE ~ "reply",
               quote == TRUE ~ "quote",
               TRUE ~ "original"
             ))
  out
}

compute_pct <- function(x,
                        value = 1,
                        digits_round = 4) {
  # round(sum(x == value) / sum(!is.na(x)), digits_round)
  sum(x == value) / sum(!is.na(x))
}

visualize_byname_bykind <-
  function(data,
           geom = c("lollipop", "col"),
           colors = NULL,
           lab_kind = NULL,
           data_labels = FALSE) {
    geom <- match.arg(geom)

    viz_labs <-
      labs(
        x = NULL,
        y = NULL,
        title = '% of data of a Certain "Kind"',
        subtitle = paste0("By ", lab_kind)
      )

    viz_theme <-
      temisc::theme_te_b_facet() +
      theme(panel.grid.major.x = element_blank()) +
      theme(legend.position = "none")

    if (geom == "col") {
      viz <-
        data %>%
        ggplot(aes(x = name, y = value, fill = name)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = colors)
    } else if (geom == "lollipop") {
      viz <-
        data %>%
        ggplot(aes(x = name, y = value, color = name)) +
        ggalt::geom_lollipop(size = 2, point.size = 4) +
        scale_color_manual(values = colors)
    }

    if (data_labels) {
      viz <-
        viz +
        geom_text(aes(group = name,
                      label = sprintf("%.0f %%", 100 * value)),
                  position = position_dodge(width = 1))
    }

    viz <-
      viz +
      scale_y_continuous(labels = scales::percent_format()) +
      facet_wrap(~ kind, scales = "free") +
      coord_flip() +
      viz_labs +
      viz_theme
    viz
  }

get_data_rgx_tidiers <-
  function(rgx_ignore_custom = "^[0-9f][0-9a-f]+$") {
    rgx_unnest <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    rgx_pattern <-
      "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
    list(
      rgx_unnest = rgx_unnest,
      rgx_pattern = rgx_pattern,
      rgx_ignore_custom = rgx_ignore_custom
    )
  }

# NOTE: Make sure nested lists are converted to characters beforehand.
# Not sure why, but unnest_tokens has trouble even if the "input" parameter is not a list
# if there is a list elsewhere in the data frame.
tidy_data_unigrams <- function(data, include_rt = FALSE) {
  rgx_tidiers <- get_data_rgx_tidiers()
  out <-
    data %>%
    filter(is_retweet == include_rt) %>%
    mutate(text = str_replace_all(text, rgx_tidiers$rgx_pattern, "")) %>%
    tidytext::unnest_tokens(word, text, token = "regex", pattern = rgx_tidiers$rgx_unnest) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    filter(!str_detect(word, rgx_tidiers$rgx_ignore_custom)) %>%
    filter(str_detect(word, "[a-z]"))
  out
}

tidy_data_bigrams <- function(data, include_rt = FALSE) {
  rgx_tidiers <- get_data_rgx_tidiers()
  out <-
    data %>%
    filter(is_retweet == include_rt) %>%
    mutate(text = str_replace_all(text, rgx_tidiers$rgx_pattern, "")) %>%
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(
      bigram,
      into = c("first", "second"),
      sep = " ",
      remove = FALSE
    ) %>%
    anti_join(tidytext::stop_words, by = c("first" = "word")) %>%
    anti_join(tidytext::stop_words, by = c("second" = "word")) %>%
    filter(
      !str_detect(first, rgx_tidiers$rgx_ignore_custom) &
        !str_detect(second, rgx_tidiers$rgx_ignore_custom)
    ) %>%
    filter(str_detect(first, "[a-z]") & str_detect(second, "[a-z]"))
  out
}


# Inspired by https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd here.
# TODO: Improve this.
visualize_byname_cnt <-
  function(data,
           num_top = 20,
           facet = FALSE,
           colors = NULL) {
    if (facet) {
      # browser()
      data_proc <-
        data %>%
        count(name, word, sort = TRUE) %>%
        group_by(name) %>%
        filter(row_number(desc(n)) <= num_top) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) # %>%
      # arrange(desc(n)) %>%
      # mutate(word = forcats::fct_inorder(word)) %>%
      # ungroup()
    } else {
      data_proc <-
        data %>%
        count(word, sort = TRUE) %>%
        filter(row_number(desc(n)) <= num_top) %>%
        mutate(word = reorder(word, n))
    }

    # browser()
    viz <-
      data_proc %>%
      ggplot(aes(x = word, y = n)) +
      ggalt::geom_lollipop(size = 2, point.size = 4)

    if (facet) {
      viz <-
        viz +
        ggalt::geom_lollipop(aes(color = name), size = 2, point.size = 4) +
        scale_color_manual(values = colors) +
        drlib::scale_x_reordered() +
        facet_wrap(~ name, scales = "free") +
        temisc::theme_te_b_facet() +
        labs(subtitle = "By Name")
    } else {
      viz <-
        viz +
        scale_color_manual(values = "grey80") +
        temisc::theme_te_b()
    }

    viz <-
      viz +
      labs(x = NULL, y = NULL) +
      labs(title = "Count of Words") +
      theme(panel.grid.major.y = element_blank()) +
      coord_flip()
    viz
  }

visualize_ngrams_byname_freqs_wordcloud <-
  function(data, name_filter, color, max_words = 25) {
    data_proc <-
      data %>%
      filter(name == name_filter)
    out <-
      wordcloud::wordcloud(
        word = data_proc$word,
        freq = data_proc$n,
        max.words = max_words,
        random.order = FALSE,
        colors = color
      )
    out
  }

filter_xy_names <- function(xy_grid, xy_names, i) {
  xy_i <- xy_names[i]
  xy_i_row <- xy_grid %>% filter(xy == xy_i)
  x_i <- xy_i_row %>% pull(x)
  y_i <- xy_i_row %>% pull(y)
  out <- list(x = x_i, y = y_i, xy = xy_i)
  out
}

preprocess_xy_data <- function(data, xy_info) {
  out <-
    data %>%
    filter(name %in% c(xy_info$x, xy_info$y))
  out
}

postprocess_xy_data <- function(data, xy_info) {
  out <-
    data %>%
    mutate(name_x = xy_info$x, name_y = xy_info$y) %>%
    mutate(name_xy = paste0(name_x, "_", name_y))
  # browser()
  if (length(setdiff(c(xy_info$x, xy_info$y), names(data))) == 0) {
    out <-
      out %>%
      rename(x = !!rlang::sym(xy_info$x),
             y = !!rlang::sym(xy_info$y)) %>%
      select(name_x, name_y, name_xy, x, y, everything())
  } else {
    out <- out %>% select(name_x, name_y, name_xy, everything())
  }
  out
}


# TODO: Figure out how to use `purrr::map()` here.
wrapper_func <- function(grid, names, data, func) {
  i <- 1
  while (i <= length(names)) {
    # browser()
    xy_i_info <- filter_xy_names(grid, names, i)
    data_i_preproc <- preprocess_xy_data(data, xy_i_info)
    data_i_proc <- do.call(func, list(data_i_preproc))
    # browser()
    data_i_postproc <- postprocess_xy_data(data_i_proc, xy_i_info)
    if (i == 1) {
      out <- data_i_postproc
    } else {
      out <- bind_rows(out, data_i_postproc)
    }
    i <- i + 1
  }
  out
}

append_dummy_cols <- function(data, num_cols_expect, add = TRUE) {
  if(ncol(data) < num_cols_expect) {
    warning(sprintf("Expected %.0f columns but see only %.0f.", num_cols_expect, ncol(data)))
    num_cols_diff <- num_cols_expect - ncol(data)
    if(num_cols_diff == 1) {
      if(add) {
        # browser()
        name_last <- names(data)[ncol(data)]
        out <- bind_cols(data, data[, ncol(data)])
        names(out)[ncol(out)] <- paste0(name_last, "2")
        warning(sprintf("Added %.0f dummy column(s).", num_cols_diff))
      }
    } else {
      stop("Don't know how to add dummy columns.")
    }
  }
  out
}

compute_unigrams_freqs <- function(data) {
  out <-
    data %>%
    select(name, word, freq) %>%
    tidyr::spread(name, freq)
  if(ncol(out) == 2) {
    out <- append_dummy_cols(out, num_cols_expect = 3)
  }
  out
}


# NOTE: Filter out replies because they would make up a disproportional share of the
# top results.
# NOTE: Modified original code a bit because it doesn't really
compute_logratio <- function(data) {
  out <-
    data %>%
    filter(is.na(reply_to_status_id)) %>%
    count(word, name) %>%
    filter(n >= 10) %>%
    tidyr::spread(name, n, fill = 0)

  if(ncol(out) == 2) {
    out <- append_dummy_cols(out, num_cols_expect = 3)
  }

  nms <- names(out)
  out <-
    out %>%
    setNames(c("word", "x", "y")) %>%
    mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
    mutate(logratio = log(x / y))

  x_i <- nms[2]
  y_i <- nms[3]
  out <-
    out %>%
    setNames(c("word", x_i, y_i, "logratio")) %>%
    arrange(desc(logratio))
  out
}

prepare_viz_ngrams_byx_corrs <-
  function(data,
           num_top_ngrams = 50,
           num_top_corrs = 50,
           col_word = "word",
           col_feature = "status_id") {
    data_byword_cnt <-
      data %>%
      count(!!rlang::sym(col_word), sort = TRUE)

    data_byword_cnt_top <-
      data_byword_cnt %>%
      mutate(rank = row_number(desc(n))) %>%
      filter(rank <= num_top_ngrams)

    data_byx_corrs <-
      data %>%
      semi_join(data_byword_cnt_top, by = col_word) %>%
      rename(
        word = !!rlang::sym(col_word),
        feature = !!rlang::sym(col_feature)
      ) %>%
      widyr::pairwise_cor(word,
                          feature,
                          sort = TRUE,
                          upper = FALSE)

    data_byx_corrs_top <-
      data_byx_corrs %>%
      mutate(rank = row_number(desc(correlation))) %>%
      filter(rank <= num_top_corrs)

    out <-
      data_byx_corrs_top %>%
      igraph::graph_from_data_frame(vertices = data_byword_cnt_top)
    out
  }

visualize_ngrams_byx_corrs <-
  function(data,
           col_label = "name",
           lab_feature = "data",
           seed = 42) {
    set.seed(42)
    viz <-
      data %>%
      ggraph::ggraph(layout = "fr") +
      # ggraph::geom_edge_link(aes(edge_alpha = correlation / 1.01, edge_width = correlation / 1.01)) +
      ggraph::geom_edge_link(edge_width = 1) +
      ggraph::geom_node_point(aes(size = n), fill = "grey80", shape = 21) +
      ggraph::geom_node_text(aes_string(label = col_label), repel = TRUE) +
      theme_void() +
      labs(title = paste0(
        "Network of Pairwise Word Correlations Within Single",
        lab_feature
      )) +
      theme(legend.position = "none")
  }

summarize_sent_byname <-
  function(sents,
           data,
           unigrams) {
    out <-
      data %>%
      inner_join(sents, by = "word") %>%
      count(status_id, sentiment) %>%
      tidyr::complete(sentiment, status_id, fill = list(n = 0)) %>%
      inner_join(unigrams, by = "status_id") %>%
      group_by(name, sentiment, total_words) %>%
      summarize(words = sum(n)) %>%
      ungroup()
    out
  }

compute_sentdiffs_poisson <- function(data) {
  out <-
    data %>%
    group_by(sentiment) %>%
    do(broom::tidy(poisson.test(.$words, .$total_words))) %>%
    ungroup()
  out
}

prepare_sents_diffs_poisson <- function(data, name_filter) {
  out <-
    data %>%
    filter(name_x == name_filter) %>%
    mutate(name_xy = paste0(name_x, " vs. ", name_y)) %>%
    mutate(sentiment = reorder(sentiment, estimate)) %>%
    mutate_at(vars(estimate, conf.low, conf.high), funs(. - 1)) %>%
    mutate(estimate_direction = if_else(estimate < 0, -1, 1)) %>%
    mutate(color_lab = if_else(estimate_direction == -1, name_filter, "other"))
  out

}

visualize_sents_diffs_poission <-
  function(data, colors, lab_lexicon = NULL) {

    viz <-
      data %>%
      ggplot(aes(x = sentiment, y = estimate, color = color_lab)) +
      geom_pointrange(aes(ymin = conf.low,
                          ymax = conf.high), size = 1) +
      geom_segment(aes(yend = 0, xend = sentiment),
                   linetype = "dotted",
                   size = 1) +
      scale_color_manual(values = colors) +
      geom_hline(aes(yintercept = 0), size = 2) +
      facet_wrap( ~ name_xy, scales = "free") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL, y = NULL) +
      labs(
        title = "Likelihood of Differences Among Sentiments",
        caption = paste0(
          lab_lexicon,
          " lexicon used for classification of words.\n",
          "Estimate and 95% confidence intervals of poisson test shown."
        )
      ) +
      temisc::theme_te_b_facet(caption_size = 12) +
      theme(legend.position = "bottom", legend.title = element_blank())
    viz
  }


create_sents_ratios_wide <-
  function(sents,
           unigrams_ratios,
           name_filter,
           num_top = 3,
           uniformize = FALSE) {

    # Do some pre-processing in order to identify if dummy groups will need
    # to be added later (in the case that there are not enough rows to plot).
    data_proc <-
      unigrams_ratios %>%
      inner_join(sents, by = "word") %>%
      filter(name_x == name_filter) %>%
      group_by(word, sentiment) %>%
      mutate(logratio = mean(logratio)) %>%
      ungroup() %>%
      mutate(sentiment = reorder(sentiment, -logratio),
             word = reorder(word, -logratio)) %>%
      group_by(name_xy, sentiment) %>%
      # group_by(sentiment) %>%
      mutate(rank = row_number(desc(logratio))) %>%
      filter(rank <= num_top) %>%
      ungroup()

    if(uniformize) {
      rank_minmax <-
        data_proc %>%
        group_by(name_xy, sentiment) %>%
        filter(rank == max(rank)) %>%
        ungroup() %>%
        filter(rank == min(rank)) %>%
        slice(1) %>%
        pull(rank)

      data_proc <-
        data_proc %>%
        group_by(name_xy, sentiment) %>%
        filter(rank <= rank_minmax) %>%
        ungroup()
    }

    out <-
      data_proc %>%
      mutate(logratio_direction = if_else(logratio < 0, TRUE, FALSE)) %>%
      mutate(color_lab = if_else(logratio_direction, name_filter, "other")) %>%
      mutate(name_xy = paste0(name_x, " vs. ", name_y))
    out
  }

visualize_sents_ratios <-
  function(data,
           sentiment_filter = c("positive", "negative"),
           colors,
           labs_colors) {
    sentiment_filter <- match.arg(sentiment_filter)
    viz <-
      data %>%
      filter(sentiment == sentiment_filter) %>%
      ggplot(aes(x = word, y = logratio, fill = color_lab)) +
      geom_bar(stat = "identity") +
      geom_hline(aes(yintercept = 0)) +
      scale_fill_manual(values = colors) +
      facet_wrap(~ name_xy, scales = "free") +
      labs(x = NULL, y = "Log Odds Ratio") +
      labs(title = "Most Significant Words Contributing to Sentiment Differences",
           subtitle = paste0(str_to_title(sentiment_filter), " Sentiments")) +
      temisc::theme_te_b_facet_dx() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "bottom", legend.title = element_blank())
    viz
  }
