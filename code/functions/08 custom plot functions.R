#######################################################
# Counterfactual function
#######################################################

plot_counterfactualTT <- function (x, plus_t = 5, facet.scales = "free", title = NULL, 
                                   zero_line = FALSE) 
{
  #original data
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[, !colnames(indicators) %in% names(df)]
  df <- cbind(df, indicators)
  df_ident_fesis <- identify_indicator_timings(df)$fesis
  fitted <- as.numeric(x$isatpanel.result$mean.fit)
  max_times <- aggregate(x$estimateddata$time, by = list(x$estimateddata$id),
                         FUN = function(x) {
                           max(x, na.rm = TRUE)
                         })
  
  #structural breaks
  names(max_times) <- c("id", "maxtime")
  df_ident <- break_uncertainty(x)
  df_ident <- merge(df_ident, max_times, by = "id") %>% 
    filter(coef<0) #remove positive breaks
  df_ident$origtime <- df_ident$time
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA
  df_ident_overall <- rbind(df_ident_start, df_ident)
  
  #effects
  for (i in 1:plus_t) {
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, 
                            intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, 
                                             df_ident_overall$time), ]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall), ]
  
  effects <- merge(x$estimateddata, df_ident_overall, by = c("id", "time"), all.x = TRUE)
  effects <- merge(effects, data.frame(x$estimateddata[, c("id", "time")], fitted), by = c("id", "time"))
  effects$cf <- (effects$coef * (-1)) + effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * 
                       (-1)) + effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * 
                       (-1)) + effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * 
                         (-1)) + effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * 
                         (-1)) + effects$fitted
  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci
  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]
  sub_title <- NULL
  
  #datasets
  df <- df %>% 
    filter(id == "Austria")
  effects <- effects %>% 
    filter(id == "Austria")
  df_ident_fesis <- df_ident_fesis %>% 
    filter(id == "Austria")
  fitted <- fitted[1:length(df$id)]
  
  #plots
  g <- ggplot(df, aes_(x = ~time, y = ~fitted, group = ~id))
  
  if (zero_line) {g = g + geom_hline(aes(yintercept = 0))}
  
  plotoutput <- g +
    geom_line(aes_(y = ~y, color = "black"),
              size = 0.7) +
    geom_rect(data = effects,
              aes_(xmin = ~start_rect,
                   xmax = ~end_rect,
                   ymin = -Inf,
                   ymax = Inf,
                   group = ~name),
              fill = "grey",
              alpha = 0.1,
              na.rm = TRUE) +
    geom_line(aes(color = "blue"),
              linetype = 1,
              size = 0.5) +
    geom_vline(data = df_ident_fesis,
               aes_(xintercept = ~time,color = "red")) +
    geom_ribbon(data = effects,
                aes_(ymin = ~cf_lwr,
                     ymax = ~cf_upr,
                     fill = "red",
                     group = ~name),
                alpha = 0.5,
                na.rm = FALSE) +
    geom_line(data = effects,
              aes_(y = ~cf,
                   color = "red",
                   group = ~name),
              na.rm = TRUE) +
    facet_wrap("id",
               scales = facet.scales) +
    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red", "darkgreen", "orange"),
                         labels = c("Actual", "Fitted","IIS", "SIS", "Counterfactual", "CFESIS", "CSIS"),
                         guide = "legend") +
    scale_linetype(name = "Variable") +
    guides(fill = "none") +
    theme(strip.background = element_blank(),
          legend.key = element_rect(fill = NA),
          panel.border = element_rect(colour = "grey",
                                      fill = NA),
          panel.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = title,
         subtitle = sub_title,
         y = NULL,
         x = NULL)
  return(plotoutput)
}




# Lime production --------------------------------------------------------------

# #setup
# x <- lime
# plus_t = 5
# facet.scales = "free"
# title = "Lime production"
# zero_line = FALSE
# 
# #original data
# df <- x$estimateddata
# indicators <- x$isatpanel.result$aux$mX
# indicators <- indicators[, !colnames(indicators) %in% names(df)]
# df <- cbind(df, indicators)
# df_ident_fesis <- identify_indicator_timings(df)$fesis
# fitted <- as.numeric(x$isatpanel.result$mean.fit)
# max_times <- aggregate(x$estimateddata$time, by = list(x$estimateddata$id),
#                        FUN = function(x) {
#                          max(x, na.rm = TRUE)
#                        })
# 
# #structural breaks
# names(max_times) <- c("id", "maxtime")
# df_ident <- break_uncertainty(x)
# df_ident <- merge(df_ident, max_times, by = "id") %>%
#   filter(coef<0) #remove positive breaks
# df_ident$origtime <- df_ident$time
# df_ident_start <- df_ident
# df_ident_start$time <- df_ident_start$time - 1
# df_ident_start$coef <- 0
# df_ident_start$sd <- 0
# df_ident_start$tci <- NA
# df_ident_overall <- rbind(df_ident_start, df_ident)
# 
# #effects
# for (i in 1:plus_t) {
#   intermed <- df_ident
#   intermed$time <- intermed$time + i
#   intermed$time <- ifelse(intermed$time > intermed$maxtime,
#                           intermed$maxtime, intermed$time)
#   df_ident_overall <- rbind(df_ident_overall, intermed)
# }
# df_ident_overall <- df_ident_overall[order(df_ident_overall$name,
#                                            df_ident_overall$time), ]
# df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall), ]
# 
# effects <- merge(x$estimateddata, df_ident_overall, by = c("id", "time"), all.x = TRUE)
# effects <- merge(effects, data.frame(x$estimateddata[, c("id", "time")], fitted), by = c("id", "time"))
# effects$cf <- (effects$coef * (-1)) + effects$fitted
# effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) *
#                      (-1)) + effects$fitted
# effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) *
#                      (-1)) + effects$fitted
# effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) *
#                        (-1)) + effects$fitted
# effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) *
#                        (-1)) + effects$fitted
# effects$start_rect <- effects$origtime - effects$tci
# effects$end_rect <- effects$origtime + effects$tci
# effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
# effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]
# sub_title <- NULL
# 
# #datasets
# df <- df %>%
#   filter(id == "Austria")
# effects <- effects %>%
#   filter(id == "Austria")
# df_ident_fesis <- df_ident_fesis %>%
#   filter(name == "fesisAustria.2006")
# fitted <- fitted[1:length(df$id)]
# 
# #plots
# g <- ggplot(df, aes_(x = ~time, y = ~fitted, group = ~id))
# 
# if (zero_line) {g = g + geom_hline(aes(yintercept = 0))}
# 
# plotoutput <- g +
#   geom_line(aes_(y = ~y, color = "black"),
#             size = 0.7) +
#   geom_rect(data = effects,
#             aes_(xmin = ~start_rect,
#                  xmax = ~end_rect,
#                  ymin = -Inf,
#                  ymax = Inf,
#                  group = ~name),
#             fill = "grey",
#             alpha = 0.1,
#             na.rm = TRUE) +
#   geom_line(aes(color = "blue"),
#             linetype = 1,
#             size = 0.5) +
#   geom_vline(data = df_ident_fesis,
#              aes_(xintercept = ~time,color = "red")) +
#   geom_ribbon(data = effects,
#               aes_(ymin = ~cf_lwr,
#                    ymax = ~cf_upr,
#                    fill = "red",
#                    group = ~name),
#               alpha = 0.5,
#               na.rm = FALSE) +
#   geom_line(data = effects,
#             aes_(y = ~cf,
#                  color = "red",
#                  group = ~name),
#             na.rm = TRUE) +
#   facet_wrap("id",
#              scales = facet.scales) +
#   scale_color_identity(name = NULL,
#                        breaks = c("black", "blue", "grey", "purple", "red", "darkgreen", "orange"),
#                        labels = c("Actual", "Fitted","IIS", "SIS", "Counterfactual", "CFESIS", "CSIS"),
#                        guide = "legend") +
#   scale_linetype(name = "Variable") +
#   guides(fill = "none") +
#   theme(strip.background = element_blank(),
#         legend.key = element_rect(fill = NA),
#         panel.border = element_rect(colour = "grey",
#                                     fill = NA),
#         panel.background = element_blank(),
#         strip.text.x = element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(title = title,
#        subtitle = sub_title,
#        y = NULL,
#        x = NULL)



#######################################################
# Model fit function
#######################################################

plot_modelfit <- function (x, plus_t = 5, facet.scales = "free", title = NULL, 
                           zero_line = FALSE) 
{
  #original data
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[, !colnames(indicators) %in% names(df)]
  df <- cbind(df, indicators)
  df_ident_fesis <- identify_indicator_timings(df)$fesis
  fitted <- as.numeric(x$isatpanel.result$mean.fit)
  max_times <- aggregate(x$estimateddata$time, by = list(x$estimateddata$id),
                         FUN = function(x) {
                           max(x, na.rm = TRUE)
                         })
  
  #structural breaks
  names(max_times) <- c("id", "maxtime")
  df_ident <- break_uncertainty(x)
  df_ident <- merge(df_ident, max_times, by = "id") %>% 
    filter(coef<0) #remove positive breaks
  df_ident$origtime <- df_ident$time
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA
  df_ident_overall <- rbind(df_ident_start, df_ident)
  
  #effects
  for (i in 1:plus_t) {
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, 
                            intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, 
                                             df_ident_overall$time), ]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall), ]
  
  effects <- merge(x$estimateddata, df_ident_overall, by = c("id", "time"), all.x = TRUE)
  effects <- merge(effects, data.frame(x$estimateddata[, c("id", "time")], fitted), by = c("id", "time"))
  effects$cf <- (effects$coef * (-1)) + effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * 
                       (-1)) + effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * 
                       (-1)) + effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * 
                         (-1)) + effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * 
                         (-1)) + effects$fitted
  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci
  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]
  sub_title <- NULL
  
  #datasets
  df <- df %>% 
    filter(id == "Austria")
  effects <- effects %>% 
    filter(id == "Austria")
  df_ident_fesis <- df_ident_fesis %>% 
    filter(id == "this won't produce anything")
  fitted <- fitted[1:length(df$id)]
  
  #plots
  g <- ggplot(df, aes_(x = ~time, y = ~fitted, group = ~id))
  
  if (zero_line) {g = g + geom_hline(aes(yintercept = 0))}
  
  plotoutput <- g +
    geom_line(aes_(y = ~y, color = "black"),
              size = 0.7) +
    geom_rect(data = effects,
              aes_(xmin = ~start_rect,
                   xmax = ~end_rect,
                   ymin = -Inf,
                   ymax = Inf,
                   group = ~name),
              fill = "grey",
              alpha = 0.1,
              na.rm = TRUE) +
    geom_line(aes(color = "blue"),
              linetype = 1,
              size = 0.5) +
    geom_vline(data = df_ident_fesis,
               aes_(xintercept = ~time,color = "red")) +
    geom_ribbon(data = effects,
                aes_(ymin = ~cf_lwr,
                     ymax = ~cf_upr,
                     fill = "red",
                     group = ~name),
                alpha = 0.5,
                na.rm = FALSE) +
    geom_line(data = effects,
              aes_(y = ~cf,
                   color = "red",
                   group = ~name),
              na.rm = TRUE) +
    facet_wrap("id",
               scales = facet.scales) +
    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red", "darkgreen", "orange"),
                         labels = c("Actual", "Fitted","IIS", "SIS", "Counterfactual", "CFESIS", "CSIS"),
                         guide = "legend") +
    scale_linetype(name = "Variable") +
    guides(fill = "none") +
    theme(strip.background = element_blank(),
          legend.key = element_rect(fill = NA),
          panel.border = element_rect(colour = "grey",
                                      fill = NA),
          panel.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = title,
         subtitle = sub_title,
         y = NULL,
         x = NULL)
}

