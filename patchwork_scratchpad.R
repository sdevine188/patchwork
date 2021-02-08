library(tidyverse)
library(patchwork)

# https://patchwork.data-imaginist.com/


# example code to make three mcp charts side by side - good for line charts to avoid too many lines on any one chart


# add color_bin and color
chart_data <- atlas %>%
        filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"),
               !(country %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs"))) %>%
        mutate(russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj =
                       case_when(is.nan(russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj) ~ 0,
                                 TRUE ~ russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj),
               estimated_russia_fuel_type_imports_as_share_of_tes =
                       russia_fuel_type_import_as_share_of_global_fuel_type_import_sum_adj *
                       fuel_type_imports_as_share_of_tes) %>%
        group_by(country, year) %>%
        mutate(estimated_russia_fossil_imports_as_share_of_tes = sum(estimated_russia_fuel_type_imports_as_share_of_tes)) %>%
        slice(1) %>%
        ungroup() %>%
        select(mcp_grouping, country, year, location_code, estimated_russia_fossil_imports_as_share_of_tes) %>%
        group_by(mcp_grouping, year) %>%
        mutate(values = mean(estimated_russia_fossil_imports_as_share_of_tes)) %>% slice(1) %>%
        ungroup() %>%
        mutate(mcp_grouping = case_when(mcp_grouping == "E&E Balkans" ~ "Balkans",
                                        mcp_grouping == "E&E Eurasia" ~ "Eurasia",
                                        mcp_grouping == "E&E graduates" ~ "Graduates",
                                        TRUE ~ mcp_grouping)) %>%
        mutate(color_bin = mcp_grouping,
               color = case_when(color_bin == "Balkans" ~ color_palette %>% slice(1) %>% pull(hex),
                                 color_bin == "Eurasia" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Graduates" ~ color_palette %>% slice(3) %>% pull(hex),
                                 color_bin == "CARs" ~ color_palette %>% slice(4) %>% pull(hex)),
               linetype_bin = mcp_grouping,
               linetype = case_when(TRUE ~ "solid"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# create linetype_list for to pass to scale_linetype_manual
chart_data_linetype_list <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype)
names(chart_data_linetype_list) <- chart_data %>% count(linetype_bin, linetype) %>% pull(linetype_bin)
chart_data_linetype_list


#////////////////////////



x <- chart_data %>%
        ggplot(data = ., aes(x = year, 
                             y = values, 
                             color = factor(color_bin, levels = c("Balkans", 
                                                                  "Eurasia", 
                                                                  "Graduates", 
                                                                  "CARs")),
                             linetype = factor(color_bin, levels = c("Balkans", 
                                                                     "Eurasia", 
                                                                     "Graduates", 
                                                                     "CARs")))) + 
        # geom_line(size = 1) + 
        # geom_point(size = 2) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Balkans"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Eurasia"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Graduates"),
                  mapping = aes(x = year + .65, y = values - .02, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "CARs"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        scale_color_manual(values = chart_data_color_list, guide = FALSE,
                           labels = c("Balkans", 
                                      "Eurasia", 
                                      "Graduates", 
                                      "CARs")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Balkans", 
                                         "Eurasia", 
                                         "Graduates", 
                                         "CARs")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.75, by = .25), limits = c(0, 1.8), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 3)) +
        labs(x = NULL, y = "Share", 
             title = "E&E regions",
             caption = NULL, color = "", linetype = "") +
        # coord_fixed(ratio = 8 / 1, clip = "off") +
        # annotate(geom = "rect", xmin = 2008.5, xmax = 2018.5, ymin = 0, ymax = 1.8, alpha = 0, color = "#DDDDDD") +
        geom_rect(data = chart_data,
                  mapping = aes(xmin = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5,
                                xmax = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5),
                  ymin = 0, ymax = 1.75, alpha = 0, color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .25, yend = .25), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .5, yend = .5), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .75, yend = .75), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1, yend = 1), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.25, yend = 1.25), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.5, yend = 1.5), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.75, yend = 1.75), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5,
                                   y = 0, yend = 0), color = "#333333", size = .5) +
        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 7, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                           margin = margin(t = 0, r = -5, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 7, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
x





# create segment_tbl
# segment_tbl <- tibble(x = rep(chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5, 
#                               times = length(seq(from = 0, to = 1.75, by = .25))),
#                       xend = rep(chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5,
#                                  times = length(seq(from = 0, to = 1.75, by = .25))),
#                       y = seq(from = 0, to = 1.75, by = .25), yend = seq(from = 0, to = 1.75, by = .25))


z <- chart_data %>%
        ggplot(data = ., aes(x = year, 
                             y = values, 
                             color = factor(color_bin, levels = c("Balkans", 
                                                                  "Eurasia", 
                                                                  "Graduates", 
                                                                  "CARs")),
                             linetype = factor(color_bin, levels = c("Balkans", 
                                                                     "Eurasia", 
                                                                     "Graduates", 
                                                                     "CARs")))) + 
        # geom_line(size = 1) + 
        # geom_point(size = 2) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Balkans"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Eurasia"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "Graduates"),
                  mapping = aes(x = year + .65, y = values - .02, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        geom_text(data = chart_data %>% filter(year == max(year), mcp_grouping == "CARs"),
                  mapping = aes(x = year + .65, y = values, label = color_bin),
                  fontface = "bold", hjust = 0, size = 1.75) +
        scale_color_manual(values = chart_data_color_list, guide = FALSE,
                           labels = c("Balkans", 
                                      "Eurasia", 
                                      "Graduates", 
                                      "CARs")) +
        scale_linetype_manual(values = chart_data_linetype_list, guide = FALSE,
                              labels = c("Balkans", 
                                         "Eurasia", 
                                         "Graduates", 
                                         "CARs")) +
        scale_y_continuous(breaks = seq(from = 0, to = 1.75, by = .25), limits = c(0, 1.8), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 3)) +
        labs(x = NULL, y = "Share", 
             title = "E&E regions",
             caption = NULL, color = "", linetype = "") +
        # coord_fixed(ratio = 8 / 1, clip = "off") +
        # annotate(geom = "rect", xmin = 2008.5, xmax = 2018.5, ymin = 0, ymax = 1.8, alpha = 0, color = "#DDDDDD") +
        geom_rect(data = chart_data,
                  mapping = aes(xmin = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5,
                                xmax = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5),
                  ymin = 0, ymax = 1.75, alpha = 0, color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .25, yend = .25), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .5, yend = .5), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = .75, yend = .75), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1, yend = 1), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.25, yend = 1.25), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.5, yend = 1.5), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .25,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .25,
                                   y = 1.75, yend = 1.75), color = "#DDDDDD", size = .25) +
        geom_segment(data = chart_data,
                     mapping = aes(x = chart_data %>% filter(year == min(year)) %>% slice(1) %>% pull(year) - .5,
                                   xend = chart_data %>% filter(year == max(year)) %>% slice(1) %>% pull(year) + .5,
                                   y = 0, yend = 0), color = "#333333", size = .5) +
        # geom_segment(data = segment_tbl, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 7, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                # axis.text.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                #                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                # axis.title.y = element_text(family = "Calibri", face = "plain", size = 9, color = "#333333", 
                #                             margin = margin(t = 0, r = 13, b = 0, l = 0)),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 7, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 
z

# # save chart as emf
# filename <- tempfile(fileext = ".emf")
# emf(file = filename)
# print(z)
# dev.off()
# 
# # add emf to word doc  
# read_docx() %>% 
#         body_add_img(src = filename, width = 6, height = 6) %>% 
#         print(target = "output/charts/x.docx")


plot_width <- 200
plot_height <- 10
layout <- c(
        area(t = 1, l = 1, b = 10, r = plot_width * 3), # spacer 1 to 1
        area(t = 11, l = 1, b = plot_height + 11, r = plot_width), # 2 to 4
        area(t = 11, l = plot_width + 1, b = plot_height + 11, r = plot_width * 2), # 6 to 8
        area(t = 11, l = (plot_width * 2) + 1, b = plot_height + 11, r = plot_width * 3), # 10 to 12
        area(t = plot_height + 12, l = 1, b = plot_height + 22, r = plot_width * 3) # spacer 13 to 13
)
plot(layout)


# chart_width <- 300
# plot_width <- 100
# plot_height <- 10
# layout <- c(
#         area(t = 1, l = 1, b = 10, r = chart_width), # spacer 1 to 1
#         area(t = 11, l = 1, b = plot_height + 11, r = plot_width), # 2 to 4
#         area(t = 11, l = chart_width / 3, b = plot_height + 11, r = (chart_width / 3) + plot_width), # 6 to 8
#         area(t = 11, l = (chart_width / 3) * 2, b = plot_height + 11, r = chart_width), # 10 to 12
#         area(t = plot_height + 12, l = 1, b = plot_height + 22, r = chart_width)) # spacer 13 to 13
# )
# plot(layout)

patchwork_test <- wrap_plots(plot_spacer(),
                             x,
                             # plot_spacer(),
                             z,
                             # plot_spacer(),
                             z, 
                             plot_spacer()) +
        plot_layout(design = layout) 
# patchwork_test


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(patchwork_test)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/patchwork_test.docx")


