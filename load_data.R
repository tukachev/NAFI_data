# Рейтинг основных барьеров для бизнеса в России
# Данные: Аналитический центр НАФИ | ноябрь 2021
# shorturl.at/aluAW

library(tidyverse)
library(readxl)
library(glue)
library(ragg)
library(here)
library(ggtext)

nafi <- read_excel(here("data", "nafi.xlsx"))

pr_levels <- rev(nafi$`Причина`)

nafi <-
  nafi %>% pivot_longer(cols = `Скорее не мешает, совершенно не мешает`:`Затрудняюсь ответить`,
                        names_to = "options") %>%
  mutate(
    options = factor(
      options,
      levels = c(
        "Затрудняюсь ответить",
        "Скорее не мешает, совершенно не мешает",
        "Ни то, ни другое",
        "Скорее мешает, очень сильно мешает"
      )
    ),
    `Причина` = factor(`Причина`,
                       levels = pr_levels)
  ) %>%
  mutate(y_p = value / 100,
         val_label = ifelse(value > 2, value, ""))


#насколько каждая проблема мешает ведению бизнеса в России
ggplot(nafi, aes(x = `Причина`, y = value, fill = options)) +
  geom_bar(position = "fill",
           color = "white",
           stat = "identity") +
  geom_text(
    aes(y = y_p, label = val_label),
    stat = "identity",
    color = "white",
    size = 5,
    position = position_stack(vjust = 0.5)
  ) +
  scale_x_discrete(labels = str_wrap(levels(nafi$Причина), 30), expand = expansion(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = expansion(0, 0)) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("#E36C33",
               "#67B4B4",
               "#416487",
               "gray70"),
    limits = c(
      "Скорее мешает, очень сильно мешает",
      "Ни то, ни другое",
      "Скорее не мешает, совершенно не мешает",
      "Затрудняюсь ответить"
    ),
    labels = c(
      "Скорее мешает,\nочень сильно мешает",
      "Ни то,\nни другое",
      "Скорее НЕ мешает,\nсовершенно НЕ мешает",
      "Затрудняюсь\nответить"
    )
  ) +
  guides(fill = guide_legend(byrow = FALSE, label.position = "right")) +
  labs(
    title = "Рейтинг основных барьеров для бизнеса в России",
    y = "",
    subtitle = "Предприниматели отмечают высокие налоги, нехватку <br>квалифицированных кадров и административные барьеры",
    caption = "Данные: Аналитический центр НАФИ | ноябрь 2021 | URL: horturl.at/aluAW\nВизуализация: Юрий Тукачев, 2022"
  ) +
  theme_minimal(base_size = 16, base_family = "OpenSans") +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(lineheight = 0.8, size = rel(0.8)),
    legend.justification = "right",
    legend.position = "top",
    legend.title = element_blank(),
    legend.box.margin = margin(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.text = element_text(
      size = 11,
      color = "gray40",
      face = "bold",
      lineheight = 0.8,
      margin = margin()
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(25, 25, 10, 25),
    axis.text.y = element_text(
      # color = c("gray30", "#416487", rep("gray30", 6), # "#E36C33", "gray30"),
      face = c(rep("plain", 7), rep("bold", 3))),
    plot.caption = element_text(
      color = "gray60",
      size = 12,
      hjust = 0
    ),
    plot.subtitle = element_markdown(
      hjust = 0,
      lineheight = 1.1,
      size = rel(1.1),
      family = "OpenSans",
      color = "gray30"
    ),
    plot.title = element_markdown(
      size = rel(1.3),
      family = "OpenSans",
      face = "bold"
    )
  )

ggsave(
  here("images", "nafi_data.png"),
  device = agg_png,
  width = 8,
  height = 8,
  dpi = 150
)
