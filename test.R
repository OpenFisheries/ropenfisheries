
library(ggplot2)
library(ropenfisheries)

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(title = 'Branding your ggplot Graphs',
       subtitle = 'Simple tweaks you can use to boost the impact of your graphs today',
       x = 'This axis title intentionally left blank',
       y = 'This axis title intentionally left blank')

p


finalise_plot(
  p,
  # source_name="openfisheries.org",
  # save_filepath = 'tester.png',
  logo_image_path = file.path(system.file("extdata", package = 'ropenfisheries'), "of_footer_logo.png")
)
