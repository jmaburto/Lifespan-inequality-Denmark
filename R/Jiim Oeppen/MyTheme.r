# Set a ggplot2 theme for a Lexis surface plot, possibly faceted
MyTheme <- function (base_size = 12, base_family = "") 
{
	theme_grey(base_size = base_size, base_family = base_family) %+replace% 
			theme(plot.title = element_text(size=rel(2), face="bold"),
					axis.title.y = element_text(size=rel(1.5), face="bold", angle=90),
					axis.title.x = element_text(size=rel(1.5), face="bold"),		
					axis.text.x = element_text(size=rel(1.3), vjust=1),
					axis.text.y = element_text(size=rel(1.3), hjust=1),
					strip.text.x = element_text(size=rel(1.5), face="bold",
					                            #  avoids thin strip side-effect of ggplot2 v2.0
					                            margin = margin(10, 0, 10, 0)),
					strip.text.y = element_text(size=rel(1.5), face="bold", angle=270),					
					legend.title = element_text(size=rel(1.5),
							face="bold",hjust=0),		
					legend.text = element_text(size=rel(1.2)))
}

# Old version pre 0.9.2
#MyTheme <- function(base_size = 10) {
#	structure(list(
#		plot.title = theme_text(size=base_size * 2, face="bold"),
#		axis.title.y = theme_text(size=base_size * 1.5, face="bold", angle=90),
#		axis.title.x = theme_text(size=base_size * 1.5, face="bold"),		
#		axis.text.x = theme_text(size=base_size * 1.3, vjust=1),
#		axis.text.y = theme_text(size=base_size * 1.3, hjust=1),
#		strip.text.x = theme_text(size=base_size * 1.5, face="bold"),
#		legend.title = theme_text(size=base_size * 1.5,
#				face="bold",hjust=0),		
#		legend.text = theme_text(size=base_size * 1.2)		
#	), class = "options")
#}

# Old version pre 0.9.2
#theme_min = function (size=10, font=NA, face='plain', 
#    panelColor=backgroundColor, axisColor='#999999', 
#    gridColor=gridLinesColor, textColor='black') 
#{
#    theme_text = function(...)
#        ggplot2::theme_text(family=font, face=face, colour=textColor, 
#            size=size, ...)
#
#opts(
#    axis.text.x = theme_text(),
#    axis.text.y = theme_text(),
#    axis.line = theme_blank(),
#    axis.ticks = theme_segment(colour=axisColor, size=0.25),
#    panel.border = theme_rect(colour=backgroundColor),
#    legend.background = theme_blank(),
#    legend.key = theme_blank(),
#    legend.key.size = unit(1.5, 'lines'),
#    legend.text = theme_text(hjust=0),
#    legend.title = theme_text(hjust=0),
#    panel.background = theme_rect(fill=panelColor, colour=NA),
#    panel.grid.major = theme_line(colour=gridColor, size=0.33),
#    panel.grid.minor = theme_blank(),
#    strip.background = theme_rect(fill=NA, colour=NA),
#    strip.text.x = theme_text(hjust=0),
#    strip.text.y = theme_text(angle=-90),
#    plot.title = theme_text(hjust=0),
#    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
#}
