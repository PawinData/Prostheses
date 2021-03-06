library(plotly)

load("prostheses.RData")

col_gender <- c(Male = "darkolivegreen", Female = "darkorange")
fig <- plot_ly(DATA, x=~FU.Years, y=~BMI, z=~nMTPM, color=~Sex, linetype=~Type,
               type="scatter3d", mode="lines+markers",
               line = list(width=6),
               marker = list(size=3.5, cmin=-20, cmax=50),
               colors = col_gender,
               transforms = list(list(type="groupby", groups=~ID))
              )

fnt <- list(family="Arial", size=18, color="black")
Xlabel <- list(title="Number of Follow-up Years", titlefont=fnt)
Ylabel <- list(title="BMI", titlefont=fnt)
Zlabel <- list(title="MTPM / mm", titlefont=fnt)
fig <- fig %>% layout(title="3D Data Visualization", 
                      scene=list(xaxis=Xlabel, yaxis=Ylabel, zaxis=Zlabel)
                     )


fig