# define a function that look beforehead if come across NA values
pre_val <- function(vector, location)
{
  if (is.na(vector[location]))
  {
    return(pre_val(vector, location-1))
  } else
  {
    return(vector[location])
  }
}


# define a function that look afterhead if come across NA values
post_val <- function(vector, location)
{
  if (!is.na(vector[location]))
  {
    return(vector[location])
  } else if (location < length(vector))
  {
    return(post_val(vector, location+1))
  } else
  {
    return(pre_val(vector, location-1))
  }
}