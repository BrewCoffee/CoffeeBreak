
#' Take a coffe break
#'
#' @description For the program to work the Power & Sleep settings has to be adjusted.
#'
#'
#' Please go to:
#'
#' Settings -> Power & Sleep
#'
#' There, change each duration to 2 hours.
#'
#'
#' After running the program keep Microsoft Team as a front window.
#'
#' Now you are all set!
#'
#' @details
#'
#' @param duration minutes; choose number of minutes the break should last (greater than 5); default 30
#'
#' @export
#'
#' @examples coffee.break(duration = 15)
coffee.break <- function(duration = 30){
  start.time <- Sys.time()
  elapsed.time <- 0
  duration <- ifelse(duration < 5, 5, duration)
  print(paste0('Your ', duration, ' minute break starts now. Enjoy!'))
  while(elapsed.time <= duration * 60){
    x <- runif(1, 50, 1500)
    y <- runif(1, 50, 800)
    KeyboardSimulator::mouse.move(x, y, duration = runif(1, 0.01, 0.5), step_ratio = 0.05)
    Sys.sleep(runif(1, 0, 60))

    elapsed.time <- - as.numeric(difftime(start.time, Sys.time(), units = "secs"))
    print(paste0('Time: ', floor(elapsed.time/60), ' minutes ', floor(elapsed.time %% 60)
                 ,' seconds has elapsed.'))

    if(floor(elapsed.time/60) %% 14 == 13){
      KeyboardSimulator::mouse.move(20, 840, duration = runif(1, 0.01, 0.5), step_ratio = 0.05)
      KeyboardSimulator::mouse.click(button = "left", hold = FALSE)
      Sys.sleep(1)
      KeyboardSimulator::mouse.move(745, 380, duration = runif(1, 0.01, 0.5), step_ratio = 0.05)
      KeyboardSimulator::mouse.click(button = "left", hold = FALSE)
    }
  }
  print('Coffee break is over!')
}

# coffee.break(duration = 30)  # choose length of a break in minutes
