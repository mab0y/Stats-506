library(nycflights13)
library(data.table)
flights <- as.data.table(flights)
airports <- as.data.table(airports)
planes <- as.data.table(planes)

departure <- flights[, .(mean_delay = mean(dep_delay, na.rm = TRUE),
                     med_delay = median(dep_delay, na.rm = TRUE),
                     faa = origin,
                     numflights = .N),by = origin] |>
                    _[numflights>10] |> 
                    merge(x=_, airports, by = 'faa', all.x=TRUE) |>
                    _[order(-mean_delay)] |>
                    _[, .(name, mean_delay, med_delay)] |>
                    print(x=_, n = Inf)

arrival <- flights[, .(mean_delay = mean(arr_delay, na.rm = TRUE),
                   med_delay = median(arr_delay, na.rm = TRUE),
                         faa = dest,
                         numflights = .N),by = dest] |>
                  _[numflights>10] |> 
                  merge(x=_, airports, by = 'faa', all.x=TRUE) |>
                  _[order(-mean_delay)] |>
                  _[, .(name, mean_delay, med_delay)] |>
                  print(x=_, n = Inf)


average_speed <- flights |>
                merge(x=_, planes, by = "tailnum") |>
                _[, .(model, distance, time = air_time/60)] |>
                _[, .(model, mph = distance/time)] |>
                _[, .(avgmph = mean(mph, na.rm = TRUE),
                 nflights = .N), by = model] |>
                _[order(-avgmph)] |>
                _[1] |>
                print(x=_)