# https://stackoverflow.com/questions/31782580/how-can-i-play-birthday-music-using-r

library("dplyr")
library("audio")

notes  <- data.frame(note = c(LETTERS[1:7], "wr"), value = c(0,2,3,5,7,8,10,0), stringsAsFactors = FALSE)
notes2 <- data.frame(note = c(paste0(LETTERS[1:7], LETTERS[1:7])), value = c(0,2,3,5,7,8,10)+12, stringsAsFactors = FALSE)
notes3 <- data.frame(note = c(paste0(LETTERS[1:7], LETTERS[1:7], LETTERS[1:7])), value = c(0,2,3,5,7,8,10)+24, stringsAsFactors = FALSE)

notes  <- rbind(notes, notes2, notes3)

measure1 <- data.frame(pitch    = strsplit("E BB DD EE FF FF EE DD CC CC BB wr CC BB wr", " ")[[1]],
                       duration =         c(1, 1, 1, 1, 1, 1, .5, .5, 1, 2, 1, 1, 2, 1, 1), 
                       stringsAsFactors = FALSE)

measure2 <- data.frame(pitch    = strsplit("E BB DD EE FF FF EE DD CC FF EE wr FF EE wr wr", " ")[[1]],
                       duration =         c(1, 1, 1, 1, 1, 1, 1, .5, .5, 1, 2, 1, 1, 2, 1, 1), 
                       stringsAsFactors = FALSE)

measure3 <- data.frame(pitch    = strsplit("A A CC EE EE DD wr wr EE GG AAA BBB AAA wr", " ")[[1]],
                       duration =         c(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1), 
                       stringsAsFactors = FALSE)

measure4 <- data.frame(pitch    = strsplit("GG GG wr DDD BBB wr wr wr GG FF Eb FF GG FF Eb FF", " ")[[1]],
                       duration =         c(1, 1, 1, 1, 1, 1, 1, 1,    1, 1, 1, 1, 1, 1, 1, 1), 
                       stringsAsFactors = FALSE)

measure5 <- data.frame(pitch    = strsplit("GG GG wr DDD BBB wr wr wr", " ")[[1]],
                       duration =         c(1, 1, 1, 1, 1, 1, 1, 1    ), 
                       stringsAsFactors = FALSE)

octave <- 4

calcNoteValue <- function(note.value, pitch) { note.value + grepl("#", pitch) - grepl("b", pitch) + octave * 12 + 12 * (note.value < 3) }
calcNoteFreq  <- function(note.value) { 2 ^ ((note.value - 60) / 12) * 440 }

ducktales <- measure1 %>% rbind(measure2, measure3, measure4, measure5) %>%
  left_join(notes, by = c("pitch" = "note")) %>%
  mutate(octave = 4) %>%
  mutate(note = value) %>%
  mutate(note = calcNoteValue(value, pitch)-12) %>%
  mutate(freq = ceiling(calcNoteFreq(note))) %>%
  mutate_at(c("note", "freq"), funs(ifelse(pitch == "wr", 0, .)))
  
tempo <- 300
sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

mapply(make_sine, ducktales$freq, ducktales$duration) %>% do.call("c", .) %>% play
