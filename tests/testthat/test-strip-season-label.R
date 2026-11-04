# Pairs taken from Netflix's all-weeks-global.xlsx (season_title -> show_title), 4 Nov 2026
test_that("strip_season_label() splits full TV names the way the Excel does", {
  cases <- c(
    "Lupin: Part 4" = "Lupin",
    "The Diplomat: Season 4" = "The Diplomat",
    "Below: Limited Series" = "Below",
    "Doctor X: Mafia in White: Limited Series" = "Doctor X: Mafia in White",
    "Love Is Blind: S11: Boston" = "Love Is Blind",
    "The Uncanny Counter: Season 2: Counter Punch" = "The Uncanny Counter",
    "Raw: October 26, 2026" = "Raw",
    "Raw: 2026 - October 26, 2026" = "Raw",
    "Barbarians: II" = "Barbarians",
    "The Great British Baking Show: Collection 13" = "The Great British Baking Show",
    "Weak Hero: Class 2" = "Weak Hero"
  )
  expect_identical(strip_season_label(names(cases)), unname(cases))
})

test_that("one-off specials without a season label stay whole", {
  specials <- c("Jo Koy: Blue in the Face", "Instadocs: Jane Doe v. Cornell", "Wednesday")
  expect_identical(strip_season_label(specials), specials)
})
