# Advent of Code 2024

For the fourth year in a row, I am going to use Advent of Code as a way to learn a new programming language. This year is Guile!

Previously, I did [2021 in Go](https://github.com/emilioziniades/advent-of-code-2021), [2022 in Rust](https://github.com/emilioziniades/advent-of-code-2022), and [2023 in Haskell](https://github.com/emilioziniades/advent-of-code-2023).

## Why Guile?

I've wanted to learn more about LISP, after being exposed to MIT Scheme when working through the SICP textbook.
But having the thought "I should do AOC in a LISP" was only the beginning.
After some research, I was quite overwhelmed with all the different languages in the LSP family.
I narrowed it down to four options and used a [decision matrix](https://youtu.be/c5QF2HjHLSE?t=2348) to decide, which you can find [here](https://github.com/emilioziniades/advent-of-code-2024/blob/main/.github/assets/decision-matrix.png).

In short, Guile seemed like a good trade-off between pedagogical simplicity and production-ready complexity.
It had all the basic thing I needed for the challenge, like a testing framework and an HTTP client, without the other stuff you would want if you were going to ship a multithreaded, performant application to production.
