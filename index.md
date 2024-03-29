
<!-- index.md is generated from index.Rmd. Please edit that file -->

# Welcome

This website is meant to be a resource for Econometrics students
learning R. It is both an R package and a series of articles that
explain how to use R for statistics and how to do it in a zen-like way.
It is intended as a complement to two other books: [Introduction to
Econometrics with R](https://www.econometrics-with-r.org/) by Christoph
Hanck, Martin Arnold, Alexander Gerber, and Martin Schmelzer; and [R for
Data Science](https://r4ds.had.co.nz/) by Hadley Wickham and Garrett
Grolemund.

<div class="card">

<div class="card-body">

*Introduction to Econometrics with R* follows Jeffery Wooldridge’s
textbook of the same name and shows how to implement selected
applications. Most intro econometrics classes will cover only portions
of Wooldridge and sometimes stray quite far from the material. Hence,
this website tries to be a concise complement to *Introduction to
Econometrics with R*, offering up practical tools for the different
things I have seen covered in metrics courses.

</div>

</div>

<br/>

<div class="card">

<div class="card-body">

*R for Data Science* is a broad overview of R and the tidyverse (a
collection of packages within R). It starts slow at a beginner level and
eventually covers a vast array of important concepts. It’s highly
recommended (perhaps required) reading for new R programmers. This
website partly assumes the background information contained in *R for
Data Science* and builds on it with specific recommendations for
econometrics and academic research.

</div>

</div>

<br/>

## Purpose of the Package

### 1. Metapackage

A metapackage is a package that contains other packages. When you
install this package, you will also install a curated list of packages
that I have created. I feel this is the fastest way to get you “set up”
with a complete environment where you can do your econometrics in R.

### 2. Documentation

The series of articles on this website are created from TA review
sessions where I covered important aspects of programming in R. There
are two sections:

-   **The Art of Metrics** contains articles on the basic estimation and
    inference techniques from an advanced undergraduate or
    first-semester graduate econometrics class. It focuses heavily on
    comparisons with Stata and aims to build a foundational set of
    metrics tools.
-   **The Zen of Programming** has a completely different focus from the
    previous section. What budding computer scientists (a.k.a. you)
    sometimes miss is that there is a whole world of non-technical
    skills that make programming more productive, fun, and
    *reproducible*. This has been dubbed the “Hidden Curriculum” of data
    science by Scott Cunningham. The Zen of Programming aims to take the
    “hidden” out of that title and teach a set of skills for project
    management that help you produce high-quality, shareable, and
    understandable academic research.

### 3. Code

There are a few things that are complicated to program, and in those
cases I try to write my own functions for you to use. You’re free to use
them in your econometrics homeworks, with the understanding that

-   You assume all liability if the function gives incorrect answers. I
    thoroughly test and validate the functions that I write, but I don’t
    have the desire or time to spend tinkering with them incessantly.
-   You will eventually have to figure it out yourself. Your employer
    won’t like you loading some grad student’s GitHub repo into a
    million-dollar software project, so you will eventually have to know
    how to implement the function.

If that sounds dandy to you, head on over to the [Get
Started](articles/metrics-in-r.html) page for installation instructions,
and then start reading some articles!
