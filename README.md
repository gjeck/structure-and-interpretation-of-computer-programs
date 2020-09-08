# Structure and Interpretation of Computer Programs

A workspace for going through the textbook with implementations in [Clojure][1] and [Racket][2].

## Usage

The Clojure project is managed by [Leiningen][3]. The root `.project` and `/src` directory
contains the Clojure work from each chapter in the book. The `/checkouts` directory contains additional
projects for each assignment associated with the [MIT course][4] instructed by the authors of the book.
Racket content is in the `/racketSrc` directory.

## Development

Recommend using [cider][5] for emacs when developing Clojure locally and using [racket-mode][6] for Racket.

Run Clojure tests:
```bash
lein test
```

Run Racket tests:
```bash
raco test racketSrc
```

[1]: https://clojure.org
[2]: https://racket-lang.org
[3]: https://leiningen.org
[4]: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/index.htm
[5]: https://github.com/clojure-emacs/cider
[6]: https://racket-mode.com
