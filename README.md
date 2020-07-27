# Structure and Interpretation of Computer Programs

A workspace for going through the textbook with implementations in Clojure.

## Usage

The project is managed by [Leiningen][1]. The root `.project` and `/src` directory
contains the work from each chapter in the book. The `/checkouts` directory contains additional
projects for each assignment associated with the [MIT course][2] instructed by the authors of the book.

## Development

Recommend using [cider][3] for emacs when developing locally.

Run tests:
```bash
lein test
```

[1]: https://leiningen.org
[2]: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/index.htm
[3]: https://github.com/clojure-emacs/cider
