# [ELPA Packages](https://elpa.emacs-china.org/packages/)

**ELPA Packages** is a directory of Emacs packages sourced from:

- [GNU ELPA](https://elpa.gnu.org/)
- [MELPA](https://melpa.org/)
- [MELPA Stable](https://stable.melpa.org/)
- [Marmalade](https://stable.melpa.org/)
- [Org ELPA](http://orgmode.org/elpa.html)
- [Sunrise Commander ELPA](http://joseito.republika.pl/sunrise-commander/)
- [User42 ELPA](http://user42.tuxfamily.org/elpa/index.html)

## API

The API is updated once a day.

| ELPA                   | API                                                          |
| ---------------------- | ------------------------------------------------------------ |
| GNU ELPA               | https://elpa.emacs-china.org/packages/gnu.json               |
| MELPA                  | https://elpa.emacs-china.org/packages/melpa.json             |
| MELPA Stable           | https://elpa.emacs-china.org/packages/melpa-stable.json      |
| Marmalade              | https://elpa.emacs-china.org/packages/marmalade.json         |
| Org ELPA               | https://elpa.emacs-china.org/packages/org.json               |
| Sunrise Commander ELPA | https://elpa.emacs-china.org/packages/sunrise-commander.json |
| User42 ELPA            | https://elpa.emacs-china.org/packages/user42.json            |
| **All of above**       | https://elpa.emacs-china.org/packages/all.json               |

## Build

Make sure that you have [Nanoc](http://nanoc.ws/) gem installed, then simply run

    nanoc

from shell to build the site.

## Deploy

[![Build Status](https://travis-ci.org/emacs-china/packages.svg?branch=master)](https://travis-ci.org/emacs-china/packages)

Travis-CI builds and deploys the site daily by
using [Cron Jobs - Travis CI](https://docs.travis-ci.com/user/cron-jobs/). And
update on the master branch also triggers the update.

Thanks Travis-CI.
