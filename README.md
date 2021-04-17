# emacs-newrelic

An unofficial New Relic client package. 

Provides a set of commands for interacting with New Relic through [the official GraphQL API](https://api.newrelic.com/graphiql).

## Installation

This package is not yet published.

Please clone the repository, add it to your load path and require the package:

```elisp
(add-to-list 'load-path' "~/emacs-newrelic")
(require 'newrelic)
```

## Configuration

1. Generate your API key on [this page](https://one.newrelic.com/launcher/api-keys-ui.api-keys-launcher)
2. Configure the package to use your API key with

```elisp
(setq newrelic-api-key "<YOUR-API-KEY>")
```

## Usage

This package exposes a set of useful commands for interacting with your New Relic account

- `newrelic-dashboards` gives a quick access to your dashboards
- `newrelic-nrql-eval` opens a prompt allowing to write and execute NRQL queries, renders chart picture
- `newrelic-nrql-eval-line` executes NRQL from the current line in the buffer, renders chart picture
- `newrelic-nrql-eval-region` executes NRQL from the selected region, renders chart picture

## Development

Add the path of the cloned repository to the `'load-path` variable to make local requires work.


```elisp
(add-to-list 'load-path' "~/emacs-newrelic")
```

_Note: this can be done either in your configuration, or the repl `ielm` <kbd>M-x ielm</kbd>, or by typing the following line after running <kbd>M-x eval-expression</kbd>._
