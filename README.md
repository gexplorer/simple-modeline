# simple-modeline

[![MELPA](https://melpa.org/packages/simple-modeline-badge.svg)](https://melpa.org/#/simple-modeline)

A simple mode-line configuration for Emacs.

# About

* Simple design
* Lightweight
* Split layout with left and right align segments
* Customization option for modeline segments
* Several predefined segments:
  - Modified/readonly indicator
  - Buffer name
  - Position and region info
  - Version control status
  - Encoding
  - EOL
  - Minor modes list
  - Process info
  - Input method
  - Major mode

# Configuration

## Manual

```
(require 'simple-modeline)
(simple-modeline-mode)
```

## With use-package

```
(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))
```

# Preview

![Simple modeline](screenshot.png)
