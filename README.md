# Customize size and positions of a frame.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Build Status][build-badge]][build-link]

Allows for customization of Emacs frames, which include height and width of
new Emacs frames.  Options for new frames are those given to `make-frame`.
This is handy for those that rather resize your Emacs frames with a key binding
than using your mouse.


## Usage

This library uses much of the functinality of
the
[config-manage](https://github.com/plandes/buffer-manage/blob/master/config-manage.el) library.
It "learns" frame configurations, then restores them later on:

* Record frame positions with `M-x cframe-add-or-advance-setting`.
* Restore previous settings on start up with `cframe-restore`.
* Cycles through configuratinos with `cframe-add-or-advance-setting`.
* Pull up the [entries buffer] with `cframe-list`.

I use the following in my `~/.emacs` configuration file:
```elisp
(require 'cframe)

;; frame size settings based on screen dimentions
(global-set-key "\C-x9" 'cframe-restore)

;; doesn't clobber anything in shell, emacs lisp buffers (maybe others?)
(global-set-key "\C-\\" 'cframe-add-or-advance-setting)
```


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 - 2020 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[entries buffer]: https://github.com/plandes/buffer-manage#entries-mode

[melpa-link]: https://melpa.org/#/cframe
[melpa-stable-link]: https://stable.melpa.org/#/cframe
[melpa-badge]: https://melpa.org/packages/cframe-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/cframe-badge.svg
[build-badge]: https://github.com/plandes/cframe/workflows/CI/badge.svg
[build-link]: https://github.com/plandes/cframe/actions
