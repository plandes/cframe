# Customize size and positions of a frame.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Build Status][build-badge]][build-link]

Emacs frames position and dimensions are "learned" from user provided frame
configurations, then restores them later.  This is for users that prefer to
resize Emacs frames with a key binding rather than the mouse.

To use this library:

1. Position the frame how you like it.
2. Record the frame with `M-x cframe-add-or-advance-setting`.
3. Restore previous settings on start up with `cframe-restore`.
4. Cycle through configurations with `cframe-add-or-advance-setting`.

You can get a list of the configuration and which is currently used with
`cframe-list`.


## Recommended Configuration

Recommended `~/.emacs` configuration to restore the frame on start up:
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

Copyright © 2017 - 2025 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[entries buffer]: https://github.com/plandes/buffer-manage#entries-mode

[melpa-link]: https://melpa.org/#/cframe
[melpa-stable-link]: https://stable.melpa.org/#/cframe
[melpa-badge]: https://melpa.org/packages/cframe-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/cframe-badge.svg
[build-badge]: https://github.com/plandes/cframe/workflows/CI/badge.svg
[build-link]: https://github.com/plandes/cframe/actions

[config-manage]: https://github.com/plandes/buffer-manage/blob/master/config-manage.el
