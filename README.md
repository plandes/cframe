# Customize size and positions of a frame.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

Allows for customization of frame types, which includes height and width of new
Emacs frames.  Options included are all of those which are included from
`make-frame`.  This is handy for those that rather resize your Emacs frames
with a key binding rather than using your mouse.


## Usage

This library *learns* frame positions with `M-x cframe-add-or-advance-setting`
and then cycles through configuratinos with `cframe-add-or-advance-setting`.
You can pull up the [entries buffer] with `cframe-list`.


## Changelog

An extensive changelog is available [here](CHANGELOG.md).


## License

Copyright © 2017 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[entries buffer]: https://github.com/plandes/buffer-manage#entries-mode

[melpa-link]: https://melpa.org/#/frame-customize
[melpa-stable-link]: https://stable.melpa.org/#/frame-customize
[melpa-badge]: https://melpa.org/packages/frame-customize-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/frame-customize-badge.svg
[travis-link]: https://travis-ci.org/plandes/frame-customize
[travis-badge]: https://travis-ci.org/plandes/frame-customize.svg?branch=master
