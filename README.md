diagrams-qrcode
===============

This library is able to draw diagrams of QR codes using the
`diagrams` library.  In order to create the QR code itself, you
may use either the `haskell-qrencode` library or `qrcode`
library, both are supported.  In order to draw and save the
diagram to a file, we suggest using either the `diagrams-svg`
library (pure Haskell, fast, SVG output only) or the
`diagrams-cairo` library (requires Cairo, supports many different
output targets, including SVG, PNG, PDF, PS and directly onto a
GUI).

For example:

```
$ cabal install
$ cd examples/
$ echo 'https://github.com/meteficha/diagrams-qrcode/' | \
  runhaskell using-haskell-qrencode.hs > qrcode.svg
$ xdg-open qrcode.svg
```

![qrcode.png](https://raw.github.com/meteficha/diagrams-qrcode/master/examples/qrcode.png)
