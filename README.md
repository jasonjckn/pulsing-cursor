# pulsing-cursor
UI tweak cursor visual animation tweak: cursor fades in & out.

This is my first emacs module/contribution, feedback welcome.

Doom packages.el:
```
    (package! pulsing-cursor
        :recipe (:host github :repo "jasonjckn/pulsing-cursor"))
```

Emacs config:

```
    (use-package pulsing-cursor
        :config (pulsing-cursor-mode +1))

```
