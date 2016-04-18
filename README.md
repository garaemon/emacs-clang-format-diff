# clang-format-diff

Visualize diff between current buffer and formatted code by clang-format

## Setup
```lisp
(require 'clang-format-diff)
(global-set-key "\M-[" 'clang-format-diff-view)
```

If you want to show diff side-by-side style,
use `'split-window-horizontally`.

```lisp
(custom-set-variables '(ediff-split-window-function
                         'split-window-horizontally))
```
