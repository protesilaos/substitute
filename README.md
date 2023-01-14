# Substitute (substitute.el)

⚠️ WORK-IN-PROGRESS ⚠️

Efficiently replace targets in the buffer or context.

Sample configuration:

```elisp
(require 'substitute)

;; If you like visual feedback on matching target.  Default is nil.
(setq substitute-highlight t)

;; If you want a message reporting the matches that changed.  We don't
;; do it by default
(add-hook 'substitute-post-replace-hook #'substitute-report-operation)

;; We do not bind any keys.  This is just an idea.  The mnemonic is
;; that M-# (or M-S-3) is close to M-% (or M-S-5).
(let ((map global-map))
  (define-key map (kbd "M-# s") #'substitute-target-below-point)
  (define-key map (kbd "M-# r") #'substitute-target-above-point)
  (define-key map (kbd "M-# d") #'substitute-target-in-function)
  (define-key map (kbd "M-# b") #'substitute-target-in-buffer))
```

+ Package name (GNU ELPA): `substitute` (not available yet)
+ Git repo on SourceHut: <https://git.sr.ht/~protesilaos/substitute>
  - Mirrors:
    + GitHub: <https://github.com/protesilaos/substitute>
    + GitLab: <https://gitlab.com/protesilaos/substitute>
+ Mailing list: <https://lists.sr.ht/~protesilaos/general-issues>
+ Backronym: Some Utilities Built to Substitute Things Independent of
  Their Utterances, Thoroughly and Easily.
