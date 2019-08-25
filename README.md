# Cheat
A small utility to register and create cheatsheets based on org-mode.

## Installation
`package-install RET cheat`

``` emacs-lisp
(use-package cheat
    :config
    (cheat/init))
```

## Usage

### Provided cheatsheets
- Asciidoc
- Org-mode

### Open a cheatsheet 
All cheatsheets are accessible through interactive functions their first list element preceded by the `cheat/` prefix. 

- `M-x RET cheat/adoc` opens asciidoc cheatsheet
- `M-x RET cheat/org` open org-mode cheatsheet

### Register a new cheatsheet

``` emacs-lisp
;; Register your cheatsheets in the cheat/sheets list
(add-to-list 'cheat/sheets
    '(custom "My custom title" "path/to/my/cheatsheet.org"))
;; Launch initialization of the utility, it will create the `cheat/custom` interactive function.
(cheat/init)

```
