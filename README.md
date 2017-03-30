# meson-mode.el --- Major mode for the Meson build system files

Copyright (C) 2017  Michal Sojka

* Author: Michal Sojka <sojkam1@fel.cvut.cz>
* Version: 0.1
* Keywords: languages, tools
* URL: https://github.com/wentasah/meson-mode
* Package-Requires: ((emacs "24.3"))

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

This is a major mode for Meson build system files. Syntax
highlighting works reliably. Indentation works too, but there are
probably cases, where it breaks. Simple completion is supported via
`` `completion-at-point' ``. To start completion, use either <C-M-i> or
install completion frameworks such as `` `company' ``. To enable
`` `company' `` add the following to your .emacs:

    (add-hook 'meson-mode-hook 'company-mode)
