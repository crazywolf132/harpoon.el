<div align="center">

# Harpoon.el

**Getting you where you want with the fewest keystrokes.**

[![Emacs](https://img.shields.io/badge/Emacs-25.1%2B-blue.svg?style=for-the-badge&logo=gnu-emacs&logoColor=white)](https://www.gnu.org/software/emacs/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg?style=for-the-badge)](https://opensource.org/licenses/MIT)

<img alt="Harpoon" height="280" src="assets/harpoon-icon.png" />

</div>

---

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
  - [For Emacs](#for-emacs)
  - [For Spacemacs](#for-spacemacs)
  - [For Doom Emacs](#for-doom-emacs)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Configuration](#configuration)
- [Credits](#credits)
- [License](#license)

---

## Introduction

**Harpoon.el** is an Emacs port of the popular Neovim plugin [Harpoon](https://github.com/ThePrimeagen/harpoon/tree/harpoon2). It provides a simple and efficient way to navigate between frequently used files and buffers, reducing the keystrokes required and enhancing your productivity.

Harpoon.el allows you to mark files and buffers, access them quickly through a minimal interface, and execute custom actions. It's particularly useful when working on projects where you need to switch between a set of files regularly.

---

## Features

- **Quick File Navigation**: Mark files and switch between them with minimal keystrokes.
- **Customizable Lists**: Manage multiple lists of files or items, tailored to your workflow.
- **Simple Interface**: Toggle a quick menu to view and select from your marked items.
- **Extensibility**: Define custom behaviors and actions for your lists.
- **Persistence**: Harpoon.el remembers your marks between sessions.
- **Integration**: Works seamlessly with popular Emacs distributions like Spacemacs and Doom Emacs.

---

## Installation

### For Emacs

You can install Harpoon.el manually or through package managers like [MELPA](https://melpa.org/) (once it's available).

#### Manual Installation

1. Clone the repository:

   ```shell
   git clone https://github.com/crazywolf132/harpoon.el.git
   ```

2. Add the directory to your Emacs `load-path`:

   ```elisp
   (add-to-list 'load-path "/path/to/harpoon.el/")
   ```

3. Require Harpoon in your Emacs configuration:

   ```elisp
   (require 'harpoon)
   ```

### For Spacemacs

Add Harpoon.el to your `dotspacemacs-additional-packages` list in your `.spacemacs` file:

```elisp
dotspacemacs-additional-packages '(
  harpoon
)
```

Then, require Harpoon in your user configuration:

```elisp
(defun dotspacemacs/user-config ()
  (require 'harpoon)
  (harpoon-setup))
```

### For Doom Emacs

Add Harpoon.el to your `packages.el`:

```elisp
(package! harpoon
  :recipe (:host github :repo "crazywolf132/harpoon.el"))
```

Run `doom sync` in your terminal, then restart Emacs.

In your `config.el`, require and set up Harpoon:

```elisp
(use-package harpoon
  :config
  (harpoon-setup))
```

---

## Getting Started

### Basic Setup

First, ensure that Harpoon is properly installed and required in your Emacs configuration. Then, initialize Harpoon by calling `harpoon-setup`:

```elisp
(require 'harpoon)
(harpoon-setup)
```

### Key Bindings

Set up key bindings to add files, toggle the quick menu, and navigate between your marked files:

```elisp
(global-set-key (kbd "C-c h a") 'harpoon-add-file)
(global-set-key (kbd "C-c h t") 'harpoon-toggle-quick-menu)

;; Navigate to files by index
(global-set-key (kbd "C-c h 1") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1)))
(global-set-key (kbd "C-c h 2") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2)))
(global-set-key (kbd "C-c h 3") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3)))
(global-set-key (kbd "C-c h 4") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4)))
```

---

## Usage

### Adding Files

- **Add Current File to Harpoon**: Press `C-c h a` to add the current file to your Harpoon list.

### Navigating Files

- **Toggle Quick Menu**: Press `C-c h t` to open or close the Harpoon quick menu.
- **Navigate to Marked Files**: Use `C-c h 1`, `C-c h 2`, etc., to jump to the files you've marked.
- **Quick Navigation**: Within the quick menu, select a file to jump to it.

### Managing Marks

- **Remove Marks**: Within the quick menu, you can remove files from your list by deleting the corresponding line and saving.
- **Reorder Marks**: Rearrange the lines in the quick menu to reorder your marks.

---

## Configuration

Harpoon.el is highly customizable. You can adjust settings and define custom behaviors for your lists.

### Settings

To configure Harpoon, you can pass an optional configuration to `harpoon-setup`:

```elisp
(harpoon-setup
 '(:settings (:save-on-toggle t
               :sync-on-ui-close t
               :key (lambda () (projectile-project-root)))))
```

#### Available Settings

- `:save-on-toggle` (boolean): Save the state when toggling the quick menu. Default is `nil`.
- `:sync-on-ui-close` (boolean): Sync the state to disk when closing the UI. Default is `nil`.
- `:key` (function): Function to determine the key for storing lists, allowing project-specific marks.

### Customizing Lists

You can create custom lists and define their behaviors:

```elisp
(harpoon-setup
 '(:lists ((:name "commands"
            :select-with-nil t
            :select (lambda (item _list _options)
                      (when item
                        (shell-command (plist-get item :value)))))
           (:name "buffers"
            :display (lambda (item)
                       (format "Buffer: %s" (plist-get item :value)))
            :select (lambda (item _list _options)
                      (when item
                        (switch-to-buffer (plist-get item :value))))))))
```

### Extending Harpoon

You can add custom actions and key bindings by extending Harpoon's functionality:

```elisp
(harpoon-extensions-add-listener
 (harpoon-extensions (harpoon--init))
 '((ui-create . (lambda (context)
                  (define-key (current-local-map) (kbd "C-v")
                    (lambda () (interactive)
                      (harpoon-ui-select-item :vsplit t)))
                  (define-key (current-local-map) (kbd "C-x")
                    (lambda () (interactive)
                      (harpoon-ui-select-item :split t)))
                  (define-key (current-local-map) (kbd "C-t")
                    (lambda () (interactive)
                      (harpoon-ui-select-item :tab t)))))))
```

---

## Credits

Harpoon.el is an Emacs port of [Harpoon](https://github.com/ThePrimeagen/harpoon/tree/harpoon2) by [ThePrimeagen](https://github.com/ThePrimeagen). All credit for the original concept and implementation goes to him.

Special thanks to ThePrimeagen for creating such a useful tool and inspiring this Emacs version.

---

## License

Harpoon.el is released under the [MIT License](LICENSE).

---

**Note**: The original Harpoon for Neovim is a powerful tool, and this Emacs port aims to bring the same efficiency and convenience to Emacs users. If you find Harpoon.el helpful, consider checking out the original [Harpoon](https://github.com/ThePrimeagen/harpoon/tree/harpoon2) plugin and supporting its creator.

---

Happy harpooning! 🏹
