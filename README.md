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
  - [Adding Files](#adding-files)
  - [Navigating Files](#navigating-files)
  - [Managing Marks](#managing-marks)
- [Customization](#customization)
  - [Changing Key Bindings](#changing-key-bindings)
  - [Configuration](#configuration)
    - [Settings](#settings)
    - [Customizing Lists](#customizing-lists)
    - [Extending Harpoon](#extending-harpoon)
- [Logger](#logger)
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
- **Logger**: Built-in logging utility to help with debugging and development.

---

## Installation

### For Emacs

You can install Harpoon.el manually or through package managers like [MELPA](https://melpa.org/) (once it's available).

#### Manual Installation

1. **Clone the repository:**

   ```shell
   git clone https://github.com/crazywolf132/harpoon.el.git
   ```

2. **Add the directory to your Emacs `load-path`:**

   ```elisp
   (add-to-list 'load-path "/path/to/harpoon.el/")
   ```

3. **Require Harpoon in your Emacs configuration:**

   ```elisp
   (require 'harpoon)
   ```

4. **Set up Harpoon:**

   ```elisp
   (harpoon-setup)
   ```

### For Spacemacs

Add Harpoon.el to your `dotspacemacs-additional-packages` list in your `.spacemacs` file:

```elisp
dotspacemacs-additional-packages '(
  harpoon
)
```

Then, require and set up Harpoon in your user configuration:

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

First, ensure that Harpoon is properly installed and required in your Emacs configuration. Then, initialize Harpoon by calling `harpoon-setup`:

```elisp
(require 'harpoon)
(harpoon-setup)
```

---

## Usage

### Adding Files

- **Add Current File to Harpoon**: Press `C-c h a` (or your chosen keybinding) to add the current file to your Harpoon list.

### Navigating Files

- **Toggle Quick Menu**: Press `C-c h t` (or your chosen keybinding) to open or close the Harpoon quick menu.
- **Navigate to Marked Files**: Use `C-c h 1`, `C-c h 2`, etc., to jump to the files you've marked.
- **Quick Navigation**: Within the quick menu, select a file to jump to it.

### Managing Marks

- **Remove Marks**: Within the quick menu, you can remove files from your list by deleting the corresponding line and saving.
- **Reorder Marks**: Rearrange the lines in the quick menu to reorder your marks.

---

## Customization

### Changing Key Bindings

Harpoon.el does not enforce any default keybindings, allowing you to customize them according to your preferences. Here's how you can set up and change the keybindings:

#### Example Key Bindings

```elisp
;; Require Harpoon and set up
(require 'harpoon)
(harpoon-setup)

;; Key bindings
(global-set-key (kbd "C-c h a") 'harpoon-add-file)             ; Add file
(global-set-key (kbd "C-c h t") 'harpoon-toggle-quick-menu)    ; Toggle menu

;; Navigate to files by index (change the keybindings as desired)
(global-set-key (kbd "C-c h 1") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1)))
(global-set-key (kbd "C-c h 2") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2)))
(global-set-key (kbd "C-c h 3") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3)))
(global-set-key (kbd "C-c h 4") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4)))
```

#### Customizing Key Bindings

You can change the keybindings to whatever suits your workflow. For example:

```elisp
;; Use F5 to add the current file
(global-set-key (kbd "<f5>") 'harpoon-add-file)

;; Use F6 to toggle the quick menu
(global-set-key (kbd "<f6>") 'harpoon-toggle-quick-menu)

;; Use Alt + number keys to navigate
(global-set-key (kbd "M-1") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4)))
```

Feel free to assign any key combinations that don't conflict with your existing keybindings.

#### Using `use-package`

If you use `use-package`, you can define keybindings within its configuration:

```elisp
(use-package harpoon
  :config
  (harpoon-setup)
  :bind
  (("C-c h a" . harpoon-add-file)
   ("C-c h t" . harpoon-toggle-quick-menu)
   ("C-c h 1" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1)))
   ("C-c h 2" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2)))
   ("C-c h 3" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3)))
   ("C-c h 4" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4)))))
```

#### For Spacemacs Users

In Spacemacs, you can customize keybindings in your `dotspacemacs/user-config` function:

```elisp
(defun dotspacemacs/user-config ()
  (require 'harpoon)
  (harpoon-setup)

  ;; Key bindings
  (spacemacs/set-leader-keys
    "ah" 'harpoon-add-file              ; Add file (leader + a h)
    "at" 'harpoon-toggle-quick-menu     ; Toggle menu (leader + a t)
    "a1" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1))
    "a2" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2))
    "a3" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3))
    "a4" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4))))
```

#### For Doom Emacs Users

In Doom Emacs, you can set keybindings in your `config.el`:

```elisp
(use-package harpoon
  :config
  (harpoon-setup)
  :bind
  (("C-c h a" . harpoon-add-file)
   ("C-c h t" . harpoon-toggle-quick-menu)
   ("C-c h 1" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1)))
   ("C-c h 2" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2)))
   ("C-c h 3" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3)))
   ("C-c h 4" . (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4)))))
```

Alternatively, you can use Doom's keybinding macros:

```elisp
(map! :leader
      (:prefix ("h" . "harpoon")
       :desc "Add file" "a" #'harpoon-add-file
       :desc "Toggle menu" "t" #'harpoon-toggle-quick-menu
       :desc "Go to file 1" "1" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 1))
       :desc "Go to file 2" "2" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 2))
       :desc "Go to file 3" "3" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 3))
       :desc "Go to file 4" "4" (lambda () (interactive) (harpoon-list-select (harpoon-get-list) 4))))
```

### Configuration

Harpoon.el is highly customizable. You can adjust settings and define custom behaviors for your lists.

#### Settings

To configure Harpoon, you can pass an optional configuration to `harpoon-setup`:

```elisp
(harpoon-setup
 '(:settings (:save-on-toggle t
               :sync-on-ui-close t
               :key (lambda () (projectile-project-root)))))
```

##### Available Settings

- `:save-on-toggle` (boolean): Save the state when toggling the quick menu. Default is `nil`.
- `:sync-on-ui-close` (boolean): Sync the state to disk when closing the UI. Default is `nil`.
- `:key` (function): Function to determine the key for storing lists, allowing project-specific marks.

#### Customizing Lists

You can create custom lists and define their behaviors by providing custom configurations. Here's an example of creating a custom list named `"commands"` that allows you to execute shell commands:

```elisp
(harpoon-setup
 `(:lists ((:name "commands"
             :select-with-nil t
             :create-list-item ,(lambda (_config &optional name)
                                  (let ((cmd (or name
                                                 (read-string "Enter command: "))))
                                    (list :value cmd :context nil)))
             :select ,(lambda (item _list _options)
                        (when item
                          (shell-command (plist-get item :value)))))
            (:name "buffers"
             :display ,(lambda (item)
                         (format "Buffer: %s" (plist-get item :value)))
             :select ,(lambda (item _list _options)
                        (when item
                          (switch-to-buffer (plist-get item :value))))))))
```

In this configuration:

- **`commands` List:**
  - **`select-with-nil`:** Allows selection even if the item is `nil`.
  - **`create-list-item`:** Prompts the user to enter a command.
  - **`select`:** Executes the command when selected.

- **`buffers` List:**
  - **`display`:** Custom display format for buffer items.
  - **`select`:** Switches to the buffer when selected.

#### Extending Harpoon

You can add custom actions and key bindings by extending Harpoon's functionality using the extensions system.

**Example: Adding Keybindings for Splits and Tabs**

```elisp
(let ((extensions (harpoon-extensions (harpoon--init))))
  (harpoon-extensions-add-listener
   extensions
   `((ui-create . ,(lambda (context)
                     ;; Add keybinding for vertical split
                     (define-key (current-local-map) (kbd "C-v")
                       (lambda () (interactive)
                         (harpoon-ui-select-item :vsplit t)))
                     ;; Add keybinding for horizontal split
                     (define-key (current-local-map) (kbd "C-x")
                       (lambda () (interactive)
                         (harpoon-ui-select-item :split t)))
                     ;; Add keybinding for opening in new tab
                     (define-key (current-local-map) (kbd "C-t")
                       (lambda () (interactive)
                         (harpoon-ui-select-item :tab t))))))))
```

**Explanation:**

- **`ui-create` Event:** We add a listener for the `ui-create` event, which is emitted when the Harpoon UI is created.
- **Keybindings:**
  - **`C-v`:** Opens the selected file in a vertical split.
  - **`C-x`:** Opens the selected file in a horizontal split.
  - **`C-t`:** Opens the selected file in a new tab (Emacs does not have tabs by default, but you can integrate with packages like `tab-bar-mode`).

**Note:** You may need to adjust the `harpoon-ui-select-item` function to handle these options, or customize the select function in your configuration to handle splits and tabs.

---

## Logger

Harpoon.el includes a built-in logger to help with debugging and development. You can use the logger to view internal messages and diagnose issues.

### Using the Logger

- **Show the Log Buffer:**

  ```elisp
  (harpoon-logger-show)
  ```

  This command will display the Harpoon log in a buffer called `*Harpoon Log*`.

- **Clear the Log:**

  ```elisp
  (harpoon-logger-clear)
  ```

- **Log Messages:**

  The logger is used internally by Harpoon, but you can also log custom messages:

  ```elisp
  (harpoon-logger-log "This is a custom log message.")
  ```

### Debugging Steps

If you encounter issues with Harpoon.el:

1. **Open a New Emacs Instance:**

   Start Emacs without any additional configurations that might interfere.

2. **Perform the Steps to Reproduce the Issue:**

   Use Harpoon as you normally would to trigger the issue.

3. **View the Log:**

   ```elisp
   (harpoon-logger-show)
   ```

4. **Copy the Log:**

   Copy the contents of the `*Harpoon Log*` buffer.

5. **Report the Issue:**

   Provide the log when reporting issues to help with debugging.

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
