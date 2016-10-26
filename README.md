# jax-emacs-setup
This is where I store all of my custom emacs configs and lisps.

## Features

* Package management through Cask!
* A `custom-init.el` file, where you can store any sensitive configs that you
don't want in a git repo. It's also great for setting up configs for different
machines.
* A separate folder for any copy and pasted/created lisps in `lisps/`
* Theme - Monokai with ProFontWindows. Looks great on any monitor!
* The ability to use a simple `C-y` in `ansi-term` for pasting
* Pulls in your `PATH` in `ansi-term`
* Line Numbers! On Everything! For now. Hopefully, I can get this to only happen
for certain major modes. If you have any suggestions, add an issue.
* Helm, Projectile, Smex, Ido-Vertical and more for completion ease of use
* Auto-modes for Python, Javascript, Coldfusion (don't ask...), HTML, CSS, LESS and Org files

## Installation
* First, clone this repo into your `~/.emacs.d` folder.
* Second, download ProFontWindows [here](http://www.fontsquirrel.com/fonts/profontwindows) or change the config to use a different font.

### NOTE:
This setup no longer uses Cask, but instead uses `use-package`. This has proven to work more effectively between different OSes for my
setup and was easier to install. You shouldn't have to do anything special for installing it, unlike Cask.

You should now be able to use my config for emacs!

## Organization

* lisp: Folder where you can place custom lisp files which will be loaded by init.el.
* settings: Folder for `editor.el`, which are my 'standard' settings for the editor,
and `custom-editor.el`, which is a great place to store settings for the editor that
git will not override, so any customizations you want or environment specific settings
should go here.
* init.el: Main config file; if you want to protect yourself from git overriding your
changes, then I suggest using `settings/custom-editor.el` or `lisp/*.el`. If you're
forking this repo and won't be reintroducing changes later, then edit away. :D

## License
Standard MIT License, so do whatever. I'm not responsible for what happens
after your copy and pasting shenanigans or git cloning or whatever....ing. :D
