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
First, clone this repo into your `~/.emacs.d` folder.
Second, download ProFontWindows [here](http://www.fontsquirrel.com/fonts/profontwindows) or change the config to use a different font.
Third, install Cask through `curl`.

NOTE: If you install Cask through Homebrew for the Mac, then you will need to update how you require cask to where it is installed. The Cask documentation will cover this.

Last, run `cd ~/.emacs.d/ && ~/.cask/bin/cask install`

You should now be able to use my config for emacs!

## License
Standard MIT License, so do whatever. I'm not responsible for what happens
after your copy and pasting shenanigans or git cloning or whatever....ing. :D
