# Personal Dotfiles

## Main
- WM       :- [Awesomewm](.config/awesome/README.md)
- Terminal :- Kitty(main) & xterm(secondary)
- Editor   :- [Neovim](https://github.com/shubham-cpp/dotfiles/tree/main/.config/nvim)\([nightly](https://github.com/neovim/neovim/wiki/Building-Neovim)\)
- Shell    :- Zsh
- Run launcher :- [dmenu_run_history](https://tools.suckless.org/dmenu/scripts/dmenu_run_with_command_history) and rofi

## Installation
```bash
# Install stow(https://wiki.archlinux.org/title/Dotfiles) :
# A program that i use for dotfile management
sudo pacman -S --needed stow    # On Arch based distros
git clone https://github.com/shubham-cpp/dotfiles.git
stow -v -t ~/ dotfiles
```
