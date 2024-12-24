# Personal Dotfiles

## Main

- WM :- [Hyprland](.config/hyprland)
- DE :- KDE
- Terminal :- Kitty(main) & alacritty(secondary)
- Editor :- [Neovim](https://github.com/shubham-cpp/dotfiles/tree/main/.config/nvim)
- Shell :- Fish
- OS :- [CachyOS](https://cachyos.org/)
- Run launcher :- [rofi](https://github.com/lbonn/rofi)(its rofi-wayland)

## Installation

```bash
# Install stow(https://wiki.archlinux.org/title/Dotfiles) :
# A program that i use for dotfile management
sudo pacman -S --needed stow    # On Arch based distros
git clone https://github.com/shubham-cpp/dotfiles.git
stow -v -t ~/ dotfiles
```
