# Personal Awesomewm config

## Awesomewm setup with some bloated features
- Title bar for floating windows only
- Cycle Through Non empty(Active) tags
- Better way to resize windows
- Don't show borders on max layout or if only 1 window exists
- Move the window to immediate right/left tag
- Move/resize floating windows using keybindings
- And much more

## To make this setup work properly
```bash
# First make backup of current config
cp -r ~/.config/awesome{,-bkp}
git clone https://github.com/lcpz/awesome-copycats.git
cd awesome-copycats
cp -r lain themes freedesktop --target-directory ~/.config/awesome
# then clone this repo and copy awesome folder from
# dotfiles/window-managers/.config/awesome
```

## Credits
- [Copycats](https://github.com/lcpz/awesome-copycats.git) : Making it possible to configure statusbar
- [Lain](https://github.com/lcpz/lain.git) : Widget library and also providing extra functionalities
- [awesome-wm-widgets](https://pavelmakhov.com/awesome-wm-widgets/) : The brightness widget comes from here

## Todo
- Will probably use awesome-wm-widgets to configure entire statusbar to remove copycat dependency
- Either use [awesome-ez](https://github.com/jcrd/awesome-ez) or sxhkd to configure keybindings in better way
