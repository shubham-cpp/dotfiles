{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home = {
    username = "shubham";
    homeDirectory = "/home/shubham";
    stateVersion = "23.05"; # Please read the comment before changing.
    sessionVariables = {
      # EDITOR = "neovim";
    };
  };
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
  atool
  alacritty
  kitty
  tmux
  nodejs
  fd
  ripgrep
  brave
  fnm
  qbittorrent
  gnome.file-roller
  evince
  xclip
  exa
  starship
  luajit
  rofi
  rofimoji
  openjdk
  # gcc
  clang
  go
  xwallpaper
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/shubham/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "Shubham Pawar(shubham-cpp)";
    userEmail = "shubhampawar3007@gmail.com";
    delta = {
      enable = true;
    };
    aliases = {
	    logline = "log --graph --pretty=format:'%C(ul red)%h%Creset %Cgreen%as%Creset |%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
	    ll = "log --graph --pretty=format:'%C(ul red)%h%Creset %Cgreen%as%Creset |%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'";
    };
    includes = [{ 
      contents = { 
        init.defaultBranch = "main";
        core.excludesfile = "~/.config/git/ignore";
      };
    }];
  };
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    withNodeJs = true;
  };
  programs.vscode = {
    enable = true;
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    package =  pkgs.vscodium;
  };
  programs.fzf = {
    enable = true;
    changeDirWidgetCommand = "fd -t d";
    defaultCommand = "fd -t f";
    fileWidgetCommand = "fd -t f";
    fileWidgetOptions = [ "--preview 'head {}'" ];
  };
}
