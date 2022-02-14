{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    username = "sp";
    homeDirectory = "/home/sp";
    stateVersion = "21.11";
    sessionVariables = { BROWSER = "firefox"; };
    sessionPath = [
      "$HOME/.local/bin"
      "$HOME/.local/share/npm/bin"
      "$HOME/.local/share/cargo/bin"
    ];
  };

  home.packages = with pkgs; [
    htop
    neofetch
    bpytop
    # Themes
    dracula-theme
    paper-gtk-theme
    qogir-theme
    tela-icon-theme
    moka-icon-theme
    qogir-icon-theme
    # Browsers
    ungoogled-chromium
    # Office
    onlyoffice-bin
    # CLI tools
    ripgrep
    fd
    tree
    exa
    ksuperkey
    # xorg
    xorg.setxkbmap
    xcape
    nixfmt
    statix
    rnix-lsp
    # Torrent
    qbittorrent
  ];

  programs = {
    git = { enable = true; };
    fzf = {
      enable = true;
      changeDirWidgetCommand = "fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
      defaultCommand = "rg --files --hidden";
      fileWidgetCommand = "rg --files --hidden";
    };
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      dotDir = ".config/zsh";
      history = {
        extended = true;
        ignoreDups = true;
        path = "$HOME/.cache/zsh_history";
      };
    };
    fish = { enable = true; };
    alacritty = {
      enable = true;
      settings = {
        font = {
          normal.family = "JetBrainsMono Nerd Font";
          size = 10;
        };
        draw_bold_text_with_bright_colors = true;
        background_opacity = 0.8;
        selection = {
          semantic_escape_chars = '',│`|:"' ()[]{}<>	'';
          save_to_clipboard = true;
          dynamic_title = true;
        };
      };
    };
    kitty = {
      enable = true;
      font = {
        size = 10;
        name = "JetBrainsMono Nerd Font";
      };
    };
    rofi = {
      enable = true;
      plugins = with pkgs; [ rofi-emoji ];
      cycle = true;
      font = "BlexMono Nerd Font 12";
      extraConfig = {
        display-run = " ";
        display-drun = "  ";
        display-window = "  ";
        drun-display-format = "{icon} {name}";
        matching = "regex";
        font = "FuraCode Nerd Font 12";
        modi = "window,run,drun";
        show-icons = true;
        sorting-method = "fzf";
        sort = true;
      };
    };
  };
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      svelte.svelte-vscode
      # bbenoist.Nix
      formulahendry.code-runner
      streetsidesoftware.code-spell-checker
      wix.vscode-import-cost
      bradlc.vscode-tailwindcss
      dbaeumer.vscode-eslint
      brettm12345.nixfmt-vscode
      mskelton.one-dark-theme
      pkief.material-icon-theme
      esbenp.prettier-vscode
      vscodevim.vim
      formulahendry.auto-close-tag
      naumovs.color-highlight
    ];
  };
  xsession = {
    pointerCursor = {
      package = pkgs.capitaine-cursors;
      name = "Vanilla-DMZ";
    };
    profilePath = ".config/xprofile";
    scriptPath = ".config/xsession";
    numlock.enable = true;
  };
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
