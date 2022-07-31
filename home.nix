{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jhilker";
  home.homeDirectory = "/home/jhilker";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  targets.genericLinux.enable = true;
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
      stow
      hugo
      neofetch
      jq
      git-crypt
      dict
      dictdDBs.wordnet
      dictdDBs.wiktionary
      powershell
      unzip
      youtube-dl
    #  ncspot
      sqlite
      binutils
      (ripgrep.override { withPCRE2 = true; })
      gnutls
      fd
      imagemagick
      zstd
      nodePackages.javascript-typescript-langserver
      editorconfig-core-c
      emacs-all-the-icons-fonts
      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        en-science
        grc]))
      gcc
      zlib
      (python39.withPackages(p: with p; [
        fontforge
        numpy
        pandas
        flask
        virtualenvwrapper
        pip
        python-lsp-server
      ]))
      nodejs
      nodePackages.npm
      nodePackages.tailwindcss
      nodePackages.postcss-cli
      nodePackages.typescript
      nodePackages.pyright
      rustc
      cargo
      ##rustup
      go
      
    ];
  home.sessionVariables = {
    EDITOR="$HOME/.nix-profile/bin/nvim";
    WSLHOME = "/mnt/c/Users/camoh";
    PROJECT_HOME="$HOME/Devel/python/";
    LD_LIBRARY_PATH="$(nix eval nixpkgs#zlib.outPath --raw)/lib";
    #DISPLAY="$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0";
    #LIBGL_ALWAYS_INDIRECT = 1;
  };
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      tmux.enableShellIntegration = true;
  };
  programs.zsh = {
    enable = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
    autocd = true;
    shellAliases = {
      ref = "source ~/.zshrc";
      mypy = "~/.nix-profile/bin/python3 $@";
      #hms = "home-manager switch -f ~/.dotfiles/home.nix";
      doom = "~/.emacs.d/bin/doom $@";
      ls = "${pkgs.exa.outPath}/bin/exa -alh --git-ignore --icons";
      ll = "${pkgs.exa.outPath}/bin/exa -alh";
      cat = "${pkgs.bat.outPath}/bin/bat $@";
      notify-send = "wsl-notify-send.exe $@";
      spicetify = "spicetify.exe $@";
    };
    initExtra = ''
      source "${pkgs.python39Packages.virtualenvwrapper.outPath}/bin/virtualenvwrapper.sh"
      ## source ~/.local/fzf-marks/fzf-marks.plugin.zsh
      RUNNING=`ps aux | grep dockerd | grep -v grep`
      if [ -z "$RUNNING" ]; then
          sudo dockerd > /dev/null 2>&1 &
          disown
      fi
      function toWorkOn(){
          project="$(lsvirtualenv -b | fzf)"
          if [[  $VIRTUALENVWRAPPER_VIRTUALENV == "virtualenv" ]]; then
            workon $project
          else
             printf "deactivating venv for %s\n" "$project"
             deactivate; workon $project
          fi
      
      }
      gi() {
              toIgnore="$(curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/list | sed 's/,/\n/g' | fzf -m | xargs | sed 's/\s/,/g')"
              curl -sL "https://www.toptal.com/developers/gitignore/api/$toIgnore" >> .gitignore
      }
    '';
  };
  programs.bash = {
    enable = true;
    shellAliases = {
      ref = "source ~/.bashrc";
      mypy = "~/.nix-profile/bin/python3 $@";
      #hms = "home-manager switch -f ~/.dotfiles/home.nix";
      doom = "~/.emacs.d/bin/doom $@";
      ls = "${pkgs.exa.outPath}/bin/exa -alh --git-ignore --icons";
      ll = "${pkgs.exa.outPath}/bin/exa -alh";
      cat = "${pkgs.bat.outPath}/bin/bat $@";
      notify-send = "wsl-notify-send.exe $@";
      spicetify = "spicetify.exe $@";
    };
    initExtra = ''
      source "${pkgs.python39Packages.virtualenvwrapper.outPath}/bin/virtualenvwrapper.sh"
      ## source ~/.local/fzf-marks/fzf-marks.plugin.bash
      RUNNING=`ps aux | grep dockerd | grep -v grep`
      if [ -z "$RUNNING" ]; then
          sudo dockerd > /dev/null 2>&1 &
          disown
      fi
      function toWorkOn(){
          project="$(lsvirtualenv -b | fzf)"
          if [[  $VIRTUALENVWRAPPER_VIRTUALENV == "virtualenv" ]]; then
            workon $project
          else
             printf "deactivating venv for %s\n" "$project"
             deactivate; workon $project
          fi
      
      }
      gi() {
              toIgnore="$(curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/list | sed 's/,/\n/g' | fzf -m | xargs | sed 's/\s/,/g')"
              curl -sL "https://www.toptal.com/developers/gitignore/api/$toIgnore" >> .gitignore
      }
      '';
  };
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      line_break.disabled = true;
    };
  };
  programs.tmux = {
    enable = true;
    shortcut = "a";
    keyMode = "vi";
  };
  programs.emacs.enable = true;
  services.emacs.enable = true;
  programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      extraConfig = ''
        lua << EOF
        ${builtins.readFile ./editors/nvim/config.lua};
      '';
      coc.enable = true;
      #coc.package = pkgs.vimPlugins.nvim-lspconfig;
  
      plugins = with pkgs.vimPlugins; [
        gruvbox-nvim
        vim-nix
        nvim-treesitter
        vim-lightline-coc
        vim-airline
        vim-airline-themes
        coc-pyright
        coc-tailwindcss
      ];
  
      withPython3 = true;
      withNodeJs = true;
  };
  programs.git = {
    enable = true;
    userName = "Jacob Hilker";
    userEmail = "jacob.hilker2@gmail.com";
    signing = {
      key = "jacob.hilker2@gmail.com";
  
      signByDefault = true;
    };
    delta = {
      enable = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
    };
  };
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    defaultCacheTtlSsh = 86400;
  };
}
