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
  home.packages = with pkgs; [
      stow
      hugo
      neofetch
      jq
      git-crypt
      go
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
      (python39.withPackages(p: with p; [
        fontforge
        numpy
        pandas
        flask
        virtualenvwrapper
        pip
      ]))
      nodejs
      nodePackages.npm
      nodePackages.tailwindcss
      nodePackages.postcss-cli
      nodePackages.typescript
      
    ];
  home.sessionVariables = {
    WSLHOME = "/mnt/c/Users/camoh";
  };
  programs.fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
  };
  programs.zsh = {
    enable = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
    autocd = true;
    shellAliases = {
      ref = "source ~/.zshrc";
      mypy = "~/.nix-profile/bin/python3 $@";
      hms = "home-manager switch";
      doom = "~/.emacs.d/bin/doom $@";
      ls = "${pkgs.exa.outPath}/bin/exa -alh --git-ignore --icons";
      ll = "${pkgs.exa.outPath}/bin/exa -alh";
      cat = "${pkgs.bat.outPath}/bin/bat $@";
      notify-send = "wsl-notify-send.exe $@";
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
      echo $project
  }
    '';
  };
  programs.bash = {
    enable = true;
    shellAliases = {
      ref = "source ~/.bashrc";
      mypy = "~/.nix-profile/bin/python3 $@";
      hms = "home-manager switch";
      doom = "~/.emacs.d/bin/doom $@";
      ls = "${pkgs.exa.outPath}/bin/exa -alh --git-ignore --icons";
      ll = "${pkgs.exa.outPath}/bin/exa -alh";
      cat = "${pkgs.bat.outPath}/bin/bat $@";
      notify-send = "wsl-notify-send.exe $@";
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
          echo $project
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
  programs.emacs.enable = true;
  services.emacs.enable = true;
  programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
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
