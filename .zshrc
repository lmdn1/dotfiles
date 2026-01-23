# Dumb but needed for tmux based nonsense
[[ -z "${LOAD_RC_ANYWAY}" ]] && { [[ $- = *i* ]] || return }

# Loading these early to avoid breaking aliases.
if [[ -d "${HOME}/.rc_extra" ]]; then
  for file in ${HOME}/.rc_extra/*.sh; do
    [[ -f "${file}" ]] && source "${file}"
  done
fi

# Shell colors
B281C1=$'\e[38;2;178;129;193m'
A05EAC=$'\e[38;2;160;94;172m'
EE829F=$'\e[38;2;238;130;159m'
RESET=$'\e[0m'

# ls & lf theme
eval $(dircolors ~/.config/bliss.dircolors)

# Aliases because I am very lazy
alias ..="cd .."
alias cd..="cd .."
alias pls="sudo"
alias please="sudo"
alias nano="nvim"
alias v="nvim"
alias vi="nvim"
alias v.="nvim ."
alias ta="tmux a"
alias to="tmux a -t"
alias tls="tmux ls"
alias tren="tmux rename-session"
alias tks="tmux kill-server"
alias tkss="tmux kill-session"
alias neofetch="fastfetch"
alias cockfetch="fastfetch"

# Env vars
export TERMINAL="alacritty"
export EDITOR="nvim"
export PAGER="less"
export GOPATH="${HOME}/go"

# Keybindings to make zsh nicer
bindkey -v
bindkey "^[[1~" beginning-of-line  # Home (scrt)
bindkey "^[[4~" end-of-line        # End (scrt)
bindkey "^[[H" beginning-of-line   # Home (normal)
bindkey "^[[F" end-of-line         # End (normal)
bindkey "^[[3~" delete-char        # Delete
bindkey "^H" backward-kill-word    # Ctrl + Backspace
bindkey "^[[3;5~" kill-word        # Ctrl + Delete
bindkey "^[[1;5C" forward-word     # Ctrl + RArrow
bindkey "^[[1;5D" backward-word    # Ctrl + LArrow

# Explicitly set for tmux
bindkey '^R' history-incremental-pattern-search-backward

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'e' edit-command-line

# Functions
set-title() {
    echo -e "\e]0;$*\007"
}
tmc() {
    tmux switch-client -t "$(tmux new -dP -s ${1})"
}

# Prompt Setup
typeset -gA GIT_ALIASES=(
    lmdn1 lmdn
)

get-git-profile() {
    unset GIT_PROF
    ! git rev-parse --is-inside-work-tree &>/dev/null && return

    local user=$(git config user.name)
    GIT_PROF=${GIT_ALIASES[${user}]:-${user}}
}
set-prompt() {
    get-git-profile

    # I like having an empty newline between commands
    local _prompt=$'\n'

    [[ -n "${AWS_PROFILE}" ]] && {
      _prompt="${_prompt}%F{default}[%F{172}${AWS_PROFILE}%F{default}]"
    }
    [[ -n "${GIT_PROF}" ]] && {
      _prompt="${_prompt}%F{default}[%F{099}${GIT_PROF}%F{default}]"
    }
    [[ -n "${vcs_info_msg_0_}" ]] && {
      _prompt="${_prompt}"'${vcs_info_msg_0_}'$'\n'
    }

    _prompt="${_prompt}"'%{$B281C1%}%36<...<%/%{$RESET%}%F{default}> '
    export PROMPT="${_prompt}"
}
autoload -Uz add-zsh-hook vcs_info
setopt prompt_subst
add-zsh-hook precmd vcs_info
add-zsh-hook precmd set-prompt
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr " %{$EE829F%}*%{$RESET%}"
zstyle ':vcs_info:*' stagedstr " %{$B281C1%}+%{$RESET%}"
zstyle ':vcs_info:git:*' formats "[%{$A05EAC%}%r %b%{$RESET%}%u%c]"
export RPROMPT=

HISTFILE=~/.hist_zsh
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

mkdir -p "${HOME}/.parallel"
touch "${HOME}/.parallel/will-cite"

# Machine specific (PATH, etc)
[[ -f "${HOME}/.zshrc.local" ]] && source "${HOME}/.zshrc.local"
