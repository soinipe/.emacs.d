# Personal aliases

alias ll='ls -l'
alias rm='rm -i'
alias t=less
alias j=jobs
alias h=history

alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gl="git log"
alias gd="git diff"
alias gb="git branch"
alias gr="git reset"

alias valg="G_SLICE=always-malloc; G_DEBUG=gc-friendly; valgrind -v --tool=memcheck --leak-check=full --num-callers=40"

# End of file
