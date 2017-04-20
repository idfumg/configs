# Path to your oh-my-zsh installation.
  export ZSH=/home/idfumg/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"
# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.

# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

# User configuration

  export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/oracle/product/db/bin:/home/idfumg/bin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -f $HOME/.common_env ] ; then
    . ~/.common_env
fi

alias ..='cd ..'
alias cds='cd /home/idfumg/work/trunk/src'
alias rail='cd /home/idfumg/work/trunk/src/rail'
alias sqw='rlwrap sqlplus stable/stable'
alias sqa='rlwrap sqlplus trunk/trunk'
alias e='emacs -nw'
alias grep='LC_ALL=POSIX grep --color=auto'
alias svn_diff='svn diff --diff-cmd=colordiff | iconv -f ibm866 -t utf-8'
alias diff='colordiff'

GCC4_7_3PATH='/opt/usr/bin:/usr/bin:/usr/sbin'
#PATH=${GCC4_7_3PATH}:${PATH}

C_COMPILER='gcc'
CXX_COMPILER='g++'
export CC=$C_COMPILER
export CXX=$CXX_COMPILER
export LOCAL_CC=$C_COMPILER
export LOCAL_CXX=$CXX_COMPILER
export ENABLE_GLIBCXX_DEBUG=1 # add to executable additional checkings when compiling.

export SIRENA_MAKE_COLOR=1
export ECHO='echo -e'
export BUILD_TESTS=1
export ENABLE_SHARED=1

alias sirena_build='cc_version.sh && ./buildFromScratch.sh'
alias obrzap_make='make -sj4 obrzap'
alias rail_make='make -sj4 rail'
alias rail_clean='make -C rail clean'
alias rail_test='XP_LIST=rail make xp-tests'
alias rail_rebuild='rail_clean && rail_make && obrzap_make && rail_test'
alias pos_make='make -sj4 pos_auth'
alias pos_clean='make -C pos_auth clean'
alias pos_test='XP_LIST=pos_auth make xp-tests'
alias pos_rebuild='pos_clean && pos_make && obrzap_make && pos_test'

#export SVN_SSH='ssh -l -i /home/idfumg/.ssh/id_rsa svn'
export SVN_SSH='ssh -i /home/idfumg/.ssh/id_rsa -l svn'
export SVN_BASE='svn+ssh://svn/SVNroot/sirena'
export NLS_LANG=AMERICAN_CIS.RU8PC866

export ORACLE_BASE=/oracle
export ORACLE_HOME=/oracle/product/db
export ORACLE_SID=database
export ORACLE_INVENTORY=/oracle/inventory

export PATH=$HOME/bin:$PATH:$ORACLE_HOME/bin
export LD_LIBRARY_PATH=$HOME/lib:$HOME/work/trunk/externallibs/boost/lib:/usr/lib:/usr/local/lib:$ORACLE_HOME/lib:$LD_LIBRARY_PATH
export EDITOR=subl3
export VISUAL=subl3

export PLATFORM=m64
export LANGUAGE=en_US
export LANG=en_US.UTF-8
#export LOCALE=ru_RU.IBM866


archey3
if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi
pgrep -u idfumg -x -f awesome || startx

