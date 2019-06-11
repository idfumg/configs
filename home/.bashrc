# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export PLATFORM=m64

################################################################################
# C++ COMPILER
################################################################################

export C_COMPILER='gcc'
export CXX_COMPILER='g++'
export CC=$C_COMPILER
export CXX=$CXX_COMPILER
export LOCAL_CC=$C_COMPILER
export LOCAL_CXX=$CXX_COMPILER

gcc_version() {
    echo '****************** ' $CC ' configuration **********************\n'
    echo | $CC -xc++ -E -v -
    echo '\nOPTIONS:'
    echo 'ENABLE_GLIBCXX_DEBUG='${ENABLE_GLIBCXX_DEBUG}
    echo 'CC='${CC}
    echo 'CXX='${CXX}
    echo 'LOCAL_CC='${LOCAL_CC}
    echo 'LOCAL_CXX='${LOCAL_CXX}
    echo '\n*************************************************************\n'
}

################################################################################
# SSH
################################################################################

export SVN_SSH='ssh -i $HOME/.ssh/id_rsa -l svn'
export SVN_BASE='svn+ssh://svn/SVNroot/sirena'

################################################################################
# ORACLE
################################################################################

export ORACLE_BASE=/u01/app/oracle
export ORACLE_HOME=$ORACLE_BASE/product/12.2.0/db_1
export ORACLE_SID=orcl
export ORACLE_INVENTORY=$ORACLE_HOME/inventory
export PATH=$ORACLE_HOME/bin:$PATH
export LD_LIBRARY_PATH=$ORACLE_HOME/lib:$LD_LIBRARY_PATH
export NLS_LANG=AMERICAN_CIS.RU8PC866

################################################################################
# MISC
################################################################################

export LC_ALL=POSIX
export EDITOR=nano
export VISUAL=nano
export PATH=$HOME/bin:$PATH
export TERM=screen-256color
export PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

# export PLATFORM=m64
# export LANGUAGE=en_US
# export LANG=en_US.UTF-8
# export LANG=ru_RU.utf8
# export LOCALE=ru_RU.IBM866

################################################################################
# Sirena helper functions
################################################################################

export SIRENA_PATH="$HOME/work"
export SIRENA_PATH_TRUNK="$SIRENA_PATH/trunk"
export SIRENA_PATH_STABLE="$SIRENA_PATH/stable"
export SIRENA_DOCKER_PATH="/sirena_src"

cpu_count() {
    grep -c ^processor /proc/cpuinfo
}

sirena_data_postgres() {
    docker exec -u postgres:postgres sirena sh -c "psql -c \"SHOW data_directory\""
}

sirena_data_oracle() {
    sirena_exec "echo \"select distinct regexp_substr(name,'^.*\\') from v\\\$datafile;\" > /root/start.sql && sqlplus / as sysdba @/root/start.sql"
}

sirena_exec() {
    docker exec -u $(id -u $USER):$(id -g $USER) sirena sh -c ". /root/.bashrc && $@"
}

sirena_start_docker() {
    local POSTGRESQL_DATA="/var/lib/postgresql/10/main"
    local ORACLE_DATA="$ORACLE_BASE/oradata"
    docker run --network host --privileged --rm --name sirena -d  -u $(id -u $USER):$(id -g $USER) -v $PWD:$SIRENA_DOCKER_PATH -v $POSTGRESQL_DATA -v $ORACLE_DATA sirena/dev:0.10.0 sh -c "trap : TERM INT; sleep infinity & wait"
}

sirena_start_postgresql() {
    docker exec -u root:root sirena sh -c "service postgresql start"
    docker exec -u postgres:postgres sirena sh -c "psql -c \"create user system encrypted password 'manager' superuser;\""
}

sirena_start_oracle() {
    docker exec -u oracle sirena sh -c ". /root/.bashrc && echo 'startup;' > /root/start.sql && sqlplus / as sysdba @/root/start.sql"
}

sirena_start() {
    sirena_start_docker
    sirena_start_postgresql
    sirena_start_oracle
}

sirena_stop() {
    docker stop sirena
}

sirena_build() {
    local SIRENA_BUILD_VARS="BUILD_TESTS=1 ENABLE_SHARED=1 ENABLE_GLIBCXX_DEBUG=1 LANG=en_US.UTF-8 LANGUAGE=en_US"

    local gcc_version="
    echo CC=\${CC}
    echo CXX=\${CXX}
    echo LOCAL_CC=\${LOCAL_CC}
    echo LOCAL_CXX=\${LOCAL_CXX}
    echo ''
    echo ''
    echo \$(\${CC} -xc++ -E -v -)"

    local build_command="$gcc_version && cd $SIRENA_DOCKER_PATH && echo Path: \${PWD} && $SIRENA_BUILD_VARS ./buildFromScratch.sh"

    local database_login=$1
    local database_password=$2

    if [ -z $database_login ] && [ -z $database_password ]; then
        sirena_exec "$build_command $@"
        return 0
    fi

    if [ $# -eq 2 ] || [ $# -eq 3 ]; then
        if [ -n $database_login ] && [ -n $database_password ]; then
            sirena_exec "$build_command $database_login/$database_password $3"
            return 0
        fi
    fi

    echo "Error! Wrong function parameters!"
    return 1
}

sirena_build_trunk() {
    sirena_build trunk trunk
}

sirena_build_stable() {
    sirena_build stable stable
}

sirena_make() {
    sirena_exec "cd $SIRENA_DOCKER_PATH/src && make -sj $(cpu_count) $@"
}

sirena_make_obrzap() {
    sirena_make "obrzap"
}

sirena_make_rail() {
    sirena_make "rail"
}

sirena_make_posauth() {
    sirena_make "pos_auth"
}

sirena_make_airimp() {
    sirena_make "airimp"
}

sirena_clean() {
    sirena_exec "cd $SIRENA_DOCKER_PATH/src && make -C $@ clean"
}

sirena_clean_rail() {
    sirena_clean "rail"
}

sirena_clean_posauth() {
    sirena_clean "pos_auth"
}

sirena_clean_airimp() {
    sirena_clean "airimp"
}

sirena_rebuild_rail() {
    sirena_clean_rail && sirena_make_rail
}

sirena_rebuild_posauth() {
    sirena_clean_posauth && sirena_make_posauth
}

sirena_rebuild_airimp() {
    sirena_clean_airimp && sirena_make_airimp
}

sirena_test() {
    if [ $# -eq 1 ]; then
        sirena_exec "cd $SIRENA_DOCKER_PATH/src && XP_LIST=$1 make xp-tests"
        return 0
    fi

    sirena_exec "cd $SIRENA_DOCKER_PATH/src && XP_LIST=$1.$2 make xp-tests"
}

sirena_test_rail() {
    sirena_test "rail" $@
}

sirena_test_posauth() {
    sirena_test "pos_auth" $@
}

sirena_test_airimp() {
    sirena_test "airimp" $@
}

# docker exec -e LC_CTYPE="en_US.UTF-8" -e LANG="en_US.UTF-8" -e LANGUAGE="en_US:en" -e LC_ALL="en_US.UTF-8" -u root:root sirena sh -c "locale-gen en_US"
# docker exec -e LC_CTYPE="en_US.UTF-8" -e LANG="en_US.UTF-8" -e LANGUAGE="en_US:en" -e LC_ALL="en_US.UTF-8" -u root:root sirena sh -c "locale-gen en_US.UTF-8"
# docker exec -e LC_CTYPE="en_US.UTF-8" -e LANG="en_US.UTF-8" -e LANGUAGE="en_US:en" -e LC_ALL="en_US.UTF-8" -u postgres:postgres sirena sh -c "cat /etc/locale.conf"
# LC_ALL=en_US.UTF-8
# LC_CTYPE=en_US.UTF-8
# LANG=en_US.UTF-8
# docker exec -e LC_CTYPE="en_US.UTF-8" -e LANG="en_US.UTF-8" -e LANGUAGE="en_US:en" -e LC_ALL="en_US.UTF-8" -u postgres:postgres sirena sh -c "cat /etc/locale.gen" | grep en_US
# en_US ISO-8859-1
# # en_US.ISO-8859-15 ISO-8859-15
# en_US.UTF-8 UTF-8

################################################################################
# DOCKER
################################################################################

FIREFOX="jess/firefox"
CHROME="jess/chrome"
CHROMIUM="jess/chromium"
TELEGRAM="xorilog/telegram"
CURL="jess/curl"
VIRTUALBOX="jess/virtualbox"
SKYPE="jess/skype"
THUNDERBIRD="gruen/thunderbird"
EMACS="silex/emacs"
GCC="gcc"
HTTPBIN="jess/httpbin"
GITEA="gitea/gitea"

IMAGES="$FIREFOX $CHROME $CHROMIUM $TELEGRAM $CURL $VIRTUALBOX $SKYPE $THUNDEBIRD $EMACS $GCC $HTTPBIN $GITEA"

user_group() {
    echo "$(id -u $USER):$(id -g $USER)"
}

docker_pull_images() {
    for image in $IMAGES; do
        docker pull $image
    done
    sudo apt install -y curl screen
    curl -fsSL https://raw.githubusercontent.com/mviereck/x11docker/master/x11docker | sudo bash -s -- --update
}

docker_clean_containers() {
    result=$(docker ps -aq -f status=exited)
    count=$(echo $result | wc -w)

    if [ $count -eq 0 ]; then
        return 0
    fi

    echo $result | xargs docker rm
}

docker_clean_images() {
    result=$(docker images -q --filter dangling=true)
    count=$(echo $result | wc -w)

    if [ $count -eq 0 ]; then
        return 0
    fi

    echo $result | xargs docker rmi
}

docker_clean() {
    docker_clean_containers
    docker_clean_images
}

firefox() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} -- -u $(user_group) -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads --network host -- $FIREFOX
}

chrome() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} --user=RETAIN -- --no-sandbox -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $CHROME
}

chromium() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} --user=RETAIN -- --no-sandbox -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $CHROMIUM
}

telegram() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --home --pulseaudio --name ${FUNCNAME[0]} -- -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $TELEGRAM
}

curl() {
    docker run --rm --name ${FUNCNAME[0]} -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads $CURL "$@"
}

virtualbox() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --home --name ${FUNCNAME[0]} -- -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $VIRTUALBOX
}

skype() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --home --name ${FUNCNAME[0]} --pulseaudio -- -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $SKYPE
}

thunderbird() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --home --name ${FUNCNAME[0]} -- -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $THUNDERBIRD
}

gcc() {
    docker run --rm -v $PWD:/usr/src/myapp -w /usr/src/myapp $GCC:latest gcc $@
}

gccs() {
    for i in {5..9}; do
        echo "Compiling with gcc-$i"
        docker run --name ${FUNCNAME[0]}$i --rm -v $PWD:/usr/src/myapp -w /usr/src/myapp $GCC:$i gcc -o "a.out$i" $@
    done
}

httpbin() {
    docker run -d --rm -p 3001:8080 --name ${FUNCNAME[0]} $HTTPBIN
}

gitea() {
    docker run --rm --name ${FUNCNAME[0]} -p 3002:3000 $GITEA
}

emacs() {
    screen -S ${FUNCNAME[0]}2 -dm x11docker --share $HOME --share $HOME/.emacs --share $HOME/.emacs.d --name ${FUNCNAME[0]}2 -- -u $(id -u $USER):$(id -g $USER) -v $HOME/Dropbox/sync/development:$HOME/development -- $EMACS
}
