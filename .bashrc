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
# shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
# shopt -s checkwinsize

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
# if ! shopt -oq posix; then
#   if [ -f /usr/share/bash-completion/bash_completion ]; then
#     . /usr/share/bash-completion/bash_completion
#   elif [ -f /etc/bash_completion ]; then
#     . /etc/bash_completion
#   fi
# fi

export PLATFORM=m64

[ -r ~/.env ] && . ~/.env

# Example of the `.env` file

export CXX=g++
export CC=gcc
export LOCALCXX=${CXX}
export LOCALCC=${CC}

# export DOT_ENV_EMACS_FONT_SIZE=160
# export DOT_ENV_C_COMPILER=${CC}
# export DOT_ENV_CXX_COMPILER=${CXX}
# export DOT_ENV_ENABLE_GLIBCXX_DEBUG=0
# export DOT_ENV_EDITOR=emacs
# export DOT_ENV_EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs"
# export DOT_ENV_VISUAL=emacs
# export DOT_ENV_LC_ALL=POSIX
# export DOT_ENV_TERM=screen-256color

# export DOT_ENV_SIRENA_PATH=~/1/work
# export DOT_ENV_SIRENA_PATH_TRUNK=${DOT_ENV_SIRENA_PATH}/trunk
# export DOT_ENV_SIRENA_PATH_STABLE=${DOT_ENV_SIRENA_PATH}/stable
# export DOT_ENV_SIRENA_PATH_DOCKER=/sirena_src
# export DOT_ENV_SIRENA_CPU_COUNT=7
# export DOT_ENV_SIRENA_ORACLE_BASE=/u01/app/oracle
# export DOT_ENV_SIRENA_ORACLE_HOME=${ORACLE_BASE}/product/12.2.0/db_1
# export DOT_ENV_SIRENA_ORACLE_SID=orcl
# export DOT_ENV_SIRENA_ORACLE_INVENTORY=${ORACLE_HOME}/inventory
# export DOT_ENV_SIRENA_ORACLE_PATH=${ORACLE_HOME}/bin
# export DOT_ENV_SIRENA_ORACLE_LD_LIBRARY_PATH=${ORACLE_HOME}/lib
# export DOT_ENV_SIRENA_ORACLE_NLS_LANG=AMERICAN_CIS.RU8PC866
# export DOT_ENV_SIRENA_SVN_SSH='ssh -i ~/.ssh/id_rsa -l svn'
# export DOT_ENV_SIRENA_SVN_BASE='svn+ssh://svn/SVNroot/sirena'

################################################################################
# Environment variables
################################################################################

export EMACS_FONT_SIZE=${DOT_ENV_EMACS_FONT_SIZE:-220}
export C_COMPILER=${DOT_ENV_C_COMPILER:-'gcc'}
export CXX_COMPILER=${DOT_ENV_CXX_COMPILER:-'g++'}
export ENABLE_GLIBCXX_DEBUG=${DOT_ENV_ENABLE_GLIBCXX_DEBUG:-0}
export EDITOR=${DOT_ENV_EDITOR:-nano}
export VISUAL=${DOT_ENV_VISUAL:-nano}
export LC_ALL=${DOT_ENV_LC_ALL:-en_US.UTF-8}
export TERM=${DOT_ENV_TERM:-screen-256color}

export SIRENA_PATH=${DOT_ENV_SIRENA_PATH:-}
export SIRENA_PATH_TRUNK=${DOT_ENV_SIRENA_PATH_TRUNK:-}
export SIRENA_PATH_STABLE=${DOT_ENV_SIRENA_PATH_STABLE:-}
export SIRENA_PATH_DOCKER=${DOT_ENV_SIRENA_PATH_DOCKER:-}
export SIRENA_CPU_COUNT=${DOT_ENV_SIRENA_CPU_COUNT:-6}
export ORACLE_BASE=${DOT_ENV_SIRENA_ORACLE_BASE:-}
export ORACLE_HOME=${DOT_ENV_SIRENA_ORACLE_HOME:-}
export ORACLE_SID=${DOT_ENV_SIRENA_ORACLE_SID:-}
export ORACLE_INVENTORY=${DOT_ENV_SIRENA_ORACLE_INVENTORY:-}
export ORACLE_PATH=${DOT_ENV_SIRENA_ORACLE_PATH:-}
export ORACLE_LD_LIBRARY_PATH=${DOT_ENV_SIRENA_ORACLE_LD_LIBRARY_PATH:-}
export ORALCE_NLS_LANG=${DOT_ENV_SIRENA_ORACLE_NLS_LANG:-}
export SVN_SSH=${DOT_ENV_SIRENA_SVN_SSH:-}
export SVN_BASE=${DOT_ENV_SIRENA_SVN_BASE:-}
export PATH=${ORACLE_PATH}:${PATH}
export LD_LIBRARY_PATH=${ORACLE_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}

export CC=${C_COMPILER}
export CXX=${CXX_COMPILER}
export LOCALCC=${C_COMPILER}
export LOCALCXX=${CXX_COMPILER}
export PATH=~/bin:${PATH}
#export PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

################################################################################
# Aliases
################################################################################

alias ..="cd .."

################################################################################
# Helper functions
################################################################################

gcc_version() {
    echo '****************** ' $CC ' configuration **********************\n'
    echo | $CC -xc++ -E -v -
    echo '\nOPTIONS:'
    echo 'ENABLE_GLIBCXX_DEBUG='${ENABLE_GLIBCXX_DEBUG}
    echo 'CC='${CC}
    echo 'CXX='${CXX}
    echo 'LOCALCC='${LOCALCC}
    echo 'LOCALCXX='${LOCALCXX}
    echo '\n*************************************************************\n'
}

################################################################################
# Sirena helper functions
################################################################################

cpu_count() {
    grep -c ^processor /proc/cpuinfo
}

sirena_exec() {
    if [ $# -lt 1 ]; then
        echo "Usage: ${FUNCNAME[0]} command"
        return 1
    fi

    docker exec -e MAKE_J=${SIRENA_CPU_COUNT} -u $(id -u $USER):$(id -g $USER) sirena sh -c ". /root/.bashrc && $@"
}

sirena_exec_user() {
    if [ $# -lt 1 ]; then
        echo "Usage: ${FUNCNAME[0]} command"
        return 1
    fi

    local ARGS=${@:2}
    docker exec -u $1 sirena sh -c ". /root/.bashrc && $ARGS"
}

sirena_exec_user_it() {
    if [ $# -lt 1 ]; then
        echo "Usage: ${FUNCNAME[0]} command"
        return 1
    fi

    local ARGS=${@:2}
    docker exec -u $1 -it sirena sh -c ". /root/.bashrc && $ARGS"
}

sirena_cd_trunk() {
    cd ${SIRENA_PATH_TRUNK}/src
}

sirena_cd_stable() {
    cd $SIRENA_PATH_STABLE/src
}

sirena_sqv() {
    sirena_exec_user_it oracle "sqlplus trunk/trunk"
}

sirena_sqw() {
    sirena_exec_user_it oracle "sqlplus stable/stable"
}

sirena_start_docker() {
    local POSTGRESQL_DATA=/var/lib/postgresql/12/main
    local ORACLE_DATA=$ORACLE_BASE/oradata
    local LOCAL_DB_DATA=~/1/work/db
    local STORAGE="storage.komtex:10.1.90.152"
    local SVN="svn.komtex:10.1.90.222"

    docker run \
           --add-host $STORAGE \
           --add-host $SVN \
           --privileged \
           --rm \
           --name sirena \
           --volume /var/run/dbus/system_bus_socket:/var/run/dbus/system_bus_socket:rw \
           -d \
           -u $(id -u $USER):$(id -g $USER) \
           -v $PWD:${SIRENA_PATH_DOCKER} \
           -v $LOCAL_DB_DATA/oracle:$ORACLE_DATA \
           -v $LOCAL_DB_DATA/postgresql:$POSTGRESQL_DATA \
           sirena/dev sh -c "trap : TERM INT; sleep infinity & wait"
    #sirena_exec_user root:root "hostname idfumg"
}

sirena_stop_docker() {
    docker stop sirena
}

sirena_start_postgresql() {
    sirena_exec_user root:root "service postgresql start"
    sirena_postgresql_command "SET TIME ZONE 'Europe/Moscow';"
    sirena_postgresql_command "ALTER DATABASE postgres SET timezone TO 'Europe/Moscow';"
    sirena_postgresql_command "SHOW timezone;"

    # sudo grep zone ../db/postgresql/postgresql.conf
    # log_timezone = 'Europe/Moscow'
    # timezone = 'Europe/Moscow'
}

sirena_stop_postgresql() {
    sirena_exec_user root:root "service postgresql stop"
}

sirena_postgresql_command() {
    if [ ! $# -eq 1 ]; then
       echo "Usage: ${FUNCNAME[0]} <postgresql sql statement>"
       return 1
    fi

    sirena_exec_user postgres:postgres "psql -c \"$1\""
}

sirena_oracle_command() {
    if [ ! $# -eq 1 ]; then
       echo "Usage: ${FUNCNAME[0]} <oracle sql statement>"
       return 1
    fi

    sirena_exec_user oracle "echo \"$1\" > /root/start.sql && sqlplus / as sysdba @/root/start.sql"
}

sirena_data_postgres() {
    sirena_postgresql_command "SHOW data_directory;"
}

sirena_data_oracle() {
    sirena_oracle_command "select distinct regexp_substr(name,'^.*\\') from v\\\$datafile;"
}

sirena_start_oracle() {
    sirena_oracle_command "ALTER DATABASE SET TIME_ZONE='Europe/Moscow';"
    sirena_oracle_command "startup;"
    sirena_oracle_command "ALTER USER system IDENTIFIED BY manager ACCOUNT UNLOCK;"
    sirena_oracle_command "ALTER SYSTEM SET open_cursors = 2500 SCOPE=BOTH;"
    sirena_oracle_command "GRANT CREATE SESSION TO stable;"
    sirena_oracle_command "GRANT CREATE SESSION TO trunk;"
    sirena_oracle_command "GRANT CREATE SESSION TO astra;"
    sirena_oracle_command "ALTER SESSION SET TIME_ZONE='Europe/Moscow';"
    sirena_oracle_command "SELECT dbtimezone FROM DUAL;"
    sirena_oracle_command "SELECT sessiontimezone FROM DUAL;"
    sirena_oracle_command "select systimestamp from dual;"
}

sirena_stop_oracle() {
    sirena_oracle_command "shutdown;"
}

sirena_start() {
    if [ ! -e "$PWD/buildFromScratch.sh" ]; then
        echo "Error! It's not a project root directory!"
        return 1
    fi

    sirena_start_docker
    sirena_start_postgresql & sirena_start_oracle
}

sirena_stop() {
    sirena_stop_postgresql & sirena_stop_oracle
    sirena_stop_docker
}

sirena_is_running() {
    docker ps -a | grep sirena > /dev/null
}

sirena_wait_while_docker_removing_container() {
    sirena_is_running
    while [ $? -eq 0 ]; do
        sleep 1
        sirena_is_running
    done
}

sirena_stop_if_running() {
    sirena_is_running
    if [ $? -eq 0 ]; then
        sirena_stop
        sirena_wait_while_docker_removing_container
    fi
}

sirena_restart() {
    sirena_stop_if_running
    sirena_start
}

sirena_init_postgres_build_db() {
    local POSTGRESQL_DIR="/var/lib/postgresql"
    local POSTGRESQL_DATA="$POSTGRESQL_DIR/12/main"

    docker exec -u root:root sirena sh -c "rm -fr $POSTGRESQL_DATA/*"
    docker exec -u root:root sirena sh -c "chown postgres:postgres -R $POSTGRESQL_DIR"
    docker exec -u postgres:postgres sirena sh -c "/usr/lib/postgresql/12/bin/initdb -D $POSTGRESQL_DATA"
}

sirena_init_postgres() {
    sirena_init_postgres_build_db
    sirena_start_postgresql
    sirena_postgresql_command "create user system encrypted password 'manager' superuser;"
    sirena_postgresql_command "create user etick_test encrypted password 'etick' superuser;"
    sirena_postgresql_command "create user astra encrypted password 'astra' superuser;"
    sirena_postgresql_command "SET TIME ZONE 'Europe/Moscow';"
    sirena_postgresql_command "ALTER DATABASE postgres SET timezone TO 'Europe/Moscow';"
    sirena_postgresql_command "SHOW timezone;"
    sirena_stop_postgresql
}

sirena_init_oracle_copy_db() {
    docker exec -u root:root sirena sh -c "chown oracle:oinstall -R /oracle"
    docker exec -u oracle:oinstall sirena sh -c "cp -r $ORACLE_DATA/orcl /oracle"
}

sirena_init_oracle() {
    local ORACLE_DATA=$ORACLE_BASE/oradata

    sirena_start_oracle
    sirena_oracle_command "ALTER SYSTEM SET open_cursors = 2500 SCOPE=BOTH;"
    sirena_oracle_command "ALTER USER SYSTEM IDENTIFIED BY MANAGER ACCOUNT UNLOCK;"
    sirena_oracle_command "ALTER DATABASE SET TIME_ZONE='Europe/Moscow';"
    sirena_oracle_command "ALTER SESSION SET TIME_ZONE='Europe/Moscow';"
    sirena_oracle_command "SELECT dbtimezone FROM DUAL;"
    sirena_oracle_command "SELECT sessiontimezone FROM DUAL;"
    sirena_oracle_command "select systimestamp from dual;"
    sirena_init_oracle_copy_db
    sirena_stop_oracle
}

sirena_init_docker() {
    local POSTGRESQL_DATA=/var/lib/postgresql/12/main
    local LOCAL_DB_DATA=~/1/work/db

    docker run \
           --network host \
           --privileged \
           --rm \
           --name sirena \
           -d \
           -u $(id -u $USER):$(id -g $USER) \
           -v $PWD:${SIRENA_PATH_DOCKER} \
           -v $LOCAL_DB_DATA/oracle:/oracle \
           -v $LOCAL_DB_DATA/postgresql:$POSTGRESQL_DATA \
           sirena/dev sh -c "trap : TERM INT; sleep infinity & wait"

    sirena_init_postgres & sirena_init_oracle
    sirena_stop_docker
}

sirena_build() {
    local GCC_VERSIONS="
export LOCALCC='gcc-8 -fsanitize=address'
export LOCALCXX='g++-8 -fsanitize=address'
export CC='gcc-8 -fsanitize=address'
export CXX='g++-8 -fsanitize=address'"

    local SIRENA_BUILD_VARS="LOCALCC='gcc-8 -fsanitize=address' LOCALCXX='g++-8 -fsanitize=address' CC='gcc-8 -fsanitize=address' CXX='g++-8 -fsanitize=address' BUILD_TESTS=1 ENABLE_SHARED=1 ENABLE_GLIBCXX_DEBUG=${ENABLE_GLIBCXX_DEBUG} LANG=en_US.UTF-8 LANGUAGE=en_US PLATFORM=m64"

    local gcc_version="
    echo CC=\${CC}
    echo CXX=\${CXX}
    echo LOCALCC=\${LOCALCC}
    echo LOCALCXX=\${LOCALCXX}
    echo ''
    echo ''
    echo 'Compiler configuration and include paths'
    echo \$(\${CC} -xc++ -E -v -)
    echo ''
    echo ''
    echo ''"

    local build_command="$GCC_VERSIONS && $gcc_version && cd ${SIRENA_PATH_DOCKER} && echo Path: \${PWD} && $SIRENA_BUILD_VARS ./buildFromScratch.sh"

    local database_login=$1
    local database_password=$2

    if [ -z $database_login ] || [ -z $database_password ]; then
        sirena_exec "$build_command"
        return 0
    fi

    if [ $# -ge 2 ]; then
        local ARGS=${@:3}
        if [ -n $database_login ] && [ -n $database_password ]; then
            sirena_exec "$build_command $database_login/$database_password $ARGS"
            return 0
        fi
    fi

    echo "Error! Wrong function parameters!"
    return 1
}

sirena_build_clang() {
    local GCC_VERSIONS="
export LOCALCC='clang-6.0 -fsanitize=address'
export LOCALCXX='clang++-6.0 -fsanitize=address'
export CC='clang-6.0 -fsanitize=address'
export CXX='clang++-6.0 -fsanitize=address'"

    local SIRENA_BUILD_VARS="LOCALCC='clang-6.0 -fsanitize=address' LOCALCXX='clang++-6.0 -fsanitize=address' CC='clang-6.0 -fsanitize=address' CXX='clang++-6.0 -fsanitize=address' BUILD_TESTS=1 ENABLE_SHARED=1 ENABLE_GLIBCXX_DEBUG=${ENABLE_GLIBCXX_DEBUG} LANG=en_US.UTF-8 LANGUAGE=en_US PLATFORM=m64"

    local gcc_version="
    echo CC=\${CC}
    echo CXX=\${CXX}
    echo LOCALCC=\${LOCALCC}
    echo LOCALCXX=\${LOCALCXX}
    echo ''
    echo ''
    echo 'Compiler configuration and include paths'
    echo \$(\${CC} -xc++ -E -v -)
    echo ''
    echo ''
    echo ''"

    local build_command="$GCC_VERSIONS && $gcc_version && cd ${SIRENA_PATH_DOCKER} && echo Path: \${PWD} && $SIRENA_BUILD_VARS ./buildFromScratch.sh"

    local database_login=$1
    local database_password=$2

    if [ -z $database_login ] || [ -z $database_password ]; then
        sirena_exec "$build_command"
        return 0
    fi

    if [ $# -ge 2 ]; then
        local ARGS=${@:3}
        if [ -n $database_login ] && [ -n $database_password ]; then
            sirena_exec "$build_command $database_login/$database_password $ARGS"
            return 0
        fi
    fi

    echo "Error! Wrong function parameters!"
    return 1
}

astra_build() {
    local GCC_VERSIONS="
export LOCALCC=gcc-8
export LOCALCXX=g++-8
export CC=gcc-8
export CXX=g++-8"

    local ASTRA_BUILD_VARS="ENABLE_ORACLE=1 XP_NO_RECHECK=1 XP_LIST_EXCLUDE=SqlUtil,Serverlib,httpsrv,httpsrv_ext,ssim PG_HOST=${PG_HOST:-localhost} CPP_STD_VERSION=c++17 PLATFORM=m64 BUILD_TESTS=1 ENABLE_SHARED=1 LANG=en_US.UTF-8 LANGUAGE=en_US SYSPAROL=system/manager MY_LOCAL_CFLAGS=\"-Wno-error=maybe-uninitialized -gz\" MY_LOCAL_LDFLAGS=\"-gz\""

    local gcc_version="
    echo CC=\${CC}
    echo CXX=\${CXX}
    echo LOCALCC=\${LOCALCC}
    echo LOCALCXX=\${LOCALCXX}
    echo ''
    echo ''
    echo 'Compiler configuration and include paths'
    echo \$(\${CC} -xc++ -E -v -)
    echo ''
    echo ''
    echo ''"

    local build_command="$GCC_VERSIONS && $gcc_version && cd ${SIRENA_PATH_DOCKER} && echo Path: \${PWD} && $ASTRA_BUILD_VARS ./buildFromScratch.sh"

    local build_modules="--build_external_libs --configlibs --buildlibs --configastra --buildastra --createtcl --createdb"

    local database_login=$1
    local database_password=$2

    if [ -z $database_login ] || [ -z $database_password ]; then
        sirena_exec "$build_command"
        return 0
    fi

    if [ $# -ge 2 ]; then
        local ARGS=${@:3}
        if [ -n $database_login ] && [ -n $database_password ]; then
            sirena_exec "$build_command $database_login/$database_password $ARGS"
            return 0
        fi
    fi

    echo "Error! Wrong function parameters!"
    return 1
}

sirena_build_trunk() {
    sirena_build trunk trunk
}

astra_build_trunk() {
    astra_build astra_trunk astra_trunk
}

sirena_build_stable() {
    sirena_build stable stable
}

astra_build_stable() {
    astra_build astra_stable astra_stable
}

sirena_build_trunk_db() {
    sirena_build trunk trunk --createdb
}

astra_build_trunk_db() {
    astra_build astra_trunk astra_trunk --createtcl --createdb
}

sirena_build_stable_db() {
    sirena_build stable stable --createdb
}

sirena_make_sql_change() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/sql && ./make_sql_change trunk/trunk $@"
}

sirena_make() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && make -sj ${SIRENA_CPU_COUNT} $@"
}

astra_make() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && make -sj ${SIRENA_CPU_COUNT} $@"
}

sirena_make_libs() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/sirenalibs && make -sj ${SIRENA_CPU_COUNT} $@"
}

sirena_make_serverlib() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/sirenalibs/serverlib && make -sj ${SIRENA_CPU_COUNT} $@"
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

sirena_make_emd() {
    sirena_make "emd"
}

sirena_clean() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && make -C $@ clean"
}

astra_clean() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && make -C $@ clean"
}

sirena_clean_libs() {
    sirena_exec "cd ${SIRENA_PATH_DOCKER}/sirenalibs && make clean"
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

sirena_clean_emd() {
    sirena_clean "emd"
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

sirena_rebuild_emd() {
    sirena_clean_emd && sirena_make_emd
}

sirena_sql_apply_alter() {
    if [ ! $# -eq 2 ]; then
       echo "Usage: ${FUNCNAME[0]} sql_filename login/password"
       return 1
    fi

    local SQL_FILENAME=$1
    local DB_LOGIN_PASSWORD=$2

    sirena_exec_user oracle "echo '@/sirena_src/$SQL_FILENAME' > /root/start.sql && sqlplus $DB_LOGIN_PASSWORD @/root/start.sql"
}

astra_sql_apply_alter() {
    if [ ! $# -eq 2 ]; then
       echo "Usage: ${FUNCNAME[0]} sql_filename login/password"
       return 1
    fi

    local SQL_FILENAME=$1
    local DB_LOGIN_PASSWORD=$2

    sirena_exec_user oracle "echo '@/sirena_src/$SQL_FILENAME' > /root/start.sql && sqlplus $DB_LOGIN_PASSWORD @/root/start.sql"
}

sirena_sql_apply_fdat() {
    if [ ! $# -eq 2 ]; then
       echo "Usage: ${FUNCNAME[0]} fdat_filename login/password"
       return 1
    fi

    local FDAT_FILENAME=$1
    local DB_LOGIN_PASSWORD=$2

    sirena_exec_user oracle "cd ${SIRENA_PATH_DOCKER} && sqlldr $DB_LOGIN_PASSWORD $FDAT_FILENAME"
}

sirena_ts() {
    local FILENAME=$1
    local ARGS=${@:2}

    if [ $# -lt 1 ]; then
        echo "Usage: ${FUNCNAME[0]} test_name [test_number test_number ...]"
        return 1
    fi

    if [ $# -eq 1 ]; then
        sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && ./tscript.sh $FILENAME"
        return 0
    fi

    if [ $# -gt 1 ]; then
        for num in $ARGS; do
            sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && ./tscript.sh $FILENAME $num"
        done
        return 0
    fi
}

astra_ts() {
    local FILENAME=$1
    local ARGS=${@:2}

    if [ $# -lt 1 ]; then
        echo "Usage: ${FUNCNAME[0]} test_name [test_number test_number ...]"
        return 1
    fi

    if [ $# -eq 1 ]; then
        sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && ./tscript.sh $FILENAME"
        return 0
    fi

    if [ $# -gt 1 ]; then
        for num in $ARGS; do
            sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && ./tscript.sh $FILENAME $num"
        done
        return 0
    fi
}

sirena_test() {
    if [ ! $# -eq 1 ] && [ ! $# -eq 2 ]; then
        echo "Usage: ${FUNCNAME[0]} module_name [test_name]"
        return 1
    fi

    if [ $# -eq 1 ]; then
        sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && XP_LIST=$1 make xp-tests"
        return 0
    fi

    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && XP_LIST=$1.$2 make xp-tests"
}

astra_test() {
    if [ $# -eq 2 ]; then
        sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && XP_LIST=$1.$2 make xp-tests"
        return 0
    fi

    sirena_exec "cd ${SIRENA_PATH_DOCKER}/src && XP_LIST_EXCLUDE=SqlUtil,Serverlib,httpsrv,httpsrv_ext,ssim make xp-tests-local"
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

sirena_test_emd() {
    sirena_test "emd" $@
}

sirena_rsync() {
    rsync -vu rail/*{cc,h} apushkin@test:/home/tst/sirena/src/rail/
}

sirena_rsync_rail_files() {
    if [ $# -eq 0 ]; then
        echo "Usage: ${FUNCNAME[0]} [files_names]"
        return 1
    fi

    rsync -vu $@ idfumg@10.1.7.94:/home/idfumg/1/work/sirena_trunk/src/rail/
}

sirena_rsync_src_files() {
    if [ $# -eq 0 ]; then
        echo "Usage: ${FUNCNAME[0]} [files_names]"
        return 1
    fi

    rsync -vu $@ idfumg@10.1.7.94:/home/idfumg/1/work/sirena_trunk/src/
}

sirena_rsync_rail_pg_tables() {
    if [ $# -eq 0 ]; then
        echo "Usage: ${FUNCNAME[0]} [files_names]"
        return 1
    fi

    rsync -vu sql/bases/sirena_pg/bases_pg/rail/1Tab/$@ idfumg@10.1.7.94:/home/idfumg/1/work/sirena_trunk/sql/bases/sirena_pg/bases_pg/rail/1Tab/
}

sirena_rsync_rail_pg_indexes() {
    if [ $# -eq 0 ]; then
        echo "Usage: ${FUNCNAME[0]} [files_names]"
        return 1
    fi

    rsync -vu sql/bases/sirena_pg/bases_pg/rail/1Tind/$@ idfumg@10.1.7.94:/home/idfumg/1/work/sirena_trunk/sql/bases/sirena_pg/bases_pg/rail/1Tind/
}

sirena_rsync_rail_pg_seqs() {
    if [ $# -eq 0 ]; then
        echo "Usage: ${FUNCNAME[0]} [files_names]"
        return 1
    fi

    rsync -vu sql/bases/sirena_pg/bases_pg/rail/3Seq/$@ idfumg@10.1.7.94:/home/idfumg/1/work/sirena_trunk/sql/bases/sirena_pg/bases_pg/rail/3Seq/
}

sirena_connect_test() {
    ssh -A apushkin@test
}

sirena_connect_grs() {
    ssh -A apushkin@grsbuild
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
DROPBOX="janeczku/dropbox"
ELIXIR="elixir"

IMAGES="$FIREFOX $CHROME $CHROMIUM $TELEGRAM $CURL $VIRTUALBOX $SKYPE $THUNDEBIRD $EMACS $GCC $HTTPBIN $GITEA $DROPBOX $ELIXIR"

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

# firefox() {
#     screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} -- -u $(user_group) -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads --network host -- $FIREFOX
# }

chrome() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} --user=RETAIN -- --no-sandbox -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $CHROME
}

chromium() {
    screen -S ${FUNCNAME[0]} -dm x11docker --gpu --pulseaudio --home --name ${FUNCNAME[0]} --user=RETAIN -- --no-sandbox -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $CHROMIUM
}

# telegram() {
#     screen -S ${FUNCNAME[0]} -dm x11docker --gpu --home --pulseaudio --name ${FUNCNAME[0]} -- -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads -- $TELEGRAM
# }

# curl() {
#     docker run --network host --rm --name ${FUNCNAME[0]} -w $HOME/Downloads -v $HOME/Downloads:$HOME/Downloads $CURL $@
# }

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

# dropbox() {
#     local DROPBOX_DIR="$HOME/Dropbox"

#     if [ -e "/Dropbox" ]; then
#         local DROPBOX_DIR="/Dropbox"
#         ln -sf /Dropbox/Dropbox $HOME/Dropbox
#     fi

#     docker run -d --restart=always --name=dropbox -v $DROPBOX_DIR:/dbox/Dropbox --net=host -e DBOX_UID=$(id -u $USER) -e DBOX_GID=$(id -g $USER) -v $HOME/.dropbox:/dbox/.dropbox $DROPBOX
# }

# dropbox_status() {
#     docker exec -t -i dropbox dropbox status
# }

# emacs() {
#     screen -S ${FUNCNAME[0]} -dm x11docker --share $HOME --share $HOME/.emacs --share $HOME/.emacs.d --name ${FUNCNAME[0]} -- -u $(id -u $USER):$(id -g $USER) -e SIRENA_PATH_TRUNK -e SIRENA_PATH_STABLE -v /Dropbox:/Dropbox -- $EMACS $@
# }

# elixir_init() {
#     docker run -d -u $(user_group) --network host --rm --name elixir -v $PWD:/src -w /src elixir iex
# }

# elixir_exec() {
#     docker exec elixir $@
# }

# elixir() {
#     elixir_exec elixir $@
# }

# iex() {
#     elixir_exec iex $@
# }

# mix() {
#     elixir_exec mix $@
# }

################################################################################
# GITHUB
################################################################################

MY_CONFIGS="https://github.com/idfumg/MyConfigs.git"
ELIXIR_SYNOPSIS="https://github.com/idfumg/ElixirSynopsis.git"
LUA_SYNOPSIS="https://github.com/idfumg/LuaSynopsis.git"
QT_SYNOPSIS="https://github.com/idfumg/QtSynopsis.git"
PYTHON_SYNOPSIS="https://github.com/idfumg/PythonSynopsis.git"
EMACS_SYNOPSIS="https://github.com/idfumg/EmacsSynopsis.git"
VK_PYTHON_STATS="https://github.com/idfumg/VkPythonStats.git"
ASM_LEARNING="https://github.com/idfumg/AsmLearning.git"
OPENSSL_GOST_WRAPPER="https://github.com/idfumg/OpensslGostWrapper.git"
SAILFISH_SECRETS_APIE_XAMPLE="https://github.com/idfumg/SailfishSecretsApiExample.git"
SAILFISH_SECRETS_GOST_PLUGIN="https://github.com/idfumg/sailfishsecrets-gost-plugin.git"
SYSTEM_CALLS_LEARNING="https://github.com/idfumg/SystemCallsLearning.git"
CPP_UTILS="https://github.com/idfumg/CppUtils.git"
XML11="https://github.com/idfumg/xml11.git"
PLANETS_OF_THE_UNIVERSE="https://github.com/idfumg/planets-of-the-universe.git"
CREQUESTS="https://github.com/idfumg/crequests.git"
STEVENS_SYNOPSIS="https://github.com/idfumg/StevensSynopsis.git"
PNR_PARSE="https://github.com/idfumg/PnrParse.git"
PATTERNS_SYNOPSIS="https://github.com/idfumg/PatternsSynopsis.git"
PERSONAL_BLOG="https://github.com/idfumg/PersonalBlog.git"
PERSONAL_PAGE="https://github.com/idfumg/PersonalPage.git"
ALGORITHMS_SYNOPSIS="https://github.com/idfumg/AlgorithmsSynopsis.git"
GULP_TEMPLATE="https://github.com/idfumg/GulpTemplate.git"
MONITOR_WEB="https://github.com/idfumg/MonitorWeb.git"
JAVASCRIPT_SYNOPSIS="https://github.com/idfumg/JavaScriptSynopsis.git"
MP11="https://github.com/idfumg/mp11.git"
CPP_PROXY_CONTAINERS="https://github.com/idfumg/CppProxyContainers.git"
SERIES_FINDER_SELENIUM="https://github.com/idfumg/series-finder-selenium.git"
VK_MUSIC_SELENIUM_BS="https://github.com/idfumg/vk-music-selenium-bs.git"
VK_MUSIC="https://github.com/idfumg/vk-music.git"
FLASK_OIDC="https://github.com/idfumg/flask-oidc.git"
EMACS_D="https://github.com/idfumg/emacs.d.git"
FLASK_AUTOINDEX="https://github.com/idfumg/flask-autoindex.git"
FLASK_BCRYPT_USAGE="https://github.com/idfumg/flask-bcrypt-usage.git"
FLASK_WTF_USAGE="https://github.com/idfumg/flask-wtf-usage.git"
FLASK_LOGIN_USAGE="https://github.com/idfumg/flask-login-usage.git"
FLASK_DEFAULT_TREE="https://github.com/idfumg/flask-default-tree.git"
SQLALCHEMY_TUTORIAL_EXAMPLES="https://github.com/idfumg/SQLAlchemy_tutorial_examples.git"

GITHUB_REPOS="
$MY_CONFIGS
$ELIXIR_SYNOPSIS
$LUA_SYNOPSIS
$QT_SYNOPSIS
$PYTHON_SYNOPSIS
$EMACS_SYNOPSIS
$VK_PYTHON_STATS
$ASM_LEARNING
$OPENSSL_GOST_WRAPPER
$SAILFISH_SECRETS_APIE_XAMPLE
$SAILFISH_SECRETS_GOST_PLUGIN
$SYSTEM_CALLS_LEARNING
$CPP_UTILS
$XML11
$PLANETS_OF_THE_UNIVERSE
$CREQUESTS
$STEVENS_SYNOPSIS
$PNR_PARSE
$PATTERNS_SYNOPSIS
$PERSONAL_BLOG
$PERSONAL_PAGE
$ALGORITHMS_SYNOPSIS
$GULP_TEMPLATE
$MONITOR_WEB
$JAVASCRIPT_SYNOPSIS
$MP11
$CPP_PROXY_CONTAINERS
$SERIES_FINDER_SELENIUM
$VK_MUSIC_SELENIUM_BS
$VK_MUSIC
$FLASK_OIDC
$EMACS_D
$FLASK_AUTOINDEX
$FLASK_BCRYPT_USAGE
$FLASK_WTF_USAGE
$FLASK_LOGIN_USAGE
$FLASK_DEFAULT_TREE
$SQLALCHEMY_TUTORIAL_EXAMPLES"

github_initiate() {
    local GITHUB_DIR=$HOME/1/github

    echo "Copying repositories into $GITHUB_DIR ..." && echo
    mkdir -p $GITHUB_DIR && cd $GITHUB_DIR

    for repo in $GITHUB_REPOS; do
        echo "Copying $repo"
        git clone -q $repo
    done

    cd -
    echo && echo "Done"
}

github_pull() {
    local GITHUB_DIR=$HOME/1/github

    initial_dir = pwd

    echo "Update repositories in $GITHUB_DIR ..." && echo
    mkdir -p $GITHUB_DIR && cd $GITHUB_DIR

    for repo in `ls`; do
        echo "Updating $repo"
        cd $repo
        git pull -q
        cd -
    done

    cd $initial_dir
    echo && echo "Done"
}

################################################################################
# Utils
################################################################################

utils_ports_local() {
    sudo netstat -tulpn
}

utils_ports_remote() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} host"
        return 1
    fi

    sudo nmap -sTU -O $@
}

utils_ports_opened() {
    if [ $# -eq 0 ]; then
        utils_ports_local | grep LISTEN
        return 0
    fi

    utils_ports_remote $@
}

utils_ports_established() {
    sudo lsof -i -P -n | grep ESTABLISHED
}

utils_backup() {
    if [ ! $# -eq 2 ] && [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} filename or ${FUNCNAME[0]} directory filename"
        return 1
    fi

    if [ $# -eq 2 ]; then
        tar cfvz $1/$2.tar.gz $2
        return 0
    fi

    tar cfvz $1.tar.gz $1
}

utils_backup_show_files() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} filename"
        return 1
    fi

    tar --list --verbose --file=$1
}

utils_backup_to_dropbox() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} filename"
        return 1
    fi

    utils_backup $HOME/Dropbox/sync/development $1
}

utils_restore() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} filename"
        return 1
    fi

    tar xfzv $1
}

utils_restore_from_dropbox() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} filename"
        return 1
    fi

    cd $HOME/1
    utils_restore $HOME/Dropbox/sync/development/$1.tar.gz
    cd -
}

utils_backup_dropbox() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} dropbox_directory"
        return 1
    fi

    local DESTINATION=$HOME/1

    rm -fr $DESTINATION/$1
    mkdir -p $DESTINATION/$1
    cd
    utils_backup $DESTINATION/$1 $1
    cd -
}

declare -a CONFIG_FILES=(
.bashrc
.emacs
)

CONFIG_DESTINATION=~/1/github/MyConfigs/home

utils_backup_config() {
    cd $CONFIG_DESTINATION

    for file in "${CONFIG_FILES[@]}"; do
        cp -v $HOME/$file $CONFIG_DESTINATION/$file
    done

    echo "[git pull]" && echo
    git pull

    if [ -z $(git status -s) ]; then
        echo "Nothing to do. Exit" && echo
        return 0;
    fi

    echo && echo && echo "[git diff]" && echo
    git diff

    echo && echo && echo "[git status]" && echo
    git add .bashrc .emacs
    git status

    echo && echo && echo "[git commit and push]" && echo
    git commit -m "Update configs"
    git push origin

    cd -
}

utils_restore_config() {
    cd $CONFIG_DESTINATION

    echo "[git pull]" && echo
    git pull

    echo "[copying files]" && echo

    for file in "${CONFIG_FILES[@]}"; do
        cp -v $CONFIG_DESTINATION/$file $HOME/$file
    done

    cd -
}

export LIBGL_ALWAYS_INDIRECT=1

# git clone https://github.com/magicmonty/bash-git-prompt.git ~/.bash-git-prompt --depth=1
# if [ -f"$HOME/.bash-git-prompt/gitprompt.sh" ]; then
#    GIT_PROMPT_ONLY_IN_REPO=1
#    source $HOME/.bash-git-prompt/gitprompt.sh
# fi

alias git_fetch_with_prune="git fetch --all --prune"

export LC_ALL=en_US.UTF-8

# Golang
export GOROOT=/usr/local/go
export PATH=$GOROOT/bin:$PATH
export GOPATH=~/.local/go
export GOBIN=$GOPATH/bin
export PATH=$GOBIN:$PATH

# Highlighting
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
alias cat="bat --paging=never"
alias bathelp='bat --plain --language=help'
help() {
    "$@" --help 2>&1 | bathelp
}
batdiff() {
    git diff --name-only --relative --diff-filter=d | xargs bat --diff
}

# ls
alias ls="exa -a --group-directories-first --icons --colour-scale -F -b --color=always"
alias lls="exa --all --icons --colour-scale -l -F -b -g -h --time-style=long-iso --color=always --sort=modified"

# curl
alias curl="xh"

# tree
alias tree="tre"

# du
alias du="dust"

# ps
alias ps="procs -i TcpPort -i UdpPort -i State -i VmRss -i StartTime -i ReadBytes -i WriteBytes"

# ping
alias ping="gping"

# top
alias top="btm"

# find
#alias find="fd"

# df
alias df="duf --theme dark --all"

# tail and grc highlighting
alias tail="grc tail"

# grep and grc highlighting
alias grep="grc grep"

# mkcd() {
#     mkdir -p $1 & cd $1
# }

# Load Angular CLI autocompletion.
# source <(ng completion script)
