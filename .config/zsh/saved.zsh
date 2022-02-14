# alias xin="sudo xbps-install -S"
# alias xr="sudo xbps-remove -R"
# alias xs="xbps-query -R --regex -s"

alias gcc='gcc -g -Wall -Wextra -Wfloat-equal -Wundef -Wstrict-prototypes -Wcast-align -Waggregate-return -Wunreachable-code -Wpointer-arith -Wshadow -Wwrite-strings -Wlogical-op -Wmissing-declarations -Wredundant-decls -O3 -foptimize-strlen -ffunction-sections -pipe'
#
alias g++='g++ -std=c++17 -g -Wall -Wextra -Wfloat-equal -Wundef  -Wcast-align -Waggregate-return -Wunreachable-code  -Wpointer-arith -Wshadow -Wwrite-strings -Wlogical-op -Wmissing-declarations -Wredundant-decls -Woverloaded-virtual -Ofast'


#Cleanup orphaned packages
alias cleanup="sudo pacman -Rns $(pacman -Qtdq)"
#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# For Data science,anaconda and jupyter
# alias conda_activate="source /opt/anaconda/bin/activate root"
# alias conda_deactivate="source /opt/anaconda/bin/deactivate root"

# Arco Linux specific

#copy/paste all content of /etc/skel over to home folder - backup of config created - beware
#alias skel='cp -Rf ~/.config ~/.config-backup-$(date +%Y.%m.%d-%H.%M.%S) && cp -rf /etc/skel/* ~'
##backup contents of /etc/skel to hidden backup folder in home/user
#alias bupskel='cp -Rf /etc/skel ~/.skel-backup-$(date +%Y.%m.%d-%H.%M.%S)'

##get fastest mirrors in your neighborhood
# alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
# alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
# alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
# alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

alias gclr='git clone --recurse-submodules'

alias jni="wget -qO- https://gitlab.com/MaxdSre/axd-ShellScript/raw/master/assets/software/Anaconda.sh | sudo bash -s -- -C"
alias jnr="sudo /opt/Anaconda/bin/conda remove"
