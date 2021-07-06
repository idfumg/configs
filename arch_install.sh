#!/usr/bin/env bash

arch_start() {
    # fdisk -l
    # fdisk /dev/sdx # substitude your disk number
    # p # to list all drives and their names
    # g # to create a new empty GPT partition table (always the first 2000 bytes)
    # n # to create a new partition
    # +100M # to allocate 100M from the starting sector (from 2000) to the UEFI
    # t # to choose the partition type (1 - for EFI System)
    # n # to create a new partition for the remaining space
    # t # to choose the partition type (30 - for Linux LVM)
    # w # write the changes and exit
}

arch_install_fs_and_lvm() {
    if [ ! $# -eq 2 ]; then
        echo "Usage: ${FUNCNAME[0]} partition_name"
    fi

    local ARCH_DISK_UEFI=$1
    local ARCH_DISK_LVM=$2

    # make a filesystem for the first UEFI partition
    mkfs.fat -F32 /dev/$ARCH_DISK_UEFI
    # create a partition for LVM
    pvcreate --dataalignment 1m /dev/$ARCH_DISK_LVM
    # assign a ARCH_DISK_LVM partition to be a part of vgroup
    vgcreate volgroup0 /dev/$ARCH_DISK_LVM
    # create logical volume for the root
    lvcreate -L 30Gb vgroup0 -n lv_root
    # create the second logical volume for home
    lvcreate -l 100%FREE vgroup0 -n lv_home
    # load kernel module into a kernel for working with LVM
    modprobe dm_mod
    # must find the created volume group
    vgscan
    # activate our two created logical volumes
    vgchange -ay
    # create the ext4 filesystem for the root partition
    mkfs.ext4 /dev/vgroup0/lv_root
    # create the ext4 filesystem for the home partition
    mkfs.ext4 /dev/vgroup0/lv_home
}

arch_install_chroot_directories_hierarchy() {
    mkdir -p /mnt/home
    mkdir -p /mnt/etc
    mkdir -p /mnt/boot
}

arch_mount_partitions_to_chroot_fs() {
    local ARCH_DISK_UEFI=$1

    mount /dev/vgroup0/lv_root /mnt
    mount /dev/vgroup0/lv_home /mnt/home
    mount /dev/$ARCH_DISK_UEFI /mnt/boot/EFI
    # create the fstab file for the mounted in the /mnt partitions
    genfstab -U -p /mnt >> /mnt/etc/fstab
}

arch_install_bases_and_go_to_the_chroot_jail() {
    pacstrap -i /mnt base
    arch-chroot /mnt
}

arch_install_packages() {
    # enable multilib repository
    sed -i "s/#\[multilib\]/\[multilib\]\nInclude\ =\ \/etc\/pacman\.d\/mirrorlist/g" /etc/pacman.conf

    pacman -Syy

    # system
    pacman -S --noconfirm base-devel nano dialog lvm2 sudo

    # linux
    pacman -S --noconfirm linux linux-headers linux-lts linux-lts-headers linux-firmware

    # network
    pacman -S --noconfirm wpa_supplicant wireless_tools netctl networkmanager network-manager-applet net-tools ethtool
    systemctl enable NetworkManager

    # grub packages
    pacman -S --noconfirm grub dosfstools os-prober mtools efibootmgr

    # graphics card
    pacman -S --noconfirm nvidia nvidia-lts nvidia-utils nvidia-settings nvtop

    # graphics card packages for the virtualbox
    # pacman -S virtualbox-guest-utils xf86-video-vmware
    # systemctl enable vboxservice # for the virtual box graphics card drivers

    # cpu
    pacman -S --noconfirm intel-ucode

    # X
    pacman -S --noconfirm xorg-server xfce4 xfce4-goodies xf86-video-nouveau

    # audio
    pacman -S --noconfirm pulseaudio

    # utilities
    pacman -S --noconfirm emacs pavucontrol firefox gnome-keyring lshw neofetch git grub-customizer bash-completion wget

    # lightdm login manager
    # pacman -S lightdm lightdm-gtk-greeter
    # systemctl enable lightdm

    # yay
    git clone https://aur.archlinux.org/yay.git /tmp/yay
    cd /tmp/yay
    makepkg -is --noconfirm

    # ly
    yay -S --noconfirm ly
    sed -i 's/#asterisk = o/asterisk = o/g' /etc/ly/config.ini
    sed -i 's/#hide_borders = true/hide_borders = true/g' /etc/ly/config.ini
    sed -i 's/#lang = en/lang = en/g' /etc/ly/config.ini
    sed -i 's/#load = true/load = true/g' /etc/ly/config.ini
    sed -i 's/#save = true/save = true/g' /etc/ly/config.ini
    systemctl enable ly

    # betterlockscreen
    yay -S --noconfirm betterlockscreen
    betterlockscreen -u ~/Downloads/image.jpg
    xfconf-query -c xfce4-session -p /general/LockCommand -s "betterlockscreen -l" --create -t string

    # openssh
    pacman -S --noconfirm openssh
    systemctl enable sshd
}

arch_install_swap() {
    dd if=/dev/zero of=/swapfile bs=1M count=2048 status=progress # fill with 0s
    chmod 600 /swapfile
    mkswap /swapfile # make a special swap stuff in it
    cp /etc/fstab /etc/fstab.bak
    echo '/swapfile none swap sw 0 0' | tee -a /etc/fstab
    mount -a # no errors
    swapon -a # activate swap
    free -m # now swap is activated
}

arch_install_timezone() {
    timedatectl list-timezones
    timedatectl set-timezone Europe/Moscow
    systemctl enable systemd-timesyncd # daemon will synchronize our clock
}

arch_install_hostname() {
    if [ ! $# -eq 1 ]; then
        echo "Usage: ${FUNCNAME[0]} hostname"
    fi

    local ARCH_HOSTNAME=$1
    hostnamectl set-hostname $ARCH_HOSTNAME
    # default hostname resolutions
    echo '127.0.0.1 localhost' | tee -a /etc/hosts
    echo '::1       $ARCH_HOSTNAME'  | tee -a /etc/hosts
    echo '127.0.1.1 $ARCH_HOSTNAME'  | tee -a /etc/hosts
    echo '127.0.1.1 $ARCH_HOSTNAME.localdomain $ARCH_HOSTNAME' | tee -a /etc/hosts
    # check if everything is good
    hostnamectl
}

arch_install_nameservers() {
    echo "nameserver 8.8.8.8" >> /etc/resolv.conf
    echo "nameserver 8.8.4.4" >> /etc/resolv.conf
}

arch_install_locales() {
    sed -i 's/#\(en_US\.UTF-8\)/\1/g' /etc/locale.gen # uncomment the en_US locale
    sed -i 's/#\(ru_RU\.UTF-8\)/\1/g' /etc/locale.gen # uncomment the ru_RU locale
    locale-gen # generate the uncommented locale

    # /etc/rc.conf
    echo "LANGUAGE=en_US.UTF-8" | tee /etc/rc.conf
    echo "LOCALE=en_US.UTF-8" | tee -a /etc/rc.conf

    # /etc/locale.conf
    echo "LANG=en_US.UTF-8" | tee /etc/locale.conf
    echo "LANGUAGE=ru_RU.UTF-8" | tee -a /etc/locale.conf # set a fallback locale
    echo "LC_ALL=en_US.UTF-8" | tee -a /etc/locale.conf

    # /etc/environment
    echo "LC_ALL=en_US.UTF-8" | tee /etc/environment

    # /etc/vconsole.conf
    echo "KEYMAP=en" | tee /etc/vconsole.conf
    echo "LOCALE=en_US.UTF-8" | tee -a /etc/vconsole.conf
    echo "HARDWARECLOCK=localtime" | tee -a /etc/vconsole.conf
    echo "TIMEZONE=Europe/Moscow" | tee -a /etc/vconsole.conf
    echo "CONSOLEFONT=" | tee -a /etc/vconsole.conf
    echo "CONSOLEMAP=" | tee -a /etc/vconsole.conf
    echo "USECOLOR=yes" | tee -a /etc/vconsole.conf
}

arch_install_users() {
    echo root:root | chpasswd
    # passwd
    # add to the wheel group in favour to use the sudo mechanism
    useradd -m -g users -G wheel idfumg
    echo idfumg:idfumg | chpasswd
    # passwd idfumg
    # EDITOR=nano visudo # enable/uncomment sudo for the wheel group
}

arch_install_mkinitcpio() {
    # add the lvm2 drivers for the initramfs (type lvm2 after block device drivers)
    # nano /etc/mkinitcpio.conf
    sed -i 's/^HOOKS=\(.*\) block \(.*\)/HOOKS=\1 block lvm2 \2/g' /etc/mkinitcpio.conf
    # make the initram fs with the lvm2 included for linux and linux-lts kernels
    mkinitcpio -p linux
    mkinitcpio -p linux-lts
}

arch_install_grub() {
    mkdir -p /boot/EFI
    mkdir -p /boot/grub/locale

    # check if efi is enabled (vars should be defined)
    efivar-tester
    # mount efi partition into the boot dir for grub

    # install the grub, actually
    grub-install --target=x86_64-efi --bootloader-id=grub_uefi --recheck
    # setup a locale for grub
    cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
    # make a grub config file
    grub-mkconfig -o /boot/grub/grub.cfg
}

arch_install_steam() {
    pacman -S --noconfirm lib32-nvidia-utils ttf-liberation steam
    # start steam
    # Settings->Steam Play->Enable Steam Play for all other titles
}

arch_finish() {
    echo -e "\e[1;32mDone! Type exit, umount -a and reboot"
    # exit # leave the chroot environment
    # umount -a # unmount everything
    # reboot
}

arch_install_wireless_firmware() {
    # Qualcomm Atheros QCZ6174 802.11ac Wireless Network Adapter (rev 32)
    # https://github.com/kvalo/ath10k-firmware/tree/master/QCA6174/hw3.0
    # board-2.bin
    # firmware-6.bin_WLAN.RM.4.4.1.c3-00230 -> firmware-6.bin

    # ls ~/Downloads # board-2.bin firmware-6.bin
    # sudo mkdir -p /lib/firmware/ath10k/QCA6174/hw3.0
    # sudo cp ~/Downloads/board-2.bin /lib/firmware/ath10k/QCA6174/hw3.0/
    # sudo cp ~/Downloads/firmware-6.bin /lib/firmware/ath10k/QCA6174/hw3.0/
    # lshw -C network
    # lspci # search for the our wireless card device and its revision number
    # modinfo ath10k_pci # which drivers our kernel uses
    # ifconfig wlp7s0 down # disable the interface
    # modprobe -r ath10k_pci # unload kernel modules
    # modprobe -r ath10k_core
    # modprobe ath10k_core # load new kernel modules
    # modprobe ath10k_pci
    # ifconfig -a # check if wifi is up
    # dmesg | grep ath10k # check if something wrong
    # ethtool -i wlp7s0 # check the driver version (WLAN.RM.4.4.1.c3-00230)
    # lsmod | grep ath10k # check wifi modules states
    # sudo ifconfig wlp7s0 up # enable wifi interface
    # sudo pacman -S iw
    # sudo iw dev wlp7s0 station dump # check wifi channel speed
    # sudo iwconfig wlp7s0 txpower 15 # change the power of the transmiter
}

main() {
    arch_start
    arch_install_fs_and_lvm sda1 sda2
    arch_install_chroot_directories_hierarchy
    arch_mount_partitions_to_chroot_fs sda1
    arch_install_bases_and_go_to_the_chroot_jail
    arch_install_packages
    arch_install_swap
    arch_install_timezone
    arch_install_hostname box
    arch_install_nameservers
    arch_install_locales
    arch_install_users
    arch_install_mkinitcpio
    arch_install_grub
    arch_install_steam
    arch_install_wireless_firmware
    arch_finish
}
