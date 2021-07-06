sudo pacman -S archiso

sudo rm -frv archliveiso
sudo rm -frv archlivebuild
mkdir -vp archlive
cp -rv /usr/share/archiso/configs/releng/* archlive
cd archlive

sed -i 's/^\(dhcpcd\)/#\1/g' packages.x86_64

echo "networkmanager
network-manager-applet
bluez
bluez-utils
cups
xf86-video-qxl
xf86-video-intel
xf86-video-amdgpu
xf86-video-nouveau
xorg
xfce4
xfce4-goodies
lightdm
lightdm-gtk-greeter
firefox
emacs" | tee -a packages.x86_64

# if it's the tty1
echo "
set -e -u

sed -i 's/#\(en_US\.UTF-8\)/\1/g' /etc/locale.gen
sed -i 's/#\(ru_RU\.UTF-8\)/\1/g' /etc/locale.gen
locale-gen

usermod -s /usr/bin/zsh root
cp -aT /etc/skel/ /root/
chmod 7000 /root
passwd -d root

sed -i 's/#\(PermitRootLogin \).*/\1yes/g' /etc/ssh/sshd_config
sed -i 's/#Server/Server/g' /etc/pacman.d/mirrorlist

systemctl enable lightdm
systemctl enable NetworkManager
systemctl enable bluetooth
systemctl enable cups
systemctl set-default graphical.target
systemctl start NetworkManager
systemctl start bluetooth
systemctl start cups
systemctl start lightdm
" | tee -a airootfs/root/.automated_script.sh

cd ..
sudo mkarchiso -v -w archlivebuild -o archliveiso archlive
sudo rm -fr archlivebuild
