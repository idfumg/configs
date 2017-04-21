#!/usr/bin/env bash

rm /etc/rc.conf
touch /etc/rc.conf
echo LANGUAGE="en_US.UTF-8" >> /etc/rc.conf
echo LOCALE="en_US.UTF-8" >> /etc/rc.conf

rm /etc/locale.conf
touch /etc/locale.conf
echo LANG=en_US.UTF-8 >> /etc/locale.conf
echo LC_ALL=en_US.UTF-8 >> /etc/locale.conf

rm /etc/vconsole.conf
touch /etc/vconsole.conf
echo KEYMAP=en >> /etc/vconsole.conf
echo LOCALE="en_US.UTF-8" >> /etc/vconsole.conf
echo HARDWARECLOCK="localtime" >> /etc/vconsole.conf
echo TIMEZONE=Europe/Moscow >> /etc/vconsole.conf
echo CONSOLEFONT= >> /etc/vconsole.conf
echo CONSOLEMAP= >> /etc/vconsole.conf
echo USECOLOR="yes" >> /etc/vconsole.conf
