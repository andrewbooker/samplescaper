#!/bin/bash
ipn=19
sudo apt-get install -y vim
sudo apt-get install -y python-pygame
sudo apt-get install -y jq
sudo apt-get install -y zip
sudo ln -fs /usr/bin/python3.7 /usr/bin/python
sudo pip install sounddevice
sudo pip install soundfile

cd ~/Documents
git clone http://github.com/andrewbooker/samplescaper.git
git clone http://github.com/andrewbooker/wavmixer.git
git clone http://github.com/andrewbooker/rotation.git

sudo sed -i '/dtparam=audio=on/c\dtoverlay=hifiberry-dacplus  #dtparam=audio=on' /boot/config.txt

echo "interface wlan0" >> /etc/dhcpcd.conf
echo "static ip_address=192.168.1.${ipn}" >> /etc/dhcpcd.conf
echo "static routers=192.168.1.254" >> /etc/dhcpcd.conf
echo "static domain_name_servers=192.168.1.254" >> /etc/dhcpcd.conf

