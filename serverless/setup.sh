#!/bin/bash
ipn=19
sudo apt-get install -y vim
sudo apt-get install -y python3-pygame
sudo apt-get install -y jq
sudo apt-get install -y zip
sudo ln -fs /usr/bin/python3.7 /usr/bin/python
sudo pip install sounddevice
sudo pip install soundfile

cd ~/Documents
git clone https://github.com/andrewbooker/samplescaper.git
git clone https://github.com/andrewbooker/wavmixer.git
git clone https://github.com/andrewbooker/rotation.git

sudo sed -i '/dtparam=audio=on/c\dtoverlay=hifiberry-dacplus  #dtparam=audio=on' /boot/config.txt

echo "interface wlan0" >> /etc/dhcpcd.conf
echo "static ip_address=192.168.1.${ipn}" >> /etc/dhcpcd.conf
echo "static routers=192.168.1.254" >> /etc/dhcpcd.conf
echo "static domain_name_servers=192.168.1.254" >> /etc/dhcpcd.conf

echo '' > ~/Documents/update.sh
echo "cd rotation" >> ~/Documents/update.sh
echo "git pull" >> ~/Documents/update.sh
echo "cd .." >> ~/Documents/update.sh
echo "cd wavmixer" >> ~/Documents/update.sh
echo "git pull" >> ~/Documents/update.sh
echo "cd .." >> ~/Documents/update.sh
echo "cd samplescaper" >> ~/Documents/update.sh
echo "git pull" >> ~/Documents/update.sh
echo "cd .." >> ~/Documents/update.sh
chmod +x ~/Documents/update.sh

echo '{' > static.json
echo '    "isPilot": 0,' >> static.json
echo '    "colour": "red",' >> static.json
echo '    "leftRelativeToRight": 1.2' >> static.json
echo '}' >> static.json

sudo sysctl -w net.ipv6.conf.all.disable_ipv6=1
sudo sysctl -w net.ipv6.conf.default.disable_ipv6=1
