#!/bin/bash

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

