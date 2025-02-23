sudo apt update
sudo apt install -y python3-pip
sudo ln -fs /usr/bin/python3 /usr/bin/python
sudo pip install sounddevice
sudo pip install soundfile
sudo apt install -y libsdl2-mixer-2.0-0 libsdl2-2.0-0 libportaudio2

sudo apt -y install vim
sudo apt -y install zip
sudo apt -y install jq
sudo apt -y install git
sudo apt -y install tig
sudo apt -y install curl
sudo apt -y install htop
sudo apt -y install net-tools
sudo apt -y install dhcpcd5
sudo apt -y install tmux

sudo mkdir /var/log/samplescaper
sudo chown $USER /var/log/samplescaper
