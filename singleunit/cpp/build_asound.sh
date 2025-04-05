g++ gen_asound.cpp -o gen_asound
./gen_asound
sudo mv asound.conf /etc
sudo chown root /etc/asound.conf

