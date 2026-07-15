if [ -v $(uname -a | grep rasp) ]; then
    echo 'not running rotation on non-Rasberry Pi machine'
else
    /home/$USER/Documents/rotation/propellorServo.py
fi
