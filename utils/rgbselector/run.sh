g++ run.cpp -o rgbselect -l pigpio
if [ $? != 0 ]; then
    exit
fi
./rgbselect
