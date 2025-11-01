#include <iostream>
#include <pigpio.h>

int main(int argc, char* argv[]) {
    gpioInitialise();
    const unsigned int port(16);
    gpioSetMode(port, PI_OUTPUT);
    std::cout << "\n" << "set freq: " << gpioSetPWMfrequency(port, 1000);
    std::cout << "\n" << "set pwm: " << gpioPWM(port, 64);
    //gpioWrite(port, 0);
    std::cout << "\n" << "duty cycle: " << gpioGetPWMdutycycle(port);
    std::cout << "\n";
    gpioTerminate();
    std::cout << "Hello Andrew" << std::endl;
}
