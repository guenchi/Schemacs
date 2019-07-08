

#include <stdio.h>
#include <termios.h>

struct termios raw;

int textmode_raw_mode(void)
{

    if (tcgetattr(0, &raw) == -1)
        return -1;


    raw.c_lflag &= ~ECHO;
    raw.c_lflag &= ~ICANON;

    // raw.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    // raw.c_oflag &= ~(OPOST);
    // raw.c_cflag |= (CS8);
    // raw.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    // raw.c_cc[VMIN] = 1;
    // raw.c_cc[VTIME] = 0;

    return tcsetattr(0, TCSAFLUSH, &raw);
}
