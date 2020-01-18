

#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>

struct termios raw;
struct winsize size;

int raw_on(void)
{

    if (tcgetattr(0, &raw) == -1)
        return -1;


    raw.c_lflag &= ~ECHO;
    raw.c_lflag &= ~ICANON;
    raw.c_lflag &= ~ISIG;


    // raw.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    // raw.c_oflag &= ~(OPOST);
    // raw.c_cflag |= (CS8);
    // raw.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    // raw.c_cc[VMIN] = 1;
    // raw.c_cc[VTIME] = 0;

    return tcsetattr(0, TCSAFLUSH, &raw);
}


int raw_off(void)
{

    if (tcgetattr(0, &raw) == -1)
        return -1;


    raw.c_lflag |= ECHO;
    raw.c_lflag |= ICANON;
    raw.c_lflag |= ISIG;

    return tcsetattr(0, TCSAFLUSH, &raw);
}



int get_row(void)
{
    ioctl(0, TIOCGWINSZ, &size);
    return size.ws_row;
}

int get_col(void)
{
    ioctl(0, TIOCGWINSZ, &size);
    return size.ws_col;
}
