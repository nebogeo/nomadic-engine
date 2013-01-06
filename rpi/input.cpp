
#include "linux/kd.h" // keyboard stuff...
#include "termios.h"
#include "sys/ioctl.h"


static struct termios tty_attr_old;
static int old_keyboard_mode;

int __mouse_fd=-1;
