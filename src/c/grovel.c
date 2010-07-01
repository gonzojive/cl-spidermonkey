#define XP_UNIX
#include "jsapi.h"

int main(int argc, const char *argv[])
{
  printf("(defconstant +jsval-void+ %lu)\n", JSVAL_VOID);

  return 0;
}
