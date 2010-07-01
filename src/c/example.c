#define XP_UNIX
#include "jsapi.h"

/* The class of the global object. */
static JSClass global_class = {
  "global", 
  JSCLASS_GLOBAL_FLAGS,
  JS_PropertyStub,
  JS_PropertyStub,
  JS_PropertyStub,
  JS_PropertyStub,
  JS_EnumerateStub,
  JS_ResolveStub,
  JS_ConvertStub,
  JS_FinalizeStub,
  JSCLASS_NO_OPTIONAL_MEMBERS
};

/* The error reporter callback. */
void reportError(JSContext *cx, const char *message, JSErrorReport *report)
{
  fprintf(stderr, "%s:%u:%s\n",
          report->filename ? report->filename : "<no filename>",
          (unsigned int) report->lineno,
          message);
}

int main(int argc, const char *argv[])
{
  printf("Global flags: %i\n", JSCLASS_GLOBAL_FLAGS);
  printf("JSCLASS_RESERVED_SLOTS_MASK: %i\n", JSCLASS_RESERVED_SLOTS_MASK);
  printf("reserved: %i\n", JSCLASS_HAS_RESERVED_SLOTS(JSProto_LIMIT));

  return 0;
}
