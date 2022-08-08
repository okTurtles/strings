#ifndef MLFFI_H
#define MLFFI_H

#include <string>
#include <vector>
#include <sstream>

#include "quickjs.h"

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>

using namespace std;

value v_ok_of_v(value v_ret, value v_ok)
{
  CAMLparam2(v_ret, v_ok);
  // Size 1, tag 0 (Ok)
  v_ret = caml_alloc_small(1, 0);
  Field(v_ret, 0) = v_ok;
  CAMLreturn (v_ret);
}

value v_error_of_cstring(value v_ret, value v_field, const char* error)
{
  CAMLparam2(v_ret, v_field);
  v_field = caml_alloc_initialized_string(strlen(error), error);
  // Size 1, tag 1 (Error)
  v_ret = caml_alloc_small(1, 1);
  Field(v_ret, 0) = v_field;
  CAMLreturn (v_ret);
}

value v_error_of_string(value v_ret, value v_field, string &error)
{
  CAMLparam2(v_ret, v_field);
  v_field = caml_alloc_initialized_string(error.length(), error.c_str());
  // Size 1, tag 1 (Error)
  v_ret = caml_alloc_small(1, 1);
  Field(v_ret, 0) = v_field;
  CAMLreturn (v_ret);
}

string stringify_prop(JSContext *ctx, JSValue &js, const char *prop_name) {
  JSValue prop = JS_GetPropertyStr(ctx, js, prop_name);
  if (!JS_IsUndefined(prop)) {
    size_t len;
    const char *str = JS_ToCStringLen(ctx, &len, prop);
    if (str == nullptr) {
      return string("<null>");
    } else {
      string ret(str, len);
      JS_FreeCString(ctx, str);
      return ret;
    }
  }
  return string("<undefined>");
}

string stringify_exn(JSContext *ctx) {
  JSValue exception_val = JS_GetException(ctx);
  ostringstream ss;

  if (JS_IsError(ctx, exception_val)) {
    ss << stringify_prop(ctx, exception_val, "message");
    ss << stringify_prop(ctx, exception_val, "stack");
  } else {
    size_t len;
    const char *str = JS_ToCStringLen(ctx, &len, exception_val);
    ss << string(str, len);
    JS_FreeCString(ctx, str);
  }

  JS_FreeValue(ctx, exception_val);
  return ss.str();
}


#endif
