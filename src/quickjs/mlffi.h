#ifndef MLFFI_H
#define MLFFI_H

#include <string>
#include <sstream>

#include "../../../quickjs/quickjs.h"

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

string stringify(JSContext *ctx, JSValue &js) {
  const char *str = JS_ToCString(ctx, js);
  string ret(str);
  JS_FreeCString(ctx, str);
  return ret;
}

value v_error_of_js_exn (value v_ret, value v_field, JSContext *ctx)
{
  CAMLparam2(v_ret, v_field);
  JSValue exception_val = JS_GetException(ctx);
  ostringstream ss;

  ss << stringify(ctx, exception_val);
  JSValue stack = JS_GetPropertyStr(ctx, exception_val, "stack");
  if (!JS_IsUndefined(stack)) {
    ss << "\n" << stringify(ctx, stack);
  }

  JS_FreeValue(ctx, stack);
  JS_FreeValue(ctx, exception_val);

  string error(ss.str());
  v_ret = v_error_of_string (v_ret, v_field, error);
  CAMLreturn (v_ret);
}

#endif
