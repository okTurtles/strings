#include <map>
#include "mlffi.h"

map<string, JSContext*> contexts;

extern "C"
value stub_init_context(value v_id, value v_script)
{
  CAMLparam2(v_id, v_script);
  CAMLlocal2(v_ret, v_field);
  string id(String_val(v_id), caml_string_length(v_id));
  string script(String_val(v_script), caml_string_length(v_script));

  caml_enter_blocking_section();

  JSRuntime *rt;
  JSContext *ctx;
  JSValue ret_val;

  rt = JS_NewRuntime();
  if (rt == NULL) {
    caml_leave_blocking_section();
    CAMLreturn (v_error_of_cstring(v_ret, v_field, "JS_NewRuntime failure"));
  }
  ctx = JS_NewContext(rt);
  if (ctx == NULL) {
      JS_FreeRuntime(rt);
      caml_leave_blocking_section();
      CAMLreturn (v_error_of_cstring(v_ret, v_field, "JS_NewContext failure"));
  }

  ret_val = JS_Eval(ctx, script.c_str(), script.length(), "<evalScript>", JS_EVAL_TYPE_GLOBAL);

  if (JS_IsException(ret_val)) {
    caml_leave_blocking_section();
    v_ret = v_error_of_js_exn(v_ret, v_field, ctx);
    JS_FreeValue(ctx, ret_val);
    JS_FreeContext(ctx);
    JS_FreeRuntime(rt);
    CAMLreturn (v_ret);
  };

  JS_FreeValue(ctx, ret_val);
  contexts[id] = ctx;

  caml_leave_blocking_section();

  v_ret = v_ok_of_v(v_ret, Val_unit);
  CAMLreturn (v_ret);
}

extern "C"
value stub_extract_ts(value v_id, value v_code)
{
  CAMLparam2(v_id, v_code);
  CAMLlocal2(v_ret, v_field);
  string id(String_val(v_id), caml_string_length(v_id));
  string code(String_val(v_code), caml_string_length(v_code));

  caml_enter_blocking_section();

  JSContext *ctx = contexts[id];
  if (ctx == NULL) {
    caml_leave_blocking_section();
    CAMLreturn (v_error_of_cstring(v_ret, v_field, "Could not find context, please report this bug."));
  }

  ostringstream ss;
  ss << "extract(\"" << code << "\");";
  string script(ss.str());

  JSValue ret_val = JS_Eval(ctx, script.c_str(), script.length(), "<evalScript>", JS_EVAL_TYPE_GLOBAL);

  if (JS_IsException(ret_val)) {
    caml_leave_blocking_section();
    v_ret = v_error_of_js_exn(v_ret, v_field, ctx);
    JS_FreeValue(ctx, ret_val);
    CAMLreturn (v_ret);
  };

  JSValue prop_len = JS_GetPropertyStr(ctx, ret_val, "length");
  int64_t len;
  if (JS_ToInt64(ctx, &len, prop_len)) {
    JS_FreeValue(ctx, prop_len);
    JS_FreeValue(ctx, ret_val);
    caml_leave_blocking_section();
    CAMLreturn (v_error_of_cstring(v_ret, v_field, "Could not get TS results length, please report this bug."));
  }
  JS_FreeValue(ctx, prop_len);

  caml_leave_blocking_section();

  v_field = caml_alloc(len, 0);
  for (int i = 0; i < len; i++) {
    JSValue prop_str = JS_GetPropertyUint32(ctx, ret_val, i);
    size_t str_len;
    const char* str = JS_ToCStringLen(ctx, &str_len, prop_str);
    Store_field(v_field, i, caml_alloc_initialized_string(str_len, str));
    JS_FreeCString(ctx, str);
    JS_FreeValue(ctx, prop_str);
  }

  JS_FreeValue(ctx, ret_val);

  CAMLreturn (v_ok_of_v(v_ret, v_field));
}
