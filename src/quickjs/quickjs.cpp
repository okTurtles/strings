#include "mlffi.h"

#include "runtime.h"

vector<JSContext*> contexts;

extern "C"
value stub_init_contexts(value v_num_threads)
{
  CAMLparam1(v_num_threads);
  CAMLlocal2(v_ret, v_field);
  int num_threads = Int_val(v_num_threads);

  caml_enter_blocking_section();

  contexts.reserve(num_threads);
  vector<string> errors(num_threads, string(""));

  #pragma omp parallel for
  for (int i = 0; i < num_threads; i++) {
    JSRuntime *rt = JS_NewRuntime();
    if (rt == NULL) {
      errors[i] = string("JS_NewRuntime failure");
    } else {
      JSContext *ctx = JS_NewContext(rt);
      if (ctx == NULL) {
        JS_FreeRuntime(rt);
        errors[i] = string("JS_NewContext failure");
      } else {
        JSValue bc = JS_ReadObject(ctx, qjsc_runtime, qjsc_runtime_size, JS_READ_OBJ_BYTECODE);
        JSValue ret_val = JS_EvalFunction(ctx, bc);

        if (JS_IsException(ret_val)) {
          errors[i] = stringify_exn(ctx);
          JS_FreeValue(ctx, ret_val);
          JS_FreeContext(ctx);
          JS_FreeRuntime(rt);
        } else {
          JS_FreeValue(ctx, ret_val);
          contexts[i] = ctx;
        }
      }
    }
  }

  caml_leave_blocking_section();

  for (int i = 0; i < num_threads; i++) {
    if (errors[i].length() > 0) {
      CAMLreturn (v_error_of_string(v_ret, v_field, errors[i]));
    }
  }

  CAMLreturn (v_ok_of_v(v_ret, Val_unit));
}

extern "C"
value stub_extract_ts(value v_id, value v_code)
{
  CAMLparam2(v_id, v_code);
  CAMLlocal2(v_ret, v_field);
  int id = Int_val(v_id);
  string code(String_val(v_code), caml_string_length(v_code));

  caml_enter_blocking_section();

  JSContext *ctx = contexts[id];
  if (ctx == NULL) {
    caml_leave_blocking_section();
    CAMLreturn (v_error_of_cstring(v_ret, v_field, "Could not find context, please report this bug."));
  }
  JS_UpdateStackTop(JS_GetRuntime(ctx));

  JSValue code_val = JS_NewStringLen(ctx, code.c_str(), code.length());
  JSValue global_obj = JS_GetGlobalObject(ctx);
  JSValue fun = JS_GetPropertyStr(ctx, global_obj, "extract");
  JSValue ret_val = JS_Call(ctx, fun, JS_NULL, 1, &code_val);
  JS_FreeValue(ctx, fun);
  JS_FreeValue(ctx, global_obj);
  JS_FreeValue(ctx, code_val);

  if (JS_IsException(ret_val)) {
    caml_leave_blocking_section();
    string error = stringify_exn(ctx);
    v_ret = v_error_of_string(v_ret, v_field, error);
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
