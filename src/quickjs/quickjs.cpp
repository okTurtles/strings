#include "mlffi.h"
#include "runtime.h"

vector<JSContext*> contexts;

extern "C"
value stub_init_contexts(value v_num_threads)
{
  CAMLparam1(v_num_threads);
  CAMLlocal2(v_ret, v_field);
  int num_threads { Int_val(v_num_threads) };

  caml_enter_blocking_section();

  contexts.reserve(num_threads);
  vector<string> errors {};
  errors.reserve(num_threads);

  #pragma omp parallel for
  for (int i = 0; i < num_threads; i++) {
    JSRuntime* rt { JS_NewRuntime() };
    if (!rt) {
      errors[i] = string { "JS_NewRuntime failure" };
    } else {
      JSContext* ctx { JS_NewContext(rt) };
      if (!ctx) {
        JS_FreeRuntime(rt);
        errors[i] = string { "JS_NewContext failure" };
      } else {
        JSValue bc { JS_ReadObject(ctx, qjsc_runtime, qjsc_runtime_size, JS_READ_OBJ_BYTECODE) };
        JSValue ret_val { JS_EvalFunction(ctx, bc) };

        if (JS_IsException(ret_val)) {
          errors[i] = stringify_exn(ctx);
          JS_FreeValue(ctx, ret_val);
          JS_FreeContext(ctx);
          JS_FreeRuntime(rt);
        } else {
          JS_FreeValue(ctx, ret_val);
          contexts[i] = move(ctx);
        }
      }
    }
  }

  caml_leave_blocking_section();

  for (int i { 0 }; i < num_threads; i++) {
    if (errors[i].length() > 0) {
      CAMLreturn (v_error_of_string(v_ret, v_field, errors[i]));
    }
  }

  CAMLreturn (v_ok_of_v(v_ret, Val_unit));
}

value convert_string_array(JSContext* const ctx, const JSValue& jspair, const string& key, value v_ret, value v_field) {
  CAMLparam2(v_ret, v_field);
  JSValue jsarray { JS_GetPropertyStr(ctx, jspair, key.c_str()) };
  JSValue prop_len { JS_GetPropertyStr(ctx, jsarray, "length") };
  int64_t len {};
  if (JS_ToInt64(ctx, &len, prop_len)) {
    JS_FreeValue(ctx, prop_len);
    JS_FreeValue(ctx, jsarray);
    CAMLreturn (v_error_of_string(v_ret, v_field, string { "Could not get results length, please report this bug." }));
  }

  v_field = caml_alloc(len, 0);
  for (int i { 0 }; i < len; i++) {
    JSValue prop_str { JS_GetPropertyUint32(ctx, jsarray, i) };
    size_t str_len {};
    const char* str { JS_ToCStringLen(ctx, &str_len, prop_str) };
    Store_field(v_field, i, caml_alloc_initialized_string(str_len, str));
    JS_FreeCString(ctx, str);
    JS_FreeValue(ctx, prop_str);
  }

  JS_FreeValue(ctx, prop_len);
  JS_FreeValue(ctx, jsarray);

  CAMLreturn (v_ok_of_v(v_ret, v_field));
}

extern "C"
value stub_extract(value v_id, value v_code, value v_fn_name)
{
  CAMLparam3(v_id, v_code, v_fn_name);
  CAMLlocal4(v_ret, v_field, v_arr1, v_arr2);
  int id { Int_val(v_id) };
  string code { String_val(v_code), caml_string_length(v_code) };
  string fn_name { String_val(v_fn_name), caml_string_length(v_fn_name) };

  caml_enter_blocking_section();

  JSContext* ctx { contexts[id] };
  if (!ctx) {
    caml_leave_blocking_section();
    CAMLreturn (v_error_of_string(v_ret, v_field, string { "Could not find context, please report this bug." }));
  }
  JS_UpdateStackTop(JS_GetRuntime(ctx));

  JSValue code_val { JS_NewStringLen(ctx, code.c_str(), code.length()) };
  JSValue global_obj { JS_GetGlobalObject(ctx) };
  JSValue fun { JS_GetPropertyStr(ctx, global_obj, fn_name.c_str()) };
  JSValue ret_val { JS_Call(ctx, fun, JS_NULL, 1, &code_val) };
  JS_FreeValue(ctx, fun);
  JS_FreeValue(ctx, global_obj);
  JS_FreeValue(ctx, code_val);

  if (JS_IsException(ret_val)) {
    caml_leave_blocking_section();
    v_ret = v_error_of_string(v_ret, v_field, stringify_exn(ctx));
    JS_FreeValue(ctx, ret_val);
    CAMLreturn (v_ret);
  };

  caml_leave_blocking_section();

  v_arr1 = convert_string_array(ctx, ret_val, string { "strings" }, v_ret, v_field);
  v_arr2 = convert_string_array(ctx, ret_val, string { "possibleScripts" }, v_ret, v_field);
  if (Tag_val(v_arr1) == 0 && Tag_val(v_arr2) == 0) {
    // Ok, Ok
    // Tuple2
    v_field = caml_alloc_small(2, 0);
    Field(v_field, 0) = Field(v_arr1, 0);
    Field(v_field, 1) = Field(v_arr2, 0);
    v_ret = v_ok_of_v(v_ret, v_field);

  } else if (Tag_val(v_arr1) == 1) {
    // Error, _
    v_ret = v_arr1;
  } else if (Tag_val(v_arr2) == 1) {
    // Ok, Error
    v_ret = v_arr2;
  }

  JS_FreeValue(ctx, ret_val);

  CAMLreturn (v_ret);
}
