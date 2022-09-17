open! Core
open Flow_ast

let extract ~on_string stmts =
  let rec extract_expr_or_spread = function
    | Expression.Expression expr -> extract_expression expr
    | Expression.Spread (_, { argument; comments = _ }) -> extract_expression argument
  and extract_computed_key ComputedKey.(_, { comments = _; expression }) = extract_expression expression
  and extract_pattern = function
    | _, Pattern.Object { properties; annot; comments = _ } ->
      extract_type_annotation_or_hint annot;
      List.iter properties ~f:(function
        | Pattern.Object.Property (_, { key; pattern; default; shorthand = _ }) ->
          (match key with
          | Pattern.Object.Property.Literal _
           |Pattern.Object.Property.Identifier _ ->
            ()
          | Pattern.Object.Property.Computed key -> extract_computed_key key);
          extract_pattern pattern;
          Option.iter default ~f:extract_expression
        | Pattern.Object.RestElement (_, { argument; comments = _ }) -> extract_pattern argument)
    | _, Pattern.Array { elements; annot; comments = _ } ->
      List.iter elements ~f:(function
        | Pattern.Array.Element (_, { argument; default }) ->
          extract_pattern argument;
          Option.iter default ~f:extract_expression
        | Pattern.Array.RestElement (_, { argument; comments = _ }) -> extract_pattern argument
        | Pattern.Array.Hole _ -> ());
      extract_type_annotation_or_hint annot
    | _, Pattern.Identifier { name = _; annot; optional = _ } -> extract_type_annotation_or_hint annot
    | _, Pattern.Expression expr -> extract_expression expr
  and extract_predicate = function
    | _, Type.Predicate.{ kind = Declared expr; comments = _ } -> extract_expression expr
    | _, Type.Predicate.{ kind = Inferred; comments = _ } -> ()
  and extract_function
     Function.
       {
         id = _;
         params = _, { params; rest; this_; comments = _ };
         body;
         async = _;
         generator = _;
         predicate;
         return;
         tparams;
         sig_loc = _;
         comments = _;
       } =
    List.iter params ~f:(fun (_, { argument; default }) ->
        Option.iter this_ ~f:(fun (_, { annot = _, ty; comments = _ }) -> extract_type ty);
        extract_pattern argument;
        Option.iter default ~f:extract_expression);
    Option.iter rest ~f:(fun (_, { argument; comments = _ }) -> extract_pattern argument);
    (match body with
    | Function.BodyBlock (_, st_block) -> extract_statement_block st_block
    | Function.BodyExpression expr -> extract_expression expr);
    Option.iter predicate ~f:extract_predicate;
    extract_type_annotation_or_hint return;
    Option.iter tparams ~f:extract_type_params
  and extract_class_property_value = function
    | Class.Property.Declared
     |Class.Property.Uninitialized ->
      ()
    | Class.Property.Initialized expr -> extract_expression expr
  and extract_implements (_, Class.Implements.{ interfaces; comments = _ }) =
    List.iter interfaces ~f:(fun (_, { id = _; targs }) -> Option.iter targs ~f:extract_type_args)
  and extract_class
     Class.
       {
         id = _;
         body = _, { body; comments = _ };
         tparams;
         extends;
         implements;
         class_decorators;
         comments = _;
       } =
    List.iter body ~f:(function
      | Class.Body.Method (_, { kind = _; key; value = _, fn; static = _; decorators; comments = _ }) ->
        extract_object_property_key key;
        extract_function fn;
        List.iter decorators ~f:(fun (_, { expression; comments = _ }) -> extract_expression expression)
      | Class.Body.Property (_, { key; value; annot; static = _; variance = _; comments = _ }) ->
        extract_object_property_key key;
        extract_class_property_value value;
        extract_type_annotation_or_hint annot
      | Class.Body.PrivateField (_, { key = _; value; annot; static = _; variance = _; comments = _ }) ->
        extract_class_property_value value;
        extract_type_annotation_or_hint annot);
    Option.iter tparams ~f:extract_type_params;
    Option.iter extends ~f:(fun (_, { expr; targs; comments = _ }) ->
        extract_expression expr;
        Option.iter targs ~f:extract_type_args);
    Option.iter implements ~f:extract_implements;
    List.iter class_decorators ~f:(fun (_, { expression; comments = _ }) -> extract_expression expression)
  and extract_object_property_key = function
    | Expression.Object.Property.Literal _
     |Expression.Object.Property.Identifier _
     |Expression.Object.Property.PrivateName _ ->
      ()
    | Expression.Object.Property.Computed key -> extract_computed_key key
  and extract_call_type_args (_, Expression.CallTypeArgs.{ arguments; comments = _ }) =
    List.iter arguments ~f:(function
      | Explicit ty -> extract_type ty
      | Implicit _ -> ())
  and extract_call
     Expression.Call.{ callee; arguments = _, { arguments; comments = _ }; targs; comments = _ } =
    extract_expression callee;
    List.iter arguments ~f:extract_expr_or_spread;
    Option.iter targs ~f:extract_call_type_args
  and extract_member Expression.Member.{ _object; property; comments = _ } =
    extract_expression _object;
    match property with
    | Expression.Member.PropertyIdentifier _
     |Expression.Member.PropertyPrivateName _ ->
      ()
    | Expression.Member.PropertyExpression expr -> extract_expression expr
  and extract_template Expression.TemplateLiteral.{ quasis = _; expressions; comments = _ } =
    List.iter expressions ~f:extract_expression
  and extract_jsx_element
     JSX.
       {
         opening_element = _, { name = _; self_closing = _; attributes };
         closing_element = _;
         children = _, children;
         comments = _;
       } =
    List.iter attributes ~f:(function
      | SpreadAttribute (_, { argument; comments = _ }) -> extract_expression argument
      | Attribute (_, { name = _; value = None }) -> ()
      | Attribute (_, { name = _; value = Some v }) -> (
        match v with
        | Literal _ -> ()
        | ExpressionContainer (_, { expression = EmptyExpression; comments = _ }) -> ()
        | ExpressionContainer (_, { expression = Expression expr; comments = _ }) ->
          extract_expression expr));
    extract_jsx_children children
  and extract_jsx_fragment
     JSX.
       {
         frag_opening_element = _;
         frag_closing_element = _;
         frag_children = _, children;
         frag_comments = _;
       } =
    extract_jsx_children children
  and extract_jsx_children children =
    List.iter children ~f:(function
      | _, JSX.Element el -> extract_jsx_element el
      | _, JSX.Fragment frag -> extract_jsx_fragment frag
      | _, JSX.ExpressionContainer { expression = Expression expr; comments = _ } ->
        extract_expression expr
      | _, JSX.ExpressionContainer { expression = EmptyExpression; comments = _ } -> ()
      | _, JSX.SpreadChild { expression; comments = _ } -> extract_expression expression
      | _, JSX.Text _ -> ())
  and extract_expression = function
    (* Special case *)
    | ( _,
        Expression.Call
          {
            callee = _, Expression.Identifier (_, Identifier.{ name = "L"; comments = _ });
            arguments = _, { arguments; comments = _ };
            targs;
            comments = _;
          } ) ->
      List.iter arguments ~f:(function
        | Expression.Expression (_, Expression.Literal { value = String s; raw = _; comments = _ }) ->
          on_string s
        | Expression.Expression _
         |Expression.Spread _ ->
          ());
      Option.iter targs ~f:extract_call_type_args
    (* All expressions *)
    | _, Expression.Array { elements; comments = _ } ->
      List.iter elements ~f:(function
        | Expression expr -> extract_expression expr
        | Spread (_, { argument; comments = _ }) -> extract_expression argument
        | Hole _ -> ())
    | _, Expression.ArrowFunction fn -> extract_function fn
    | _, Expression.Assignment { operator = _; left; right; comments = _ } ->
      extract_pattern left;
      extract_expression right
    | _, Expression.Binary { operator = _; left; right; comments = _ } ->
      extract_expression left;
      extract_expression right
    | _, Expression.Call call -> extract_call call
    | _, Expression.Class _class -> extract_class _class
    | _, Expression.Comprehension { blocks; filter }
     |_, Expression.Generator { blocks; filter } ->
      List.iter blocks ~f:(fun (_, { left; right; each = _ }) ->
          extract_pattern left;
          extract_expression right);
      Option.iter filter ~f:extract_expression
    | _, Expression.Conditional { test; consequent; alternate; comments = _ } ->
      extract_expression test;
      extract_expression consequent;
      extract_expression alternate
    | _, Expression.Function fn -> extract_function fn
    | _, Expression.Identifier _ -> ()
    | _, Expression.Import { argument; comments = _ } -> extract_expression argument
    | _, Expression.JSXElement el -> extract_jsx_element el
    | _, Expression.JSXFragment frag -> extract_jsx_fragment frag
    | _, Expression.Literal _ -> ()
    | _, Expression.Logical { operator = _; left; right; comments = _ } ->
      extract_expression left;
      extract_expression right
    | _, Expression.Member member -> extract_member member
    | _, Expression.MetaProperty { meta = _; property = _; comments = _ } -> ()
    | _, Expression.New { callee; targs; arguments; comments = _ } ->
      extract_expression callee;
      Option.iter targs ~f:extract_call_type_args;
      Option.iter arguments ~f:(fun (_, { arguments; comments = _ }) ->
          List.iter arguments ~f:extract_expr_or_spread)
    | _, Expression.Object { properties; comments = _ } ->
      List.iter properties ~f:(function
        | Expression.Object.Property (_, Init { key; value; shorthand = _ }) ->
          extract_object_property_key key;
          extract_expression value
        | Expression.Object.Property (_, Method { key; value = _, fn })
         |Expression.Object.Property (_, Get { key; value = _, fn; comments = _ })
         |Expression.Object.Property (_, Set { key; value = _, fn; comments = _ }) ->
          extract_object_property_key key;
          extract_function fn
        | Expression.Object.SpreadProperty (_, { argument; comments = _ }) -> extract_expression argument)
    | _, Expression.OptionalCall { call; optional = _; filtered_out = _ } -> extract_call call
    | _, Expression.OptionalMember { member; optional = _; filtered_out = _ } -> extract_member member
    | _, Expression.Sequence { expressions; comments = _ } -> List.iter expressions ~f:extract_expression
    | _, Expression.Super { comments = _ } -> ()
    | _, Expression.TaggedTemplate { tag; quasi = _, template; comments = _ } ->
      extract_expression tag;
      extract_template template
    | _, Expression.TemplateLiteral template -> extract_template template
    | _, Expression.This { comments = _ } -> ()
    | _, Expression.TypeCast { expression; annot = _, ty; comments = _ } ->
      extract_expression expression;
      extract_type ty
    | _, Expression.Unary { operator = _; argument; comments = _ }
     |_, Expression.Update { operator = _; argument; prefix = _; comments = _ } ->
      extract_expression argument
    | _, Expression.Yield { argument; comments = _; delegate = _; result_out = _ } ->
      Option.iter argument ~f:extract_expression
  and extract_type_object Type.Object.{ exact = _; inexact = _; properties; comments = _ } =
    List.iter properties ~f:(function
      | Type.Object.Property
          (_, { key; value; optional = _; static = _; proto = _; _method = _; variance = _; comments = _ })
        ->
        (match value with
        | Init ty -> extract_type ty
        | Get (_, fn) -> extract_type_function fn
        | Set (_, fn) -> extract_type_function fn);
        extract_object_property_key key
      | Type.Object.SpreadProperty (_, { argument; comments = _ }) -> extract_type argument
      | Type.Object.Indexer (_, { id = _; key; value; static = _; variance = _; comments = _ }) ->
        extract_type key;
        extract_type value
      | Type.Object.CallProperty (_, { value = _, fn; static = _; comments = _ }) ->
        extract_type_function fn
      | Type.Object.InternalSlot
          (_, { id = _; value; optional = _; static = _; _method = _; comments = _ }) ->
        extract_type value)
  and extract_type_annotation_or_hint = function
    | Type.Missing _ -> ()
    | Type.Available (_, annotation) -> extract_type annotation
  and extract_type_param (_, Type.TypeParam.{ name = _; bound; variance = _; default }) =
    extract_type_annotation_or_hint bound;
    Option.iter default ~f:extract_type
  and extract_type_params (_, Type.TypeParams.{ params; comments = _ }) =
    List.iter params ~f:extract_type_param
  and extract_type_args (_, Type.TypeArgs.{ arguments; comments = _ }) =
    List.iter arguments ~f:extract_type
  and extract_type_generic Type.Generic.{ id = _; targs; comments = _ } =
    Option.iter targs ~f:extract_type_args
  and extract_type_function
     Type.Function.{ tparams; params = _, { this_; params; rest; comments = _ }; return; comments = _ } =
    Option.iter tparams ~f:(fun (_, { params; comments = _ }) -> List.iter params ~f:extract_type_param);
    Option.iter this_ ~f:(fun (_, Type.Function.ThisParam.{ annot = _, ty; comments = _ }) ->
        extract_type ty);
    List.iter params ~f:(fun (_, { name = _; annot; optional = _ }) -> extract_type annot);
    Option.iter rest ~f:(fun (_, { argument = _, { name = _; annot; optional = _ }; comments = _ }) ->
        extract_type annot);
    extract_type return
  and extract_type : ('a, 'b) Type.t -> unit = function
    | _, Type.Any _ -> ()
    | _, Type.Mixed _ -> ()
    | _, Type.Empty _ -> ()
    | _, Type.Void _ -> ()
    | _, Type.Null _ -> ()
    | _, Type.Number _ -> ()
    | _, Type.BigInt _ -> ()
    | _, Type.String _ -> ()
    | _, Type.Boolean _ -> ()
    | _, Type.Symbol _ -> ()
    | _, Type.Exists _ -> ()
    | _, Type.Nullable { argument; comments = _ } -> extract_type argument
    | _, Type.Function fn -> extract_type_function fn
    | _, Type.Object obj -> extract_type_object obj
    | _, Type.Interface { body = _, obj; extends; comments = _ } ->
      extract_type_object obj;
      List.iter extends ~f:(fun (_, generic) -> extract_type_generic generic)
    | _, Type.Array { argument; comments = _ } -> extract_type argument
    | _, Type.Generic generic -> extract_type_generic generic
    | _, IndexedAccess { _object; index; comments = _ } ->
      extract_type _object;
      extract_type index
    | _, OptionalIndexedAccess { indexed_access = { _object; index; comments = _ }; optional = _ } ->
      extract_type _object;
      extract_type index
    | _, Type.Union { types = t1, t2, ll; comments = _ } ->
      extract_type t1;
      extract_type t2;
      List.iter ll ~f:extract_type
    | _, Type.Intersection { types = t1, t2, ll; comments = _ } ->
      extract_type t1;
      extract_type t2;
      List.iter ll ~f:extract_type
    | _, Type.Typeof _ -> ()
    | _, Type.Tuple { types; comments = _ } -> List.iter types ~f:extract_type
    | _, Type.StringLiteral _ -> ()
    | _, Type.NumberLiteral _ -> ()
    | _, Type.BigIntLiteral _ -> ()
    | _, Type.BooleanLiteral _ -> ()
  and extract_declare_class
     Statement.DeclareClass.
       { id = _; tparams; body = _, ty_obj; extends; mixins; implements; comments = _ } =
    Option.iter tparams ~f:extract_type_params;
    extract_type_object ty_obj;
    Option.iter extends ~f:(fun (_, x) -> extract_type_generic x);
    List.iter mixins ~f:(fun (_, x) -> extract_type_generic x);
    Option.iter implements ~f:extract_implements
  and extract_interface Statement.Interface.{ id = _; tparams; extends; body = _, ty_obj; comments = _ } =
    Option.iter tparams ~f:extract_type_params;
    List.iter extends ~f:(fun (_, x) -> extract_type_generic x);
    extract_type_object ty_obj
  and extract_variable_declaration Statement.VariableDeclaration.{ declarations; kind = _; comments = _ }
      =
    List.iter declarations ~f:(fun (_, { id; init }) ->
        extract_pattern id;
        Option.iter init ~f:extract_expression)
  and extract_statement_block Statement.Block.{ body; comments = _ } = List.iter body ~f:extract_statement
  and extract_type_alias Statement.TypeAlias.{ id = _; tparams; right; comments = _ } =
    Option.iter tparams ~f:extract_type_params;
    extract_type right
  and extract_opaque_type Statement.OpaqueType.{ id = _; tparams; impltype; supertype; comments = _ } =
    Option.iter tparams ~f:extract_type_params;
    Option.iter impltype ~f:extract_type;
    Option.iter supertype ~f:extract_type
  and extract_statement = function
    | _, Statement.Block st_block -> extract_statement_block st_block
    | _, Statement.Break { label = _; comments = _ } -> ()
    | _, Statement.ClassDeclaration _class -> extract_class _class
    | _, Statement.Continue { label = _; comments = _ } -> ()
    | _, Statement.Debugger { comments = _ } -> ()
    | _, Statement.DeclareClass dc -> extract_declare_class dc
    | ( _,
        Statement.DeclareExportDeclaration
          { default = _; declaration; specifiers = _; source = _; comments = _ } ) ->
      Option.iter declaration ~f:(function
        | Statement.DeclareExportDeclaration.Variable (_, { id = _; annot = _, ty; comments = _ }) ->
          extract_type ty
        | Statement.DeclareExportDeclaration.Function
            (_, { id = _; annot = _, ty; predicate; comments = _ }) ->
          extract_type ty;
          Option.iter predicate ~f:extract_predicate
        | Statement.DeclareExportDeclaration.Class (_, dc) -> extract_declare_class dc
        | Statement.DeclareExportDeclaration.DefaultType ty -> extract_type ty
        | Statement.DeclareExportDeclaration.NamedType (_, alias) -> extract_type_alias alias
        | Statement.DeclareExportDeclaration.NamedOpaqueType (_, opaque) -> extract_opaque_type opaque
        | Statement.DeclareExportDeclaration.Interface (_, interface) -> extract_interface interface)
    | _, Statement.DeclareFunction { id = _; annot = _, ty; predicate; comments = _ } ->
      extract_type ty;
      Option.iter predicate ~f:extract_predicate
    | _, Statement.DeclareInterface interface -> extract_interface interface
    | _, Statement.DeclareModule { id = _; body = _, st_block; kind = _; comments = _ } ->
      extract_statement_block st_block
    | _, Statement.DeclareModuleExports { annot = _, ty; comments = _ } -> extract_type ty
    | _, Statement.DeclareTypeAlias alias -> extract_type_alias alias
    | _, Statement.DeclareOpaqueType opaque -> extract_opaque_type opaque
    | _, Statement.DeclareVariable { id = _; annot = _, ty; comments = _ } -> extract_type ty
    | _, Statement.DoWhile { body; test; comments = _ } ->
      extract_statement body;
      extract_expression test
    | _, Statement.Empty { comments = _ } -> ()
    | _, Statement.EnumDeclaration _ -> ()
    | _, Statement.ExportDefaultDeclaration { default = _; declaration; comments = _ } -> (
      match declaration with
      | Statement.ExportDefaultDeclaration.Declaration st -> extract_statement st
      | Statement.ExportDefaultDeclaration.Expression expr -> extract_expression expr)
    | ( _,
        Statement.ExportNamedDeclaration
          { declaration; specifiers = _; source = _; export_kind = _; comments = _ } ) ->
      Option.iter declaration ~f:extract_statement
    | _, Statement.Expression { expression; directive = _; comments = _ } -> extract_expression expression
    | _, Statement.For { init; test; update; body; comments = _ } ->
      Option.iter init ~f:(function
        | Statement.For.InitDeclaration (_, vd) -> extract_variable_declaration vd
        | Statement.For.InitExpression expr -> extract_expression expr);
      Option.iter test ~f:extract_expression;
      Option.iter update ~f:extract_expression;
      extract_statement body
    | _, Statement.ForIn { left; right; body; each = _; comments = _ } ->
      (match left with
      | Statement.ForIn.LeftDeclaration (_, vd) -> extract_variable_declaration vd
      | Statement.ForIn.LeftPattern pattern -> extract_pattern pattern);
      extract_expression right;
      extract_statement body
    | _, Statement.ForOf { left; right; body; await = _; comments = _ } ->
      (match left with
      | Statement.ForOf.LeftDeclaration (_, vd) -> extract_variable_declaration vd
      | Statement.ForOf.LeftPattern pattern -> extract_pattern pattern);
      extract_expression right;
      extract_statement body
    | _, Statement.FunctionDeclaration fn -> extract_function fn
    | _, Statement.If { test; consequent; alternate; comments = _ } ->
      extract_expression test;
      extract_statement consequent;
      Option.iter alternate ~f:(fun (_, { body; comments = _ }) -> extract_statement body)
    | ( _,
        Statement.ImportDeclaration
          { import_kind = _; source = _; default = _; specifiers = _; comments = _ } ) ->
      ()
    | _, Statement.InterfaceDeclaration interface -> extract_interface interface
    | _, Statement.Labeled { label = _; body; comments = _ } -> extract_statement body
    | _, Statement.Return { argument; comments = _; return_out = _ } ->
      Option.iter argument ~f:extract_expression
    | _, Statement.Switch { discriminant; cases; comments = _; exhaustive_out = _ } ->
      extract_expression discriminant;
      List.iter cases ~f:(fun (_, { test; consequent; comments = _ }) ->
          Option.iter test ~f:extract_expression;
          List.iter consequent ~f:extract_statement)
    | _, Statement.Throw { argument; comments = _ } -> extract_expression argument
    | _, Statement.Try { block = _, st_block; handler; finalizer; comments = _ } ->
      extract_statement_block st_block;
      Option.iter handler ~f:(fun (_, { param; body = _, st_block; comments = _ }) ->
          Option.iter param ~f:extract_pattern;
          extract_statement_block st_block);
      Option.iter finalizer ~f:(fun (_, st_block) -> extract_statement_block st_block)
    | _, Statement.TypeAlias alias -> extract_type_alias alias
    | _, Statement.OpaqueType opaque -> extract_opaque_type opaque
    | _, Statement.VariableDeclaration vd -> extract_variable_declaration vd
    | _, Statement.While { test; body; comments = _ } ->
      extract_expression test;
      extract_statement body
    | _, Statement.With { _object; body; comments = _ } ->
      extract_expression _object;
      extract_statement body
  in

  List.iter stmts ~f:extract_statement

let unescape = function
| [
    ( _,
      Statement.Expression
        {
          expression = _, Literal { value = String s; raw = _; comments = _ };
          directive = _;
          comments = _;
        } );
  ] ->
  Some s
| _ -> None
