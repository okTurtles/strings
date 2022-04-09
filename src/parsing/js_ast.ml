open Core
open Flow_ast

let extract strings stmts =
  let rec extract_expr_or_spread = function
    | Expression.Expression expr -> extract_expression expr
    | Expression.Spread (_, { argument; comments = _ }) -> extract_expression argument
  and extract_computed_key ComputedKey.(_, { comments = _; expression }) = extract_expression expression
  and extract_pattern = function
    | _, Pattern.Object { properties; annot = _; comments = _ } ->
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
    | _, Pattern.Array { elements; annot = _; comments = _ } ->
      List.iter elements ~f:(function
        | Pattern.Array.Element (_, { argument; default }) ->
          extract_pattern argument;
          Option.iter default ~f:extract_expression
        | Pattern.Array.RestElement (_, { argument; comments = _ }) -> extract_pattern argument
        | Pattern.Array.Hole _ -> ())
    | _, Pattern.Identifier _ -> ()
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
         return = _;
         tparams = _;
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
    Option.iter predicate ~f:extract_predicate
  and extract_class
     Class.
       {
         id = _;
         body = _, { body; comments = _ };
         tparams = _;
         extends;
         implements = _;
         class_decorators;
         comments = _;
       } =
    let extract_class_property_value = function
      | Class.Property.Declared
       |Class.Property.Uninitialized ->
        ()
      | Class.Property.Initialized expr -> extract_expression expr
    in
    List.iter body ~f:(function
      | Class.Body.Method (_, { kind = _; key; value = _, fn; static = _; decorators; comments = _ }) ->
        extract_object_property_key key;
        extract_function fn;
        List.iter decorators ~f:(fun (_, { expression; comments = _ }) -> extract_expression expression)
      | Class.Body.Property (_, { key; value; annot = _; static = _; variance = _; comments = _ }) ->
        extract_object_property_key key;
        extract_class_property_value value
      | Class.Body.PrivateField (_, { key = _; value; annot = _; static = _; variance = _; comments = _ })
        ->
        extract_class_property_value value);
    Option.iter extends ~f:(fun (_, { expr; targs = _; comments = _ }) -> extract_expression expr);
    List.iter class_decorators ~f:(fun (_, { expression; comments = _ }) -> extract_expression expression)
  and extract_object_property_key = function
    | Expression.Object.Property.Literal _
     |Expression.Object.Property.Identifier _
     |Expression.Object.Property.PrivateName _ ->
      ()
    | Expression.Object.Property.Computed key -> extract_computed_key key
  and extract_call
     Expression.Call.{ callee; arguments = _, { arguments; comments = _ }; targs = _; comments = _ } =
    extract_expression callee;
    List.iter arguments ~f:extract_expr_or_spread
  and extract_member Expression.Member.{ _object; property; comments = _ } =
    extract_expression _object;
    match property with
    | Expression.Member.PropertyIdentifier _
     |Expression.Member.PropertyPrivateName _ ->
      ()
    | Expression.Member.PropertyExpression expr -> extract_expression expr
  and extract_template Expression.TemplateLiteral.{ quasis = _; expressions; comments = _ } =
    List.iter expressions ~f:extract_expression
  and extract_import Expression.Import.{ argument; comments = _ } = extract_expression argument
  and extract_expression = function
    (* Special case *)
    | ( _,
        Expression.Call
          {
            callee = _, Expression.Identifier (_, Identifier.{ name = "L"; comments = _ });
            arguments = _, { arguments; comments = _ };
            targs = _;
            comments = _;
          } ) ->
      List.iter arguments ~f:(function
        | Expression.Expression (_, Expression.Literal { value = String s; raw = _; comments = _ }) ->
          Queue.enqueue strings s
        | Expression.Expression _
         |Expression.Spread _ ->
          ())
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
    | _, Expression.Import import -> extract_import import
    | _, Expression.JSXElement _
     |_, Expression.JSXFragment _ ->
      ()
    | _, Expression.Literal _ -> ()
    | _, Expression.Logical { operator = _; left; right; comments = _ } ->
      extract_expression left;
      extract_expression right
    | _, Expression.Member member -> extract_member member
    | _, Expression.MetaProperty { meta = _; property = _; comments = _ } -> ()
    | _, Expression.New { callee; targs = _; arguments; comments = _ } ->
      extract_expression callee;
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
    | _, Expression.OptionalCall { call; optional = _ } -> extract_call call
    | _, Expression.OptionalMember { member; optional = _ } -> extract_member member
    | _, Expression.Sequence { expressions; comments = _ } -> List.iter expressions ~f:extract_expression
    | _, Expression.Super { comments = _ } -> ()
    | _, Expression.TaggedTemplate { tag; quasi = _, template; comments = _ } ->
      extract_expression tag;
      extract_template template
    | _, Expression.TemplateLiteral template -> extract_template template
    | _, Expression.This { comments = _ } -> ()
    | _, Expression.TypeCast { expression; annot = _; comments = _ } -> extract_expression expression
    | _, Expression.Unary { operator = _; argument; comments = _ }
     |_, Expression.Update { operator = _; argument; prefix = _; comments = _ } ->
      extract_expression argument
    | _, Expression.Yield { argument; comments = _; delegate = _ } ->
      Option.iter argument ~f:extract_expression
  and extract_type_object Type.Object.{ exact = _; inexact = _; properties; comments = _ } =
    List.iter properties ~f:(function
      | Type.Object.Property
          ( _,
            {
              key;
              value = _;
              optional = _;
              static = _;
              proto = _;
              _method = _;
              variance = _;
              comments = _;
            } ) ->
        extract_object_property_key key
      | Type.Object.SpreadProperty _
       |Type.Object.Indexer _
       |Type.Object.CallProperty _
       |Type.Object.InternalSlot _ ->
        ())
  and extract_type_annotation_or_hint = function
    | Type.Missing _ -> ()
    | Type.Available (_, annotation) -> extract_type annotation
  and extract_type_param (_, Type.TypeParam.{ name = _; bound; variance = _; default }) =
    extract_type_annotation_or_hint bound;
    Option.iter default ~f:extract_type
  and extract_type_args (_, Type.TypeArgs.{ arguments; comments = _ }) =
    List.iter arguments ~f:extract_type
  and extract_type_generic Type.Generic.{ id = _; targs; comments = _ } =
    Option.iter targs ~f:extract_type_args
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
    | ( _,
        Type.Function { tparams; params = _, { this_; params; rest; comments = _ }; return; comments = _ }
      ) ->
      Option.iter tparams ~f:(fun (_, { params; comments = _ }) -> List.iter params ~f:extract_type_param);
      Option.iter this_ ~f:(fun (_, Type.Function.ThisParam.{ annot = _, ty; comments = _ }) ->
          extract_type ty);
      List.iter params ~f:(fun (_, { name = _; annot; optional = _ }) -> extract_type annot);
      Option.iter rest ~f:(fun (_, { argument = _, { name = _; annot; optional = _ }; comments = _ }) ->
          extract_type annot);
      extract_type return
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
    | _, Type.Typeof { argument; comments = _ } -> extract_type argument
    | _, Type.Tuple { types; comments = _ } -> List.iter types ~f:extract_type
    | _, Type.StringLiteral _ -> ()
    | _, Type.NumberLiteral _ -> ()
    | _, Type.BigIntLiteral _ -> ()
    | _, Type.BooleanLiteral _ -> ()
  and extract_declare_class
     Statement.DeclareClass.
       { id = _; tparams = _; body = _, ty_obj; extends = _; mixins = _; implements = _; comments = _ } =
    extract_type_object ty_obj
  and extract_interface
     Statement.Interface.{ id = _; tparams = _; extends = _; body = _, ty_obj; comments = _ } =
    extract_type_object ty_obj
  and extract_variable_declaration Statement.VariableDeclaration.{ declarations; kind = _; comments = _ }
      =
    List.iter declarations ~f:(fun (_, { id; init }) ->
        extract_pattern id;
        Option.iter init ~f:extract_expression)
  and extract_statement_block Statement.Block.{ body; comments = _ } = List.iter body ~f:extract_statement
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
        | Statement.DeclareExportDeclaration.Variable _ -> ()
        | Statement.DeclareExportDeclaration.Function (_, { id = _; annot = _; predicate; comments = _ })
          ->
          Option.iter predicate ~f:extract_predicate
        | Statement.DeclareExportDeclaration.Class (_, dc) -> extract_declare_class dc
        | Statement.DeclareExportDeclaration.DefaultType _
         |Statement.DeclareExportDeclaration.NamedType _
         |Statement.DeclareExportDeclaration.NamedOpaqueType _ ->
          ()
        | Statement.DeclareExportDeclaration.Interface
            (_, { id = _; tparams = _; extends = _; body = _, ty_obj; comments = _ }) ->
          extract_type_object ty_obj)
    | _, Statement.DeclareFunction { id = _; annot = _; predicate; comments = _ } ->
      Option.iter predicate ~f:extract_predicate
    | _, Statement.DeclareInterface interface -> extract_interface interface
    | _, Statement.DeclareModule { id = _; body = _, st_block; kind = _; comments = _ } ->
      extract_statement_block st_block
    | _, Statement.DeclareModuleExports _ -> ()
    | _, Statement.DeclareTypeAlias { id = _; tparams = _; right = _; comments = _ } -> ()
    | _, Statement.DeclareOpaqueType { id = _; tparams = _; impltype = _; supertype = _; comments = _ } ->
      ()
    | _, Statement.DeclareVariable { id = _; annot = _; comments = _ } -> ()
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
    | _, Statement.Return { argument; comments = _ } -> Option.iter argument ~f:extract_expression
    | _, Statement.Switch { discriminant; cases; comments = _ } ->
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
    | _, Statement.TypeAlias { id = _; tparams = _; right = _; comments = _ } -> ()
    | _, Statement.OpaqueType { id = _; tparams = _; impltype = _; supertype = _; comments = _ } -> ()
    | _, Statement.VariableDeclaration vd -> extract_variable_declaration vd
    | _, Statement.While { test; body; comments = _ } ->
      extract_expression test;
      extract_statement body
    | _, Statement.With { _object; body; comments = _ } ->
      extract_expression _object;
      extract_statement body
  in

  List.iter stmts ~f:extract_statement

let strings ?filename queue source =
  match Parser_flow.program source with
  | _, (_ :: _ as errors) ->
    let buf = Buffer.create 128 in
    List.iter errors ~f:(fun (loc, err) ->
        Buffer.add_string buf "Error at line ";
        Loc.show loc |> Buffer.add_string buf;
        Buffer.add_string buf ":\n";
        Parse_error.PP.error err |> Buffer.add_string buf;
        Buffer.add_char buf '\n');
    failwith (Buffer.contents buf)
  | ast, [] -> (
    match ast with
    | _, Program.{ statements; comments = _; all_comments = _ } ->
      (* sprintf "Statements: %s" (List.map statements ~f:(fun stmt ->
          Format.asprintf "%a" (Statement.pp (fun _ _ -> ()) (fun _ _ -> ())) stmt
         ) |> String.concat ~sep:", ")
         |> print_endline; *)
      extract queue statements)
  | exception exn ->
    Option.iter filename ~f:(fun filename -> print_endline (sprintf "Parsing error in %s" filename));
    raise exn
