open Core_kernel
open Flow_ast

let extract strings stmts =
  let rec extract_expr_or_spread = function
  | Expression.Expression expr -> extract_expression expr
  | Expression.Spread (_, { argument }) -> extract_expression argument

  and extract_pattern = function
  | _, Pattern.Object { properties; annot = _ } ->
    List.iter properties ~f:(function
    | Pattern.Object.Property (_, { key; pattern; default; shorthand = _ }) ->
      begin match key with
      | Pattern.Object.Property.Literal _
      | Pattern.Object.Property.Identifier _ -> ()
      | Pattern.Object.Property.Computed expr -> extract_expression expr
      end;
      extract_pattern pattern;
      Option.iter default ~f:extract_expression
    | Pattern.Object.RestProperty (_, { argument }) -> extract_pattern argument
    )
  | _, Pattern.Array { elements; annot = _; comments = _ } ->
    List.iter elements ~f:(Option.iter ~f:(function
      | Pattern.Array.Element (_, { argument; default }) ->
        extract_pattern argument;
        Option.iter default ~f:extract_expression
      | Pattern.Array.RestElement (_, { argument }) -> extract_pattern argument
      ))
  | _, Pattern.Identifier _ -> ()
  | _, Pattern.Expression expr -> extract_expression expr

  and extract_predicate = function
  | _, Type.Predicate.Declared expr -> extract_expression expr
  | _, Type.Predicate.Inferred -> ()

  and extract_function Function.{
      id = _;
      params = _, { params; rest };
      body;
      async = _;
      generator = _;
      predicate;
      return = _;
      tparams = _;
      sig_loc = _;
    } =
    List.iter params ~f:(fun (_, { argument; default }) ->
      extract_pattern argument;
      Option.iter default ~f:extract_expression
    );
    Option.iter rest ~f:(fun (_, { argument }) -> extract_pattern argument);
    begin match body with
    | Function.BodyBlock (_, { body }) -> List.iter body ~f:extract_statement
    | Function.BodyExpression expr -> extract_expression expr
    end;
    Option.iter predicate ~f:extract_predicate

  and extract_class Class.{
      id = _;
      body = _, { body };
      tparams = _;
      extends;
      implements = _;
      classDecorators;
      comments = _;
    } =
    let extract_class_property_value = function
    | Class.Property.Declared
    | Class.Property.Uninitialized -> ()
    | Class.Property.Initialized expr -> extract_expression expr
    in
    List.iter body ~f:(function
    | Class.Body.Method (_, { kind = _; key; value = _, fn; static = _; decorators }) ->
      extract_object_property_key key;
      extract_function fn;
      List.iter decorators ~f:(fun (_, { expression }) -> extract_expression expression)
    | Class.Body.Property (_, { key; value; annot = _; static = _; variance = _ }) ->
      extract_object_property_key key;
      extract_class_property_value value
    | Class.Body.PrivateField (_, { key = _; value; annot = _; static = _; variance = _ }) ->
      extract_class_property_value value
    );
    Option.iter extends ~f:(fun (_, { expr; targs = _ }) -> extract_expression expr);
    List.iter classDecorators ~f:(fun (_, { expression }) -> extract_expression expression)

  and extract_object_property_key = function
  | Expression.Object.Property.Literal _
  | Expression.Object.Property.Identifier _
  | Expression.Object.Property.PrivateName _ -> ()
  | Expression.Object.Property.Computed expr -> extract_expression expr

  and extract_call Expression.Call.{ callee; arguments = _, args; targs = _ } =
    extract_expression callee;
    List.iter args ~f:extract_expr_or_spread

  and extract_member Expression.Member.{ _object; property } =
    extract_expression _object;
    begin match property with
    | Expression.Member.PropertyIdentifier _
    | Expression.Member.PropertyPrivateName _ -> ()
    | Expression.Member.PropertyExpression expr -> extract_expression expr
    end

  and extract_template Expression.TemplateLiteral.{ quasis = _; expressions } =
    List.iter expressions ~f:extract_expression


  and extract_expression = function
  (* Special case *)
  | _, Expression.Call {
      callee = _, Expression.Identifier (_, Identifier.{ name = "L"; comments = _ });
      arguments = _, args;
      targs = _;
    } ->
    List.iter args ~f:(function
    | Expression.Expression (_, Expression.Literal { value = String s; raw = _; comments = _ }) ->
      Queue.enqueue strings s
    | Expression.Expression _
    | Expression.Spread _ -> ()
    )

  (* All expressions *)
  | _, Expression.Array { elements; comments = _ } ->
    List.iter elements ~f:(Option.iter ~f:extract_expr_or_spread)

  | _, Expression.ArrowFunction fn -> extract_function fn

  | _, Expression.Assignment { operator = _; left; right } ->
    extract_pattern left;
    extract_expression right

  | _, Expression.Binary { operator = _; left; right } ->
    extract_expression left;
    extract_expression right

  | _, Expression.Call call -> extract_call call

  | _, Expression.Class _class -> extract_class _class

  | _, Expression.Comprehension { blocks; filter }
  | _, Expression.Generator ({ blocks; filter }) ->
    List.iter blocks ~f:(fun (_, { left; right; each = _ }) ->
      extract_pattern left;
      extract_expression right
    );
    Option.iter filter ~f:extract_expression

  | _, Expression.Conditional { test; consequent; alternate } ->
    extract_expression test;
    extract_expression consequent;
    extract_expression alternate

  | _, Expression.Function fn -> extract_function fn

  | _, Expression.Identifier _ -> ()

  | _, Expression.Import expr -> extract_expression expr

  | _, Expression.JSXElement _
  | _, Expression.JSXFragment _ -> ()

  | _, Expression.Literal _ -> ()

  | _, Expression.Logical { operator = _; left; right } ->
    extract_expression left;
    extract_expression right

  | _, Expression.Member member -> extract_member member

  | _, Expression.MetaProperty { meta = _; property = _ } -> ()

  | _, Expression.New { callee; targs = _ ; arguments; comments = _ } ->
    extract_expression callee;
    Option.iter arguments ~f:(fun (_, ll) -> List.iter ll ~f:extract_expr_or_spread);

  | _, Expression.Object { properties; comments = _ } ->
    List.iter properties ~f:(function
    | Expression.Object.Property (_, Init { key; value; shorthand = _ }) ->
      extract_object_property_key key;
      extract_expression value
    | Expression.Object.Property (_, Method { key; value = _, fn })
    | Expression.Object.Property (_, Get { key; value = _, fn })
    | Expression.Object.Property (_, Set { key; value = _, fn }) ->
      extract_object_property_key key;
      extract_function fn
    | Expression.Object.SpreadProperty (_, { argument }) -> extract_expression argument
    )

  | _, Expression.OptionalCall { call; optional = _ } -> extract_call call

  | _, Expression.OptionalMember { member; optional = _ } -> extract_member member

  | _, Expression.Sequence { expressions } -> List.iter expressions ~f:extract_expression

  | _, Expression.Super -> ()

  | _, Expression.TaggedTemplate { tag; quasi = _, template } ->
    extract_expression tag;
    extract_template template

  | _, Expression.TemplateLiteral template -> extract_template template

  | _, Expression.This -> ()

  | _, Expression.TypeCast { expression; annot = _ } -> extract_expression expression

  | _, Expression.Unary { operator = _; argument; comments = _ }
  | _, Expression.Update { operator = _; argument; prefix = _ } -> extract_expression argument

  | _, Expression.Yield { argument; comments = _; delegate = _ } ->
    Option.iter argument ~f:extract_expression

  and extract_type_object_properties =
    List.iter ~f:(function
    | Type.Object.Property (_, {
        key;
        value = _;
        optional = _;
        static = _;
        proto = _;
        _method = _;
        variance = _
      }) -> extract_object_property_key key
    | Type.Object.SpreadProperty _
    | Type.Object.Indexer _
    | Type.Object.CallProperty _
    | Type.Object.InternalSlot _ -> ()
    )

  and extract_declare_class Statement.DeclareClass.{
      id = _;
      tparams = _;
      body = _, { exact = _; inexact = _; properties };
      extends = _;
      mixins = _;
      implements = _;
    } =
    extract_type_object_properties properties

  and extract_interface Statement.Interface.{
      id = _;
      tparams = _;
      extends = _;
      body = _, { exact = _; inexact = _; properties };
    } = extract_type_object_properties properties

  and extract_variable_declaration Statement.VariableDeclaration.{ declarations; kind = _ } =
    List.iter declarations ~f:(fun (_, { id; init }) ->
      extract_pattern id;
      Option.iter init ~f:extract_expression
    )

  and extract_statement = function
  | _, Statement.Block { body } -> List.iter body ~f:extract_statement

  | _, Statement.Break { label = _; comments = _ } -> ()

  | _, Statement.ClassDeclaration _class -> extract_class _class

  | _, Statement.Continue { label = _; comments = _ } -> ()

  | _, Statement.Debugger -> ()

  | _, Statement.DeclareClass dc -> extract_declare_class dc

  | _, Statement.DeclareExportDeclaration { default = _; declaration; specifiers = _; source = _ } ->
    Option.iter declaration ~f:(function
    | Statement.DeclareExportDeclaration.Variable _ -> ()
    | Statement.DeclareExportDeclaration.Function (_, { id = _; annot = _; predicate }) ->
      Option.iter predicate ~f:extract_predicate
    | Statement.DeclareExportDeclaration.Class (_, dc) -> extract_declare_class dc
    | Statement.DeclareExportDeclaration.DefaultType _
    | Statement.DeclareExportDeclaration.NamedType _
    | Statement.DeclareExportDeclaration.NamedOpaqueType _ -> ()
    | Statement.DeclareExportDeclaration.Interface (_, {
        id = _;
        tparams = _;
        extends = _;
        body = _, { exact = _; inexact = _; properties };
      }) -> extract_type_object_properties properties
    )

  | _, Statement.DeclareFunction { id = _; annot = _; predicate } ->
    Option.iter predicate ~f:extract_predicate

  | _, Statement.DeclareInterface interface -> extract_interface interface

  | _, Statement.DeclareModule { id = _; body = _, { body }; kind = _ } ->
    List.iter body ~f:extract_statement

  | _, Statement.DeclareModuleExports _ -> ()

  | _, Statement.DeclareTypeAlias { id = _; tparams = _; right = _ } -> ()

  | _, Statement.DeclareOpaqueType { id = _; tparams = _; impltype = _; supertype = _ } -> ()

  | _, Statement.DeclareVariable { id = _; annot = _ } -> ()

  | _, Statement.DoWhile { body; test; comments = _ } ->
    extract_statement body;
    extract_expression test

  | _, Statement.Empty -> ()

  | _, Statement.EnumDeclaration _ -> ()

  | _, Statement.ExportDefaultDeclaration { default = _; declaration } ->
    begin match declaration with
    | Statement.ExportDefaultDeclaration.Declaration st -> extract_statement st
    | Statement.ExportDefaultDeclaration.Expression expr -> extract_expression expr
    end

  | _, Statement.ExportNamedDeclaration { declaration; specifiers = _; source = _; exportKind = _ } ->
    Option.iter declaration ~f:extract_statement

  | _, Statement.Expression { expression; directive = _ } -> extract_expression expression

  | _, Statement.For { init; test; update; body } ->
    Option.iter init ~f:(function
    | Statement.For.InitDeclaration (_, vd) -> extract_variable_declaration vd
    | Statement.For.InitExpression expr -> extract_expression expr
    );
    Option.iter test ~f:extract_expression;
    Option.iter update ~f:extract_expression;
    extract_statement body

  | _, Statement.ForIn { left; right; body; each = _ } ->
    begin match left with
    | Statement.ForIn.LeftDeclaration (_, vd) -> extract_variable_declaration vd
    | Statement.ForIn.LeftPattern pattern -> extract_pattern pattern
    end;
    extract_expression right;
    extract_statement body

  | _, Statement.ForOf { left; right; body; async = _ } ->
    begin match left with
    | Statement.ForOf.LeftDeclaration (_, vd) -> extract_variable_declaration vd
    | Statement.ForOf.LeftPattern pattern -> extract_pattern pattern
    end;
    extract_expression right;
    extract_statement body

  | _, Statement.FunctionDeclaration fn -> extract_function fn

  | _, Statement.If { test; consequent; alternate; comments = _ } ->
    extract_expression test;
    extract_statement consequent;
    Option.iter alternate ~f:extract_statement

  | _, Statement.ImportDeclaration { importKind = _; source = _; default = _; specifiers = _ } -> ()

  | _, Statement.InterfaceDeclaration interface -> extract_interface interface

  | _, Statement.Labeled { label = _; body } -> extract_statement body

  | _, Statement.Return { argument; comments = _ } -> Option.iter argument ~f:extract_expression

  | _, Statement.Switch { discriminant; cases } ->
    extract_expression discriminant;
    List.iter cases ~f:(fun (_, { test; consequent }) ->
      Option.iter test ~f:extract_expression;
      List.iter consequent ~f:extract_statement
    )

  | _, Statement.Throw { argument } -> extract_expression argument

  | _, Statement.Try { block = _, { body }; handler; finalizer; comments = _ } ->
    List.iter body ~f:extract_statement;
    Option.iter handler ~f:(fun (_, { param; body = _, { body }; comments = _ }) ->
      Option.iter param ~f:extract_pattern;
      List.iter body ~f:extract_statement
    );
    Option.iter finalizer ~f:(fun (_, { body }) ->
      List.iter body ~f:extract_statement
    )

  | _, Statement.TypeAlias { id = _; tparams = _; right = _ } -> ()

  | _, Statement.OpaqueType { id = _; tparams = _; impltype = _; supertype = _ } -> ()

  | _, Statement.VariableDeclaration vd -> extract_variable_declaration vd

  | _, Statement.While { test; body } ->
    extract_expression test;
    extract_statement body

  | _, Statement.With { _object; body } ->
    extract_expression _object;
    extract_statement body
  in

  List.iter stmts ~f:extract_statement


let strings queue source =
  begin match Parser_flow.program source with
  | _, (_::_ as errors) ->
    let buf = Buffer.create 128 in
    List.iter errors ~f:(fun (loc, err) ->
      Buffer.add_string buf "Error at line ";
      Loc.show loc |> Buffer.add_string buf;
      Buffer.add_string buf ":\n";
      Parse_error.PP.error err |> Buffer.add_string buf;
      Buffer.add_char buf '\n'
    );
    failwith (Buffer.contents buf)
  | ast, [] ->
    begin match ast with
    | _loc, stmts, _comments ->
      (* sprintf "Statements: %s" (List.map stmts ~f:(fun stmt ->
          Format.asprintf "%a" (Statement.pp (fun _ _ -> ()) (fun _ _ -> ())) stmt
         ) |> String.concat ~sep:", ")
         |> print_endline; *)
      extract queue stmts
    end
  | exception exn -> print_endline (sprintf "%s ::: %s" source (Exn.to_string exn))
  end
