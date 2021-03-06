declare module "tree-sitter-ekscript" {
  export interface Parser {
    parse(
      input: string | Input,
      previousTree?: Tree,
      options?: { bufferSize?: number; includedRanges?: Range[] }
    ): Tree;
    getLanguage(): any;
    setLanguage(language: any): void;
    getLogger(): Logger;
    setLogger(logFunc: Logger): void;
  }

  export type Point = {
    row: number;
    column: number;
  };

  export type Range = {
    startIndex: number;
    endIndex: number;
    startPosition: Point;
    endPosition: Point;
  };

  export type Edit = {
    startIndex: number;
    oldEndIndex: number;
    newEndIndex: number;
    startPosition: Point;
    oldEndPosition: Point;
    newEndPosition: Point;
  };

  export type Logger = (
    message: string,
    params: { [param: string]: string },
    type: "parse" | "lex"
  ) => void;

  export interface Input {
    seek(index: number): void;
    read(): any;
  }

  export interface SyntaxNode {
    tree: Tree;
    type: string;
    isNamed: boolean;
    text: string;
    startPosition: Point;
    endPosition: Point;
    startIndex: number;
    endIndex: number;
    parent: SyntaxNode | null;
    children: Array<SyntaxNode>;
    namedChildren: Array<SyntaxNode>;
    childCount: number;
    namedChildCount: number;
    firstChild: SyntaxNode | null;
    firstNamedChild: SyntaxNode | null;
    lastChild: SyntaxNode | null;
    lastNamedChild: SyntaxNode | null;
    nextSibling: SyntaxNode | null;
    nextNamedSibling: SyntaxNode | null;
    previousSibling: SyntaxNode | null;
    previousNamedSibling: SyntaxNode | null;

    hasChanges(): boolean;
    hasError(): boolean;
    isMissing(): boolean;
    toString(): string;
    child(index: number): SyntaxNode | null;
    namedChild(index: number): SyntaxNode | null;
    firstChildForIndex(index: number): SyntaxNode | null;
    firstNamedChildForIndex(index: number): SyntaxNode | null;

    descendantForIndex(index: number): SyntaxNode;
    descendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
    namedDescendantForIndex(index: number): SyntaxNode;
    namedDescendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
    descendantForPosition(position: Point): SyntaxNode;
    descendantForPosition(startPosition: Point, endPosition: Point): SyntaxNode;
    namedDescendantForPosition(position: Point): SyntaxNode;
    namedDescendantForPosition(
      startPosition: Point,
      endPosition: Point
    ): SyntaxNode;
    descendantsOfType(
      types: String | Array<String>,
      startPosition?: Point,
      endPosition?: Point
    ): Array<SyntaxNode>;

    closest(types: String | Array<String>): SyntaxNode | null;
    walk(): TreeCursor;
  }

  interface SyntaxNodeBase {
    tree: Tree;
    type: string;
    isNamed: boolean;
    text: string;
    startPosition: Point;
    endPosition: Point;
    startIndex: number;
    endIndex: number;
    parent: SyntaxNode | null;
    children: Array<SyntaxNode>;
    namedChildren: Array<SyntaxNode>;
    childCount: number;
    namedChildCount: number;
    firstChild: SyntaxNode | null;
    firstNamedChild: SyntaxNode | null;
    lastChild: SyntaxNode | null;
    lastNamedChild: SyntaxNode | null;
    nextSibling: SyntaxNode | null;
    nextNamedSibling: SyntaxNode | null;
    previousSibling: SyntaxNode | null;
    previousNamedSibling: SyntaxNode | null;

    hasChanges(): boolean;
    hasError(): boolean;
    isMissing(): boolean;
    toString(): string;
    child(index: number): SyntaxNode | null;
    namedChild(index: number): SyntaxNode | null;
    firstChildForIndex(index: number): SyntaxNode | null;
    firstNamedChildForIndex(index: number): SyntaxNode | null;

    descendantForIndex(index: number): SyntaxNode;
    descendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
    namedDescendantForIndex(index: number): SyntaxNode;
    namedDescendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
    descendantForPosition(position: Point): SyntaxNode;
    descendantForPosition(startPosition: Point, endPosition: Point): SyntaxNode;
    namedDescendantForPosition(position: Point): SyntaxNode;
    namedDescendantForPosition(
      startPosition: Point,
      endPosition: Point
    ): SyntaxNode;
    descendantsOfType(
      types: String | Array<String>,
      startPosition?: Point,
      endPosition?: Point
    ): Array<SyntaxNode>;

    closest<T extends SyntaxType>(types: T | readonly T[]): NamedNode<T> | null;
    walk(): TreeCursor;
  }

  export interface TreeCursor {
    nodeType: string;
    nodeText: string;
    nodeIsNamed: boolean;
    startPosition: Point;
    endPosition: Point;
    startIndex: number;
    endIndex: number;
    readonly currentNode: SyntaxNode;

    reset(node: SyntaxNode): void;
    gotoParent(): boolean;
    gotoFirstChild(): boolean;
    gotoFirstChildForIndex(index: number): boolean;
    gotoNextSibling(): boolean;
  }

  export interface Tree {
    readonly rootNode: SyntaxNode;

    edit(delta: Edit): Tree;
    walk(): TreeCursor;
    getChangedRanges(other: Tree): Range[];
    getEditedRange(other: Tree): Range;
  }

  interface NamedNodeBase extends SyntaxNodeBase {
    isNamed: true;
  }

  /** An unnamed node with the given type string. */
  export interface UnnamedNode<T extends string = string>
    extends SyntaxNodeBase {
    type: T;
    isNamed: false;
  }

  type PickNamedType<Node, T extends string> = Node extends {
    type: T;
    isNamed: true;
  }
    ? Node
    : never;

  type PickType<Node, T extends string> = Node extends { type: T }
    ? Node
    : never;

  /** A named node with the given `type` string. */
  export type NamedNode<T extends SyntaxType = SyntaxType> = PickNamedType<
    SyntaxNode,
    T
  >;

  /**
   * A node with the given `type` string.
   *
   * Note that this matches both named and unnamed nodes. Use `NamedNode<T>` to pick only named nodes.
   */
  export type NodeOfType<T extends string> = PickType<SyntaxNode, T>;

  interface TreeCursorOfType<S extends string, T extends SyntaxNodeBase> {
    nodeType: S;
    currentNode: T;
  }

  type TreeCursorRecord = {
    [K in TypeString]: TreeCursorOfType<K, NodeOfType<K>>;
  };

  /**
   * A tree cursor whose `nodeType` correlates with `currentNode`.
   *
   * The typing becomes invalid once the underlying cursor is mutated.
   *
   * The intention is to cast a `TreeCursor` to `TypedTreeCursor` before
   * switching on `nodeType`.
   *
   * For example:
   * ```ts
   * let cursor = root.walk();
   * while (cursor.gotoNextSibling()) {
   *   const c = cursor as TypedTreeCursor;
   *   switch (c.nodeType) {
   *     case SyntaxType.Foo: {
   *       let node = c.currentNode; // Typed as FooNode.
   *       break;
   *     }
   *   }
   * }
   * ```
   */
  export type TypedTreeCursor = TreeCursorRecord[keyof TreeCursorRecord];

  export interface ErrorNode extends NamedNodeBase {
    type: SyntaxType.ERROR;
    hasError(): true;
  }
  export const enum SyntaxType {
    ERROR = "ERROR",
    AbstractClassDeclaration = "abstract_class_declaration",
    AbstractMethodSignature = "abstract_method_signature",
    AccessibilityModifier = "accessibility_modifier",
    AmbientDeclaration = "ambient_declaration",
    Arguments = "arguments",
    Array = "array",
    ArrayPattern = "array_pattern",
    ArrayType = "array_type",
    ArrowFunction = "arrow_function",
    AsExpression = "as_expression",
    Asserts = "asserts",
    AssignmentExpression = "assignment_expression",
    AssignmentPattern = "assignment_pattern",
    AugmentedAssignmentExpression = "augmented_assignment_expression",
    AwaitExpression = "await_expression",
    BinaryExpression = "binary_expression",
    BreakStatement = "break_statement",
    CallExpression = "call_expression",
    CallSignature = "call_signature",
    CatchClause = "catch_clause",
    Char = "char",
    Class = "class",
    ClassBody = "class_body",
    ClassDeclaration = "class_declaration",
    ClassHeritage = "class_heritage",
    ComputedPropertyName = "computed_property_name",
    ConditionalType = "conditional_type",
    Constraint = "constraint",
    ConstructSignature = "construct_signature",
    ConstructorType = "constructor_type",
    ContinueStatement = "continue_statement",
    DebuggerStatement = "debugger_statement",
    Decorator = "decorator",
    DefaultType = "default_type",
    DoStatement = "do_statement",
    ElseClause = "else_clause",
    EmptyStatement = "empty_statement",
    EnumAssignment = "enum_assignment",
    EnumBody = "enum_body",
    EnumDeclaration = "enum_declaration",
    ExistentialType = "existential_type",
    ExportClause = "export_clause",
    ExportSpecifier = "export_specifier",
    ExportStatement = "export_statement",
    ExpressionStatement = "expression_statement",
    ExtendsClause = "extends_clause",
    FinallyClause = "finally_clause",
    FlowMaybeType = "flow_maybe_type",
    ForInStatement = "for_in_statement",
    ForStatement = "for_statement",
    FormalParameters = "formal_parameters",
    Function = "function",
    FunctionDeclaration = "function_declaration",
    FunctionSignature = "function_signature",
    FunctionType = "function_type",
    GeneratorFunction = "generator_function",
    GeneratorFunctionDeclaration = "generator_function_declaration",
    GenericType = "generic_type",
    IfStatement = "if_statement",
    ImplementsClause = "implements_clause",
    Import = "import",
    ImportAlias = "import_alias",
    ImportClause = "import_clause",
    ImportRequireClause = "import_require_clause",
    ImportSpecifier = "import_specifier",
    ImportStatement = "import_statement",
    IndexSignature = "index_signature",
    IndexTypeQuery = "index_type_query",
    InterfaceDeclaration = "interface_declaration",
    InternalModule = "internal_module",
    IntersectionType = "intersection_type",
    LabeledStatement = "labeled_statement",
    LabeledTupleTypeMember = "labeled_tuple_type_member",
    LexicalDeclaration = "lexical_declaration",
    LiteralType = "literal_type",
    LookupType = "lookup_type",
    MappedTypeClause = "mapped_type_clause",
    MemberExpression = "member_expression",
    MetaProperty = "meta_property",
    MethodDefinition = "method_definition",
    MethodSignature = "method_signature",
    Module = "module",
    NamedImports = "named_imports",
    NamespaceImport = "namespace_import",
    NestedIdentifier = "nested_identifier",
    NestedTypeIdentifier = "nested_type_identifier",
    NewExpression = "new_expression",
    NonNullExpression = "non_null_expression",
    Number = "number",
    Object = "object",
    ObjectPattern = "object_pattern",
    ObjectType = "object_type",
    OmittingTypeAnnotation = "omitting_type_annotation",
    OptingTypeAnnotation = "opting_type_annotation",
    OptionalIdentifier = "optional_identifier",
    OptionalParameter = "optional_parameter",
    Pair = "pair",
    ParenthesizedExpression = "parenthesized_expression",
    ParenthesizedType = "parenthesized_type",
    PredefinedType = "predefined_type",
    Program = "program",
    PropertySignature = "property_signature",
    PublicFieldDefinition = "public_field_definition",
    Regex = "regex",
    RequiredParameter = "required_parameter",
    RestIdentifier = "rest_identifier",
    RestParameter = "rest_parameter",
    ReturnStatement = "return_statement",
    SequenceExpression = "sequence_expression",
    SpreadElement = "spread_element",
    StatementBlock = "statement_block",
    String = "string",
    SubscriptExpression = "subscript_expression",
    SwitchBody = "switch_body",
    SwitchCase = "switch_case",
    SwitchDefault = "switch_default",
    SwitchStatement = "switch_statement",
    TemplateString = "template_string",
    TemplateSubstitution = "template_substitution",
    TernaryExpression = "ternary_expression",
    ThrowStatement = "throw_statement",
    TryStatement = "try_statement",
    TupleType = "tuple_type",
    TypeAliasDeclaration = "type_alias_declaration",
    TypeAnnotation = "type_annotation",
    TypeArguments = "type_arguments",
    TypeAssertion = "type_assertion",
    TypeParameter = "type_parameter",
    TypeParameters = "type_parameters",
    TypePredicate = "type_predicate",
    TypePredicateAnnotation = "type_predicate_annotation",
    TypeQuery = "type_query",
    UnaryExpression = "unary_expression",
    UnionType = "union_type",
    UpdateExpression = "update_expression",
    VariableDeclaration = "variable_declaration",
    VariableDeclarator = "variable_declarator",
    WhileStatement = "while_statement",
    WithStatement = "with_statement",
    YieldExpression = "yield_expression",
    BigintLiteral = "bigint_literal",
    BinaryLiteral = "binary_literal",
    Comment = "comment",
    EscapeSequence = "escape_sequence",
    False = "false",
    FloatLiteral = "float_literal",
    HashBangLine = "hash_bang_line",
    HexLiteral = "hex_literal",
    Identifier = "identifier",
    IntLiteral = "int_literal",
    Null = "null",
    OctalLiteral = "octal_literal",
    PropertyIdentifier = "property_identifier",
    Readonly = "readonly",
    RegexFlags = "regex_flags",
    RegexPattern = "regex_pattern",
    ShorthandPropertyIdentifier = "shorthand_property_identifier",
    StatementIdentifier = "statement_identifier",
    Super = "super",
    This = "this",
    True = "true",
    TypeIdentifier = "type_identifier",
  }

  export type UnnamedType =
    | "!"
    | "!="
    | "!=="
    | '"'
    | "${"
    | "%"
    | "%="
    | "&"
    | "&&"
    | "&&="
    | "&="
    | "'"
    | "("
    | ")"
    | "*"
    | "**"
    | "**="
    | "*="
    | "+"
    | "++"
    | "+="
    | ","
    | "-"
    | "--"
    | "-="
    | "-?:"
    | "."
    | "..."
    | "/"
    | "/="
    | ":"
    | ";"
    | "<"
    | "<<"
    | "<<="
    | "<="
    | "="
    | "=="
    | "==="
    | "=>"
    | ">"
    | ">="
    | ">>"
    | ">>="
    | ">>>"
    | ">>>="
    | "?"
    | "?."
    | "?:"
    | "??"
    | "??="
    | "@"
    | "["
    | "]"
    | "^"
    | "^="
    | "`"
    | "abstract"
    | "as"
    | SyntaxType.Asserts; // both named and unnamed 
    | "async"  | "await"  | "boolean"  | "break"  | "case"  | "catch"  | SyntaxType.Class // both named and unnamed 
    | "const"  | "continue"  | "debugger"  | "declare"  | "default"  | "delete"  | "do"  | "else"  | "enum"  | "export"  | "extends"  | "finally"  | "float"  | "for"  | "from"  | SyntaxType.Function // both named and unnamed
    | "get"  | "global"  | "if"  | "implements"  | SyntaxType.Import // both named and unnamed
    | "in"  | "instanceof"  | "int"  | "interface"  | "is"  | "keyof"  | "let"  | SyntaxType.Module // both named and unnamed 
    | "namespace"  | "new"  | "of"  | "private"  | "protected"  | "public"  | "require"  | "return"  | "set"  | "static"  | SyntaxType.String // both named and unnamed 
    | "switch"  | "symbol"  | "target"  | "throw"  | "try"  | "type"  | "typeof"  | "var"  | "void"  | "while"  | "with"  | "yield"  | "{"  | "{|"  | "|"  | "|="  | "||"  | "||="  | "|}"  | "}"  | "~"  ;
    export type TypeString = SyntaxType | UnnamedType;
    export type SyntaxNode =   | DeclarationNode  | DestructuringPatternNode  | ExpressionNode  | StatementNode  | AbstractClassDeclarationNode  | AbstractMethodSignatureNode  | AccessibilityModifierNode  | AmbientDeclarationNode  | ArgumentsNode  | ArrayNode  | ArrayPatternNode  | ArrayTypeNode  | ArrowFunctionNode  | AsExpressionNode  | AssertsNode  | AssignmentExpressionNode  | AssignmentPatternNode  | AugmentedAssignmentExpressionNode  | AwaitExpressionNode  | BinaryExpressionNode  | BreakStatementNode  | CallExpressionNode  | CallSignatureNode  | CatchClauseNode  | CharNode  | ClassNode  | ClassBodyNode  | ClassDeclarationNode  | ClassHeritageNode  | ComputedPropertyNameNode  | ConditionalTypeNode  | ConstraintNode  | ConstructSignatureNode  | ConstructorTypeNode  | ContinueStatementNode  | DebuggerStatementNode  | DecoratorNode  | DefaultTypeNode  | DoStatementNode  | ElseClauseNode  | EmptyStatementNode  | EnumAssignmentNode  | EnumBodyNode  | EnumDeclarationNode  | ExistentialTypeNode  | ExportClauseNode  | ExportSpecifierNode  | ExportStatementNode  | ExpressionStatementNode  | ExtendsClauseNode  | FinallyClauseNode  | FlowMaybeTypeNode  | ForInStatementNode  | ForStatementNode  | FormalParametersNode  | FunctionNode  | FunctionDeclarationNode  | FunctionSignatureNode  | FunctionTypeNode  | GeneratorFunctionNode  | GeneratorFunctionDeclarationNode  | GenericTypeNode  | IfStatementNode  | ImplementsClauseNode  | ImportNode  | ImportAliasNode  | ImportClauseNode  | ImportRequireClauseNode  | ImportSpecifierNode  | ImportStatementNode  | IndexSignatureNode  | IndexTypeQueryNode  | InterfaceDeclarationNode  | InternalModuleNode  | IntersectionTypeNode  | LabeledStatementNode  | LabeledTupleTypeMemberNode  | LexicalDeclarationNode  | LiteralTypeNode  | LookupTypeNode  | MappedTypeClauseNode  | MemberExpressionNode  | MetaPropertyNode  | MethodDefinitionNode  | MethodSignatureNode  | ModuleNode  | NamedImportsNode  | NamespaceImportNode  | NestedIdentifierNode  | NestedTypeIdentifierNode  | NewExpressionNode  | NonNullExpressionNode  | NumberNode  | ObjectNode  | ObjectPatternNode  | ObjectTypeNode  | OmittingTypeAnnotationNode  | OptingTypeAnnotationNode  | OptionalIdentifierNode  | OptionalParameterNode  | PairNode  | ParenthesizedExpressionNode  | ParenthesizedTypeNode  | PredefinedTypeNode  | ProgramNode  | PropertySignatureNode  | PublicFieldDefinitionNode  | RegexNode  | RequiredParameterNode  | RestIdentifierNode  | RestParameterNode  | ReturnStatementNode  | SequenceExpressionNode  | SpreadElementNode  | StatementBlockNode  | StringNode  | SubscriptExpressionNode  | SwitchBodyNode  | SwitchCaseNode  | SwitchDefaultNode  | SwitchStatementNode  | TemplateStringNode  | TemplateSubstitutionNode  | TernaryExpressionNode  | ThrowStatementNode  | TryStatementNode  | TupleTypeNode  | TypeAliasDeclarationNode  | TypeAnnotationNode  | TypeArgumentsNode  | TypeAssertionNode  | TypeParameterNode  | TypeParametersNode  | TypePredicateNode  | TypePredicateAnnotationNode  | TypeQueryNode  | UnaryExpressionNode  | UnionTypeNode  | UpdateExpressionNode  | VariableDeclarationNode  | VariableDeclaratorNode  | WhileStatementNode  | WithStatementNode  | YieldExpressionNode  | UnnamedNode<"!">  | UnnamedNode<"!=">  | UnnamedNode<"!==">  | UnnamedNode<"\"">  | UnnamedNode<"${">  | UnnamedNode<"%">  | UnnamedNode<"%=">  | UnnamedNode<"&">  | UnnamedNode<"&&">  | UnnamedNode<"&&=">  | UnnamedNode<"&=">  | UnnamedNode<"'">  | UnnamedNode<"(">  | UnnamedNode<")">  | UnnamedNode<"*">  | UnnamedNode<"**">  | UnnamedNode<"**=">  | UnnamedNode<"*=">  | UnnamedNode<"+">  | UnnamedNode<"++">  | UnnamedNode<"+=">  | UnnamedNode<",">  | UnnamedNode<"-">  | UnnamedNode<"--">  | UnnamedNode<"-=">  | UnnamedNode<"-?:">  | UnnamedNode<".">  | UnnamedNode<"...">  | UnnamedNode<"/">  | UnnamedNode<"/=">  | UnnamedNode<":">  | UnnamedNode<";">  | UnnamedNode<"<">  | UnnamedNode<"<<">  | UnnamedNode<"<<=">  | UnnamedNode<"<=">  | UnnamedNode<"=">  | UnnamedNode<"==">  | UnnamedNode<"===">  | UnnamedNode<"=>">  | UnnamedNode<">">  | UnnamedNode<">=">  | UnnamedNode<">>">  | UnnamedNode<">>=">  | UnnamedNode<">>>">  | UnnamedNode<">>>=">  | UnnamedNode<"?">  | UnnamedNode<"?.">  | UnnamedNode<"?:">
    | UnnamedNode<"??">  | UnnamedNode<"??=">  | UnnamedNode<"@">  | UnnamedNode<"[">  | UnnamedNode<"]">  | UnnamedNode<"^">  | UnnamedNode<"^=">  | UnnamedNode<"`">  | UnnamedNode<"abstract">  | UnnamedNode<"as">  | UnnamedNode<SyntaxType.Asserts>  | UnnamedNode<"async">  | UnnamedNode<"await">  | BigintLiteralNode  | BinaryLiteralNode  | UnnamedNode<"boolean">  | UnnamedNode<"break">  | UnnamedNode<"case">  | UnnamedNode<"catch">  | UnnamedNode<SyntaxType.Class>  | CommentNode  | UnnamedNode<"const">  | UnnamedNode<"continue">  | UnnamedNode<"debugger">  | UnnamedNode<"declare">  | UnnamedNode<"default">  | UnnamedNode<"delete">  | UnnamedNode<"do">  | UnnamedNode<"else">  | UnnamedNode<"enum">  | EscapeSequenceNode  | UnnamedNode<"export">  | UnnamedNode<"extends">  | FalseNode  | UnnamedNode<"finally">  | UnnamedNode<"float">  | FloatLiteralNode  | UnnamedNode<"for">  | UnnamedNode<"from">  | UnnamedNode<SyntaxType.Function>  | UnnamedNode<"get">  | UnnamedNode<"global">  | HashBangLineNode  | HexLiteralNode  | IdentifierNode  | UnnamedNode<"if">  | UnnamedNode<"implements">  | UnnamedNode<SyntaxType.Import>  | UnnamedNode<"in">  | UnnamedNode<"instanceof">  | UnnamedNode<"int">  | IntLiteralNode  | UnnamedNode<"interface">  | UnnamedNode<"is">  | UnnamedNode<"keyof">  | UnnamedNode<"let">  | UnnamedNode<SyntaxType.Module>  | UnnamedNode<"namespace">  | UnnamedNode<"new">  | NullNode  | OctalLiteralNode  | UnnamedNode<"of">  | UnnamedNode<"private">  | PropertyIdentifierNode  | UnnamedNode<"protected">  | UnnamedNode<"public">  | ReadonlyNode  | RegexFlagsNode  | RegexPatternNode  | UnnamedNode<"require">  | UnnamedNode<"return">  | UnnamedNode<"set">  | ShorthandPropertyIdentifierNode  | StatementIdentifierNode  | UnnamedNode<"static">  | UnnamedNode<SyntaxType.String>  | SuperNode  | UnnamedNode<"switch">  | UnnamedNode<"symbol">  | UnnamedNode<"target">  | ThisNode  | UnnamedNode<"throw">  | TrueNode  | UnnamedNode<"try">  | UnnamedNode<"type">  | TypeIdentifierNode  | UnnamedNode<"typeof">  | UnnamedNode<"var">  | UnnamedNode<"void">  | UnnamedNode<"while">  | UnnamedNode<"with">  | UnnamedNode<"yield">  | UnnamedNode<"{">  | UnnamedNode<"{|">  | UnnamedNode<"|">  | UnnamedNode<"|=">  | UnnamedNode<"||">  | UnnamedNode<"||=">  | UnnamedNode<"|}">  | UnnamedNode<"}">  | UnnamedNode<"~">  | ErrorNode  ;export type DeclarationNode =   | AbstractClassDeclarationNode  | AmbientDeclarationNode  | ClassDeclarationNode  | EnumDeclarationNode  | FunctionDeclarationNode  | FunctionSignatureNode  | GeneratorFunctionDeclarationNode  | ImportAliasNode  | InterfaceDeclarationNode  | InternalModuleNode  | LexicalDeclarationNode  | ModuleNode  | TypeAliasDeclarationNode  | VariableDeclarationNode  ;export type DestructuringPatternNode =   | ArrayPatternNode  | ObjectPatternNode  ;export type ExpressionNode =   | ArrayNode  | ArrowFunctionNode  | AsExpressionNode  | AssignmentExpressionNode  | AugmentedAssignmentExpressionNode  | AwaitExpressionNode  | BigintLiteralNode  | BinaryExpressionNode  | BinaryLiteralNode  | CallExpressionNode  | CharNode  | ClassNode  | FalseNode  | FloatLiteralNode  | FunctionNode  | GeneratorFunctionNode  | HexLiteralNode  | IdentifierNode  | ImportNode  | IntLiteralNode  | InternalModuleNode  | MemberExpressionNode  | MetaPropertyNode  | NewExpressionNode  | NonNullExpressionNode  | NullNode  | ObjectNode  | OctalLiteralNode  | ParenthesizedExpressionNode  | RegexNode  | StringNode  | SubscriptExpressionNode  | SuperNode  | TemplateStringNode  | TernaryExpressionNode  | ThisNode  | TrueNode  | TypeAssertionNode  | UnaryExpressionNode  | UpdateExpressionNode  | YieldExpressionNode  ;export type StatementNode =   | DeclarationNode  | BreakStatementNode  | ContinueStatementNode  | DebuggerStatementNode  | DoStatementNode  | EmptyStatementNode  | ExportStatementNode  | ExpressionStatementNode  | ForInStatementNode  | ForStatementNode  | IfStatementNode  | ImportStatementNode  | LabeledStatementNode  | ReturnStatementNode  | StatementBlockNode  | SwitchStatementNode  | ThrowStatementNode  | TryStatementNode  | WhileStatementNode  | WithStatementNode  ;export interface AbstractClassDeclarationNode extends NamedNodeBase {  type: SyntaxType.AbstractClassDeclaration;  bodyNode: ClassBodyNode;  decoratorNodes: DecoratorNode[];  nameNode: TypeIdentifierNode;  type_parametersNode?: TypeParametersNode;}export interface AbstractMethodSignatureNode extends NamedNodeBase {  type: SyntaxType.AbstractMethodSignature;  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface AccessibilityModifierNode extends NamedNodeBase {  type: SyntaxType.AccessibilityModifier;}export interface AmbientDeclarationNode extends NamedNodeBase {  type: SyntaxType.AmbientDeclaration;}export interface ArgumentsNode extends NamedNodeBase {  type: SyntaxType.Arguments;}export interface ArrayNode extends NamedNodeBase {  type: SyntaxType.Array;}export interface ArrayPatternNode extends NamedNodeBase {  type: SyntaxType.ArrayPattern;}export interface ArrayTypeNode extends NamedNodeBase {  type: SyntaxType.ArrayType;}export interface ArrowFunctionNode extends NamedNodeBase {  type: SyntaxType.ArrowFunction;  bodyNode: ExpressionNode | StatementBlockNode;  parameterNode?: IdentifierNode;  parametersNode?: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface AsExpressionNode extends NamedNodeBase {  type: SyntaxType.AsExpression;}export interface AssertsNode extends NamedNodeBase {  type: SyntaxType.Asserts;}export interface AssignmentExpressionNode extends NamedNodeBase {  type: SyntaxType.AssignmentExpression;  leftNode: DestructuringPatternNode | IdentifierNode | MemberExpressionNode | NonNullExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;  rightNode: ExpressionNode;}export interface AssignmentPatternNode extends NamedNodeBase {  type: SyntaxType.AssignmentPattern;  leftNode: DestructuringPatternNode | ShorthandPropertyIdentifierNode;  rightNode: ExpressionNode;}export interface AugmentedAssignmentExpressionNode extends NamedNodeBase {  type: SyntaxType.AugmentedAssignmentExpression;  leftNode: IdentifierNode | MemberExpressionNode | NonNullExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;  rightNode: ExpressionNode;}export interface AwaitExpressionNode extends NamedNodeBase {  type: SyntaxType.AwaitExpression;}export interface BinaryExpressionNode extends NamedNodeBase {  type: SyntaxType.BinaryExpression;  leftNode: ExpressionNode;  operatorNode: UnnamedNode<"!="> | UnnamedNode<"!=="> | UnnamedNode<"%"> | UnnamedNode<"&"> | UnnamedNode<"&&"> | UnnamedNode<"*"> | UnnamedNode<"**"> | UnnamedNode<"+"> | UnnamedNode<"-"> | UnnamedNode<"/"> | UnnamedNode<"<"> | UnnamedNode<"<<"> | UnnamedNode<"<="> | UnnamedNode<"=="> | UnnamedNode<"==="> | UnnamedNode<">"> | UnnamedNode<">="> | UnnamedNode<">>"> | UnnamedNode<">>>"> | UnnamedNode<"??"> | UnnamedNode<"^"> | UnnamedNode<"in"> | UnnamedNode<"instanceof"> | UnnamedNode<"|"> | UnnamedNode<"||">;  rightNode: ExpressionNode;}export interface BreakStatementNode extends NamedNodeBase {  type: SyntaxType.BreakStatement;  labelNode?: StatementIdentifierNode;}export interface CallExpressionNode extends NamedNodeBase {  type: SyntaxType.CallExpression;  argumentsNode: ArgumentsNode | TemplateStringNode;  functionNode: ExpressionNode;  type_argumentsNode?: TypeArgumentsNode;}export interface CallSignatureNode extends NamedNodeBase {  type: SyntaxType.CallSignature;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface CatchClauseNode extends NamedNodeBase {  type: SyntaxType.CatchClause;  bodyNode: StatementBlockNode;  parameterNode?: DestructuringPatternNode | IdentifierNode;}export interface CharNode extends NamedNodeBase {  type: SyntaxType.Char;}export interface ClassNode extends NamedNodeBase {  type: SyntaxType.Class;  bodyNode: ClassBodyNode;  decoratorNodes: DecoratorNode[];  nameNode?: TypeIdentifierNode;  type_parametersNode?: TypeParametersNode;}export interface ClassBodyNode extends NamedNodeBase {  type: SyntaxType.ClassBody;}export interface ClassDeclarationNode extends NamedNodeBase {  type: SyntaxType.ClassDeclaration;  bodyNode: ClassBodyNode;  decoratorNodes: DecoratorNode[];  nameNode: TypeIdentifierNode;  type_parametersNode?: TypeParametersNode;}export interface ClassHeritageNode extends NamedNodeBase {  type: SyntaxType.ClassHeritage;}export interface ComputedPropertyNameNode extends NamedNodeBase {  type: SyntaxType.ComputedPropertyName;}export interface ConditionalTypeNode extends NamedNodeBase {  type: SyntaxType.ConditionalType;  alternativeNode: ArrayTypeNode | ConditionalTypeNode | ConstructorTypeNode | ExistentialTypeNode | FlowMaybeTypeNode | FunctionTypeNode | GenericTypeNode | IndexTypeQueryNode | IntersectionTypeNode | LiteralTypeNode | LookupTypeNode | NestedTypeIdentifierNode | ObjectTypeNode | ParenthesizedTypeNode | PredefinedTypeNode | ThisNode | TupleTypeNode | TypeIdentifierNode | TypeQueryNode | UnionTypeNode;  consequenceNode: ArrayTypeNode | ConditionalTypeNode | ConstructorTypeNode | ExistentialTypeNode | FlowMaybeTypeNode | FunctionTypeNode | GenericTypeNode | IndexTypeQueryNode | IntersectionTypeNode | LiteralTypeNode | LookupTypeNode | NestedTypeIdentifierNode | ObjectTypeNode | ParenthesizedTypeNode | PredefinedTypeNode | ThisNode | TupleTypeNode | TypeIdentifierNode | TypeQueryNode | UnionTypeNode;  leftNode: ArrayTypeNode | ConditionalTypeNode | ConstructorTypeNode | ExistentialTypeNode | FlowMaybeTypeNode | FunctionTypeNode | GenericTypeNode | IndexTypeQueryNode | IntersectionTypeNode | LiteralTypeNode | LookupTypeNode | NestedTypeIdentifierNode | ObjectTypeNode | ParenthesizedTypeNode | PredefinedTypeNode | ThisNode | TupleTypeNode | TypeIdentifierNode | TypeQueryNode | UnionTypeNode;  rightNode: ArrayTypeNode | ConditionalTypeNode | ConstructorTypeNode | ExistentialTypeNode | FlowMaybeTypeNode | FunctionTypeNode | GenericTypeNode | IndexTypeQueryNode | IntersectionTypeNode | LiteralTypeNode | LookupTypeNode | NestedTypeIdentifierNode | ObjectTypeNode | ParenthesizedTypeNode | PredefinedTypeNode | ThisNode | TupleTypeNode | TypeIdentifierNode | TypeQueryNode | UnionTypeNode;}export interface ConstraintNode extends NamedNodeBase {  type: SyntaxType.Constraint;}export interface ConstructSignatureNode extends NamedNodeBase {  type: SyntaxType.ConstructSignature;}export interface ConstructorTypeNode extends NamedNodeBase {  type: SyntaxType.ConstructorType;}export interface ContinueStatementNode extends NamedNodeBase {  type: SyntaxType.ContinueStatement;  labelNode?: StatementIdentifierNode;}export interface DebuggerStatementNode extends NamedNodeBase {  type: SyntaxType.DebuggerStatement;}export interface DecoratorNode extends NamedNodeBase {  type: SyntaxType.Decorator;}export interface DefaultTypeNode extends NamedNodeBase {  type: SyntaxType.DefaultType;}export interface DoStatementNode extends NamedNodeBase {  type: SyntaxType.DoStatement;  bodyNode: StatementNode;  conditionNode: ParenthesizedExpressionNode;}export interface ElseClauseNode extends NamedNodeBase {  type: SyntaxType.ElseClause;}export interface EmptyStatementNode extends NamedNodeBase {  type: SyntaxType.EmptyStatement;}export interface EnumAssignmentNode extends NamedNodeBase {  type: SyntaxType.EnumAssignment;  valueNode: ExpressionNode;}export interface EnumBodyNode extends NamedNodeBase {  type: SyntaxType.EnumBody;}export interface EnumDeclarationNode extends NamedNodeBase {  type: SyntaxType.EnumDeclaration;  bodyNode: EnumBodyNode;  nameNode: IdentifierNode;}export interface ExistentialTypeNode extends NamedNodeBase {  type: SyntaxType.ExistentialType;}export interface ExportClauseNode extends NamedNodeBase {  type: SyntaxType.ExportClause;}export interface ExportSpecifierNode extends NamedNodeBase {  type: SyntaxType.ExportSpecifier;  aliasNode?: IdentifierNode;  nameNode: IdentifierNode;}export interface ExportStatementNode extends NamedNodeBase {  type: SyntaxType.ExportStatement;  declarationNode?: DeclarationNode;  decoratorNodes: DecoratorNode[];  sourceNode?: StringNode;  valueNode?: ExpressionNode;}export interface ExpressionStatementNode extends NamedNodeBase {  type: SyntaxType.ExpressionStatement;}export interface ExtendsClauseNode extends NamedNodeBase {  type: SyntaxType.ExtendsClause;}export interface FinallyClauseNode extends NamedNodeBase {  type: SyntaxType.FinallyClause;  bodyNode: StatementBlockNode;}export interface FlowMaybeTypeNode extends NamedNodeBase {  type: SyntaxType.FlowMaybeType;}export interface ForInStatementNode extends NamedNodeBase {  type: SyntaxType.ForInStatement;  bodyNode: StatementNode;  leftNode: DestructuringPatternNode | IdentifierNode | MemberExpressionNode | NonNullExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;  rightNode: ExpressionNode | SequenceExpressionNode;}export interface ForStatementNode extends NamedNodeBase {  type: SyntaxType.ForStatement;  bodyNode: StatementNode;  conditionNode: EmptyStatementNode | ExpressionStatementNode;  incrementNode?: ExpressionNode | SequenceExpressionNode;  initializerNode: EmptyStatementNode | ExpressionStatementNode | LexicalDeclarationNode | VariableDeclarationNode;}export interface FormalParametersNode extends NamedNodeBase {  type: SyntaxType.FormalParameters;  decoratorNodes: DecoratorNode[];}export interface FunctionNode extends NamedNodeBase {  type: SyntaxType.Function;  bodyNode: StatementBlockNode;  nameNode?: IdentifierNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface FunctionDeclarationNode extends NamedNodeBase {  type: SyntaxType.FunctionDeclaration;  bodyNode: StatementBlockNode;  nameNode: IdentifierNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface FunctionSignatureNode extends NamedNodeBase {  type: SyntaxType.FunctionSignature;  nameNode: IdentifierNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface FunctionTypeNode extends NamedNodeBase {  type: SyntaxType.FunctionType;}export interface GeneratorFunctionNode extends NamedNodeBase {  type: SyntaxType.GeneratorFunction;  bodyNode: StatementBlockNode;  nameNode?: IdentifierNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface GeneratorFunctionDeclarationNode extends NamedNodeBase {  type: SyntaxType.GeneratorFunctionDeclaration;  bodyNode: StatementBlockNode;  nameNode: IdentifierNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface GenericTypeNode extends NamedNodeBase {  type: SyntaxType.GenericType;}export interface IfStatementNode extends NamedNodeBase {  type: SyntaxType.IfStatement;  alternativeNode?: ElseClauseNode;  conditionNode: ParenthesizedExpressionNode;  consequenceNode: StatementNode;}export interface ImplementsClauseNode extends NamedNodeBase {  type: SyntaxType.ImplementsClause;}export interface ImportNode extends NamedNodeBase {  type: SyntaxType.Import;}export interface ImportAliasNode extends NamedNodeBase {  type: SyntaxType.ImportAlias;}export interface ImportClauseNode extends NamedNodeBase {  type: SyntaxType.ImportClause;}export interface ImportRequireClauseNode extends NamedNodeBase {  type: SyntaxType.ImportRequireClause;}export interface ImportSpecifierNode extends NamedNodeBase {  type: SyntaxType.ImportSpecifier;  aliasNode?: IdentifierNode;  nameNode: IdentifierNode;}export interface ImportStatementNode extends NamedNodeBase {  type: SyntaxType.ImportStatement;  sourceNode?: StringNode;}export interface IndexSignatureNode extends NamedNodeBase {  type: SyntaxType.IndexSignature;  signNode?: UnnamedNode<"-">;}export interface IndexTypeQueryNode extends NamedNodeBase {  type: SyntaxType.IndexTypeQuery;}export interface InterfaceDeclarationNode extends NamedNodeBase {  type: SyntaxType.InterfaceDeclaration;  bodyNode: ObjectTypeNode;  nameNode: TypeIdentifierNode;  type_parametersNode?: TypeParametersNode;}export interface InternalModuleNode extends NamedNodeBase {  type: SyntaxType.InternalModule;  bodyNode?: StatementBlockNode;  nameNode: IdentifierNode | NestedIdentifierNode | StringNode;}export interface IntersectionTypeNode extends NamedNodeBase {  type: SyntaxType.IntersectionType;}export interface LabeledStatementNode extends NamedNodeBase {  type: SyntaxType.LabeledStatement;  labelNode: StatementIdentifierNode;}export interface LabeledTupleTypeMemberNode extends NamedNodeBase {  type: SyntaxType.LabeledTupleTypeMember;}export interface LexicalDeclarationNode extends NamedNodeBase {  type: SyntaxType.LexicalDeclaration;}export interface LiteralTypeNode extends NamedNodeBase {  type: SyntaxType.LiteralType;}export interface LookupTypeNode extends NamedNodeBase {  type: SyntaxType.LookupType;}export interface MappedTypeClauseNode extends NamedNodeBase {  type: SyntaxType.MappedTypeClause;}export interface MemberExpressionNode extends NamedNodeBase {  type: SyntaxType.MemberExpression;  objectNode: ExpressionNode;  propertyNode: PropertyIdentifierNode;}export interface MetaPropertyNode extends NamedNodeBase {  type: SyntaxType.MetaProperty;}export interface MethodDefinitionNode extends NamedNodeBase {  type: SyntaxType.MethodDefinition;  bodyNode: StatementBlockNode;  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface MethodSignatureNode extends NamedNodeBase {  type: SyntaxType.MethodSignature;  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  parametersNode: FormalParametersNode;  return_typeNode?: AssertsNode | TypeAnnotationNode | TypePredicateAnnotationNode;  type_parametersNode?: TypeParametersNode;}export interface ModuleNode extends NamedNodeBase {  type: SyntaxType.Module;  bodyNode?: StatementBlockNode;  nameNode: IdentifierNode | NestedIdentifierNode | StringNode;}export interface NamedImportsNode extends NamedNodeBase {  type: SyntaxType.NamedImports;}export interface NamespaceImportNode extends NamedNodeBase {  type: SyntaxType.NamespaceImport;}export interface NestedIdentifierNode extends NamedNodeBase {  type: SyntaxType.NestedIdentifier;}export interface NestedTypeIdentifierNode extends NamedNodeBase {  type: SyntaxType.NestedTypeIdentifier;  moduleNode: IdentifierNode | NestedIdentifierNode;  nameNode: TypeIdentifierNode;}export interface NewExpressionNode extends NamedNodeBase {  type: SyntaxType.NewExpression;  argumentsNode?: ArgumentsNode;  constructorNode: ArrayNode | ArrowFunctionNode | BigintLiteralNode | BinaryLiteralNode | CallExpressionNode | CharNode | ClassNode | FalseNode | FloatLiteralNode | FunctionNode | GeneratorFunctionNode | HexLiteralNode | IdentifierNode | ImportNode | IntLiteralNode | MemberExpressionNode | MetaPropertyNode | NullNode | ObjectNode | OctalLiteralNode | ParenthesizedExpressionNode | RegexNode | StringNode | SubscriptExpressionNode | SuperNode | TemplateStringNode | ThisNode | TrueNode;  type_argumentsNode?: TypeArgumentsNode;}export interface NonNullExpressionNode extends NamedNodeBase {  type: SyntaxType.NonNullExpression;}export interface NumberNode extends NamedNodeBase {  type: SyntaxType.Number;}export interface ObjectNode extends NamedNodeBase {  type: SyntaxType.Object;}export interface ObjectPatternNode extends NamedNodeBase {  type: SyntaxType.ObjectPattern;}export interface ObjectTypeNode extends NamedNodeBase {  type: SyntaxType.ObjectType;}export interface OmittingTypeAnnotationNode extends NamedNodeBase {  type: SyntaxType.OmittingTypeAnnotation;}export interface OptingTypeAnnotationNode extends NamedNodeBase {  type: SyntaxType.OptingTypeAnnotation;}export interface OptionalIdentifierNode extends NamedNodeBase {  type: SyntaxType.OptionalIdentifier;}export interface OptionalParameterNode extends NamedNodeBase {  type: SyntaxType.OptionalParameter;  valueNode?: ExpressionNode;}export interface PairNode extends NamedNodeBase {  type: SyntaxType.Pair;  keyNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  valueNode: ExpressionNode;}export interface ParenthesizedExpressionNode extends NamedNodeBase {  type: SyntaxType.ParenthesizedExpression;}export interface ParenthesizedTypeNode extends NamedNodeBase {  type: SyntaxType.ParenthesizedType;}export interface PredefinedTypeNode extends NamedNodeBase {  type: SyntaxType.PredefinedType;}export interface ProgramNode extends NamedNodeBase {  type: SyntaxType.Program;}export interface PropertySignatureNode extends NamedNodeBase {  type: SyntaxType.PropertySignature;  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  typeNode?: TypeAnnotationNode;}export interface PublicFieldDefinitionNode extends NamedNodeBase {  type: SyntaxType.PublicFieldDefinition;  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;  typeNode?: TypeAnnotationNode;  valueNode?: ExpressionNode;}export interface RegexNode extends NamedNodeBase {  type: SyntaxType.Regex;  flagsNode?: RegexFlagsNode;  patternNode: RegexPatternNode;}export interface RequiredParameterNode extends NamedNodeBase {  type: SyntaxType.RequiredParameter;  valueNode?: ExpressionNode;}export interface RestIdentifierNode extends NamedNodeBase {  type: SyntaxType.RestIdentifier;}export interface RestParameterNode extends NamedNodeBase {  type: SyntaxType.RestParameter;}export interface ReturnStatementNode extends NamedNodeBase {  type: SyntaxType.ReturnStatement;}export interface SequenceExpressionNode extends NamedNodeBase {  type: SyntaxType.SequenceExpression;  leftNode: ExpressionNode;  rightNode: ExpressionNode | SequenceExpressionNode;}export interface SpreadElementNode extends NamedNodeBase {  type: SyntaxType.SpreadElement;}export interface StatementBlockNode extends NamedNodeBase {  type: SyntaxType.StatementBlock;}export interface StringNode extends NamedNodeBase {  type: SyntaxType.String;}export interface SubscriptExpressionNode extends NamedNodeBase {  type: SyntaxType.SubscriptExpression;  indexNode: ExpressionNode | SequenceExpressionNode;  objectNode: ExpressionNode;}export interface SwitchBodyNode extends NamedNodeBase {  type: SyntaxType.SwitchBody;}export interface SwitchCaseNode extends NamedNodeBase {  type: SyntaxType.SwitchCase;  valueNode: ExpressionNode | SequenceExpressionNode;}export interface SwitchDefaultNode extends NamedNodeBase {  type: SyntaxType.SwitchDefault;}export interface SwitchStatementNode extends NamedNodeBase {  type: SyntaxType.SwitchStatement;  bodyNode: SwitchBodyNode;  valueNode: ParenthesizedExpressionNode;}export interface TemplateStringNode extends NamedNodeBase {  type: SyntaxType.TemplateString;}export interface TemplateSubstitutionNode extends NamedNodeBase {  type: SyntaxType.TemplateSubstitution;}export interface TernaryExpressionNode extends NamedNodeBase {  type: SyntaxType.TernaryExpression;  alternativeNode: ExpressionNode;  conditionNode: ExpressionNode;  consequenceNode: ExpressionNode;}export interface ThrowStatementNode extends NamedNodeBase {  type: SyntaxType.ThrowStatement;}export interface TryStatementNode extends NamedNodeBase {  type: SyntaxType.TryStatement;  bodyNode: StatementBlockNode;  finalizerNode?: FinallyClauseNode;  handlerNode?: CatchClauseNode;}export interface TupleTypeNode extends NamedNodeBase {  type: SyntaxType.TupleType;}export interface TypeAliasDeclarationNode extends NamedNodeBase {  type: SyntaxType.TypeAliasDeclaration;  nameNode: TypeIdentifierNode;  type_parametersNode?: TypeParametersNode;  valueNode: ArrayTypeNode | ConditionalTypeNode | ConstructorTypeNode | ExistentialTypeNode | FlowMaybeTypeNode | FunctionTypeNode | GenericTypeNode | IndexTypeQueryNode | IntersectionTypeNode | LiteralTypeNode | LookupTypeNode | NestedTypeIdentifierNode | ObjectTypeNode | ParenthesizedTypeNode | PredefinedTypeNode | ThisNode | TupleTypeNode | TypeIdentifierNode | TypeQueryNode | UnionTypeNode;}export interface TypeAnnotationNode extends NamedNodeBase {  type: SyntaxType.TypeAnnotation;}export interface TypeArgumentsNode extends NamedNodeBase {  type: SyntaxType.TypeArguments;}export interface TypeAssertionNode extends NamedNodeBase {  type: SyntaxType.TypeAssertion;}export interface TypeParameterNode extends NamedNodeBase {  type: SyntaxType.TypeParameter;}export interface TypeParametersNode extends NamedNodeBase {  type: SyntaxType.TypeParameters;}export interface TypePredicateNode extends NamedNodeBase {  type: SyntaxType.TypePredicate;}export interface TypePredicateAnnotationNode extends NamedNodeBase {  type: SyntaxType.TypePredicateAnnotation;}export interface TypeQueryNode extends NamedNodeBase {  type: SyntaxType.TypeQuery;}export interface UnaryExpressionNode extends NamedNodeBase {  type: SyntaxType.UnaryExpression;  argumentNode: ExpressionNode;  operatorNode: UnnamedNode<"!"> | UnnamedNode<"+"> | UnnamedNode<"-"> | UnnamedNode<"delete"> | UnnamedNode<"typeof"> | UnnamedNode<"void"> | UnnamedNode<"~">;}export interface UnionTypeNode extends NamedNodeBase {  type: SyntaxType.UnionType;}export interface UpdateExpressionNode extends NamedNodeBase {  type: SyntaxType.UpdateExpression;  argumentNode: ExpressionNode;  operatorNode: UnnamedNode<"++"> | UnnamedNode<"--">;}export interface VariableDeclarationNode extends NamedNodeBase {  type: SyntaxType.VariableDeclaration;}export interface VariableDeclaratorNode extends NamedNodeBase {  type: SyntaxType.VariableDeclarator;  nameNode: DestructuringPatternNode | IdentifierNode;  typeNode?: TypeAnnotationNode;  valueNode?: ExpressionNode;}export interface WhileStatementNode extends NamedNodeBase {  type: SyntaxType.WhileStatement;  bodyNode: StatementNode;  conditionNode: ParenthesizedExpressionNode;}export interface WithStatementNode extends NamedNodeBase {  type: SyntaxType.WithStatement;  bodyNode: StatementNode;  objectNode: ParenthesizedExpressionNode;}export interface YieldExpressionNode extends NamedNodeBase {  type: SyntaxType.YieldExpression;}export interface BigintLiteralNode extends NamedNodeBase {  type: SyntaxType.BigintLiteral;}export interface BinaryLiteralNode extends NamedNodeBase {  type: SyntaxType.BinaryLiteral;}export interface CommentNode extends NamedNodeBase {  type: SyntaxType.Comment;}export interface EscapeSequenceNode extends NamedNodeBase {  type: SyntaxType.EscapeSequence;}export interface FalseNode extends NamedNodeBase {  type: SyntaxType.False;}export interface FloatLiteralNode extends NamedNodeBase {  type: SyntaxType.FloatLiteral;}export interface HashBangLineNode extends NamedNodeBase {  type: SyntaxType.HashBangLine;}export interface HexLiteralNode extends NamedNodeBase {  type: SyntaxType.HexLiteral;}export interface IdentifierNode extends NamedNodeBase {  type: SyntaxType.Identifier;}export interface IntLiteralNode extends NamedNodeBase {  type: SyntaxType.IntLiteral;}export interface NullNode extends NamedNodeBase {  type: SyntaxType.Null;}export interface OctalLiteralNode extends NamedNodeBase {  type: SyntaxType.OctalLiteral;}export interface PropertyIdentifierNode extends NamedNodeBase {  type: SyntaxType.PropertyIdentifier;}export interface ReadonlyNode extends NamedNodeBase {  type: SyntaxType.Readonly;}export interface RegexFlagsNode extends NamedNodeBase {  type: SyntaxType.RegexFlags;}export interface RegexPatternNode extends NamedNodeBase {  type: SyntaxType.RegexPattern;}export interface ShorthandPropertyIdentifierNode extends NamedNodeBase {  type: SyntaxType.ShorthandPropertyIdentifier;}export interface StatementIdentifierNode extends NamedNodeBase {  type: SyntaxType.StatementIdentifier;}export interface SuperNode extends NamedNodeBase {  type: SyntaxType.Super;}export interface ThisNode extends NamedNodeBase {  type: SyntaxType.This;}export interface TrueNode extends NamedNodeBase {  type: SyntaxType.True;}
    export interface TypeIdentifierNode extends NamedNodeBase {  type: SyntaxType.TypeIdentifier;}
}
