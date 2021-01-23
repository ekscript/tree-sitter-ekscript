const PREC = {
  COMMENT: 1, // Prefer comments over regexes
  STRING: 2, // In a string, prefer string characters over comments

  COMMA: -1,
  OBJECT: -1,
  DECLARATION: 1,
  ASSIGN: 0,
  TERNARY: 1,
  OR: 2,
  AND: 3,
  REL: 4,
  PLUS: 5,
  TIMES: 6,
  EXP: 7,
  TYPEOF: 7,
  DELETE: 8,
  VOID: 8,
  NOT: 9,
  NEG: 10,
  INC: 11,
  CALL: 12,
  NEW: 13,
  MEMBER: 14,
  AS_EXPRESSION: 1,
  TYPE_ASSERTION: 16,
  ACCESSIBILITY: 1,
  DEFINITION: 1,
  NON_NULL: 10,
  ARRAY_TYPE: 13,
  FLOW_MAYBE_TYPE: 12,
  UNION: 3,
  INTERSECTION: 3,
  FUNCTION_TYPE: 1,
};

module.exports = grammar({
  name: "ekscript",

  externals: ($) => [$._automatic_semicolon, $._template_chars, "||"],

  word: ($) => $.identifier,

  extras: ($) => [$.comment, /[\s\uFEFF\u2060\u200B\u00A0]/],

  supertypes: ($) => [
    $._statement,
    //   $._declaration,
    $._expression,
    $._destructuring_pattern,
  ],

  inline: ($) => [
    $._call_signature,
    $._primary_expression,
    $._statement,
    $._expressions,
    $._semicolon,
    $._formal_parameter,
    $._destructuring_pattern,
    $._reserved_identifier,
    $._lhs_expression,
  ],

  conflicts: ($) => [
    [$._expression, $._property_name],
    [$._expression, $._property_name, $.arrow_function],
    [$._expression, $.arrow_function],
    [$.arrow_function, $._property_name],
    [$._expression, $.method_definition],
    [$._expression, $.formal_parameters],
    [$._expression, $.rest_parameter],
    [$.labeled_statement, $._property_name],
    //  [$.assignment_pattern, $.assignment_expression],
    [$.computed_property_name, $.array],
    //  [$._for_header, $._expression],
    [$.object, $._property_name],
  ],

  word: ($) => $.identifier,

  rules: {
    program: ($) => seq(optional($.hash_bang_line), repeat($._statement)),

    hash_bang_line: ($) => /#!.*/,

    // Export declarations
    export_statement: ($) =>
      prec(
        PREC.DECLARATION,
        choice(
          seq(
            "export",
            choice(
              seq("*", $._from_clause, $._semicolon),
              seq($.export_clause, $._from_clause, $._semicolon),
              seq($.export_clause, $._semicolon)
            )
          ),
          seq(
            repeat(field("decorator", $.decorator)),
            "export",
            choice(
              field("declaration", $._declaration),
              seq("default", field("value", $._expression), $._semicolon)
            )
          ),
          seq("export", "as", "namespace", $.identifier, $._semicolon)
        )
      ),

    export_clause: ($) =>
      seq(
        "{",
        commaSep(alias($._import_export_specifier, $.export_specifier)),
        optional(","),
        "}"
      ),

    _import_export_specifier: ($) =>
      seq(
        optional(choice("typeof")),
        field("name", $.identifier),
        optional(seq("as", field("alias", $.identifier)))
      ),

    _declaration: ($) =>
      choice(
        $.function_declaration,
        $.function_signature,
        $.generator_function_declaration,
        $.class_declaration,
        // $.abstract_class_declaration,
        // $.interface_declaration,
        $.import_alias,
        // $.ambient_declaration,
        $.module,
        prec(PREC.DECLARATION, $.internal_module),
        // $.type_alias_declaration,
        $.lexical_declaration
        // $.enum_declaration
      ),

    //type_alias_declaration: ($) =>
    //  seq(
    //    "type",
    //    field("name", $._type_identifier),
    //    field("type_parameters", optional($.type_parameters)),
    //    "=",
    //    field("value", $._type),
    //    $._semicolon
    //  ),

    //interface_declaration: ($) =>
    //  seq(
    //    "interface",
    //    field("name", $._type_identifier),
    //    field("type_parameters", optional($.type_parameters)),
    //    optional($.extends_clause),
    //    field("body", $.object_type)
    //  ),

    //ambient_declaration: ($) =>
    //  seq(
    //    "declare",
    //    choice(
    //      $._declaration,
    //      seq("global", $.statement_block),
    //      seq(
    //        "module",
    //        ".",
    //        alias($.identifier, $.property_identifier),
    //        ":",
    //        $._type
    //      )
    //    )
    //  ),

    import_alias: ($) =>
      seq(
        "import",
        $.identifier,
        "=",
        choice($.identifier, $.nested_identifier),
        $._semicolon
      ),

    //enum_declaration: ($) =>
    //  seq(
    //    optional("const"),
    //    "enum",
    //    field("name", $.identifier),
    //    field("body", $.enum_body)
    //  ),

    //enum_body: ($) =>
    //  seq(
    //    "{",
    //    optional(
    //      seq(
    //        sepBy1(",", choice($._property_name, $.enum_assignment)),
    //        optional(",")
    //      )
    //    ),
    //    "}"
    //  ),

    //enum_assignment: ($) => seq($._property_name, $._initializer),

    //// Import declarations
    import: ($) => token("import"),

    import_statement: ($) =>
      prec(
        PREC.DECLARATION,
        seq(
          "import",
          optional("type"), // denotes type imports
          $.import_clause,
          $._from_clause,
          $._semicolon
        )
      ),

    import_clause: ($) =>
      choice(
        $.namespace_import,
        $.named_imports,
        seq(
          $.identifier,
          optional(seq(",", choice($.namespace_import, $.named_imports)))
        )
      ),

    _from_clause: ($) => seq("from", field("source", $.string)),

    namespace_import: ($) => seq("*", "as", $.identifier),

    named_imports: ($) =>
      seq(
        "{",
        commaSep(alias($._import_export_specifier, $.import_specifier)),
        optional(","),
        "}"
      ),

    // Statements
    _statement: ($) =>
      choice(
        $.export_statement,
        $.import_statement,
        $.debugger_statement,
        $.expression_statement,
        //    $._declaration,
        $.statement_block,

        //    $.if_statement,
        //    $.switch_statement,
        //    $.for_statement,
        //    $.for_in_statement,
        //    $.while_statement,
        //    $.do_statement,
        //    $.try_statement,
        //    $.with_statement,

        //    $.break_statement,
        //    $.continue_statement,
        $.return_statement,
        //    $.throw_statement,
        $.empty_statement,
        $.labeled_statement
      ),

    expression_statement: ($) => seq($._expressions, $._semicolon),

    lexical_declaration: ($) =>
      seq(
        choice("let", "const"),
        commaSep1($.variable_declarator),
        $._semicolon
      ),

    variable_declarator: ($) =>
      seq(
        field("name", choice($.identifier, $._destructuring_pattern)),
        optional($._initializer)
      ),

    statement_block: ($) =>
      prec.right(
        seq("{", repeat($._statement), "}", optional($._automatic_semicolon))
      ),

    //else_clause: ($) => seq("else", $._statement),

    //if_statement: ($) =>
    //  prec.right(
    //    seq(
    //      "if",
    //      field("condition", $.parenthesized_expression),
    //      field("consequence", $._statement),
    //      optional(field("alternative", $.else_clause))
    //    )
    //  ),

    //switch_statement: ($) =>
    //  seq(
    //    "switch",
    //    field("value", $.parenthesized_expression),
    //    field("body", $.switch_body)
    //  ),

    //for_statement: ($) =>
    //  seq(
    //    "for",
    //    "(",
    //    field(
    //      "initializer",
    //      choice(
    //        $.lexical_declaration,
    //        $.expression_statement,
    //        $.empty_statement
    //      )
    //    ),
    //    field("condition", choice($.expression_statement, $.empty_statement)),
    //    field("increment", optional($._expressions)),
    //    ")",
    //    field("body", $._statement)
    //  ),

    //for_in_statement: ($) =>
    //  seq("for", optional("await"), $._for_header, field("body", $._statement)),

    //_for_header: ($) =>
    //  seq(
    //    "(",
    //    optional(choice("var", "let", "const")),
    //    field("left", choice($.parenthesized_expression, $._lhs_expression)),
    //    choice("in", "of"),
    //    field("right", $._expressions),
    //    ")"
    //  ),

    //while_statement: ($) =>
    //  seq(
    //    "while",
    //    field("condition", $.parenthesized_expression),
    //    field("body", $._statement)
    //  ),

    //do_statement: ($) =>
    //  seq(
    //    "do",
    //    field("body", $._statement),
    //    "while",
    //    field("condition", $.parenthesized_expression),
    //    $._semicolon
    //  ),

    //try_statement: ($) =>
    //  seq(
    //    "try",
    //    field("body", $.statement_block),
    //    optional(field("handler", $.catch_clause)),
    //    optional(field("finalizer", $.finally_clause))
    //  ),

    //with_statement: ($) =>
    //  seq(
    //    "with",
    //    field("object", $.parenthesized_expression),
    //    field("body", $._statement)
    //  ),

    //break_statement: ($) =>
    //  seq(
    //    "break",
    //    field("label", optional(alias($.identifier, $.statement_identifier))),
    //    $._semicolon
    //  ),

    //continue_statement: ($) =>
    //  seq(
    //    "continue",
    //    field("label", optional(alias($.identifier, $.statement_identifier))),
    //    $._semicolon
    //  ),

    debugger_statement: ($) => seq("debugger", $._semicolon),

    return_statement: ($) =>
      seq("return", optional($._expressions), $._semicolon),

    //throw_statement: ($) => seq("throw", $._expressions, $._semicolon),

    empty_statement: ($) => ";",

    labeled_statement: ($) =>
      prec.dynamic(
        -1,
        seq(
          field(
            "label",
            alias(
              choice($.identifier, $._reserved_identifier),
              $.statement_identifier
            )
          ),
          ":",
          $._statement
        )
      ),

    //// Statement components
    //switch_body: ($) =>
    //  seq("{", repeat(choice($.switch_case, $.switch_default)), "}"),

    //switch_case: ($) =>
    //  seq("case", field("value", $._expressions), ":", repeat($._statement)),

    //switch_default: ($) => seq("default", ":", repeat($._statement)),

    //catch_clause: ($) =>
    //  seq(
    //    "catch",
    //    optional(
    //      seq(
    //        "(",
    //        field("parameter", choice($.identifier, $._destructuring_pattern)),
    //        ")"
    //      )
    //    ),
    //    field("body", $.statement_block)
    //  ),

    //finally_clause: ($) => seq("finally", field("body", $.statement_block)),

    parenthesized_expression: ($) =>
      seq(
        "(",
        choice(
          seq(
            $._expression
            // optional($.type_annotation)
          ),
          $.sequence_expression
        ),
        ")"
      ),

    //// Expressions
    _expressions: ($) => choice($._expression, $.sequence_expression),

    _expression: ($) =>
      choice(
        // $.as_expression,
        $.non_null_expression,
        $.internal_module,
        $.super,
        // $.type_assertion,
        $._primary_expression,
        // $.assignment_expression,
        // $.augmented_assignment_expression,
        $.await_expression
        // $.unary_expression,
        // $.binary_expression,
        // $.ternary_expression,
        // $.update_expression,
        // $.new_expression,
        // $.yield_expression
      ),

    // as_expression: ($) =>
    //   prec.left(
    //     PREC.AS_EXPRESSION,
    //     seq($._expression, "as", choice($._type, $.template_string))
    //   ),

    non_null_expression: ($) =>
      prec.left(PREC.NON_NULL, seq($._expression, "!")),

    module: ($) => seq("module", $._module),

    internal_module: ($) => seq("namespace", $._module),

    _module: ($) =>
      prec.right(
        seq(
          field("name", choice($.string, $.identifier, $.nested_identifier)),
          field("body", optional($.statement_block))
        )
      ),

    _primary_expression: ($) =>
      choice(
        $.this,
        $.super,
        $.true,
        $.false,
        $.null,
        $.identifier,
        alias($._reserved_identifier, $.identifier),
        $.number,
        $.string,
        $.template_string,
        $.regex,
        $.import,
        $.object,
        $.array,
        $.function,
        $.arrow_function,
        $.generator_function,
        $.class
        // $.parenthesized_expression,
        // $.subscript_expression,
        // $.member_expression,
        // $.call_expression
      ),

    //yield_expression: ($) =>
    //  prec.right(
    //    seq("yield", choice(seq("*", $._expression), optional($._expression)))
    //  ),

    object: ($) =>
      prec(
        PREC.OBJECT,
        seq(
          "{",
          commaSep(
            optional(
              choice(
                $.pair,
                $.spread_element,
                $.method_definition,
                // $.assignment_pattern,
                alias(
                  choice($.identifier, $._reserved_identifier),
                  $.shorthand_property_identifier
                )
              )
            )
          ),
          "}"
        )
      ),

    //assignment_pattern: ($) =>
    //  seq(
    //    field(
    //      "left",
    //      choice(
    //        alias(
    //          choice($._reserved_identifier, $.identifier),
    //          $.shorthand_property_identifier
    //        ),
    //        $._destructuring_pattern
    //      )
    //    ),
    //    "=",
    //    field("right", $._expression)
    //  ),

    array: ($) =>
      seq(
        "[",
        commaSep(optional(choice($._expression, $.spread_element))),
        "]"
      ),

    nested_identifier: ($) =>
      prec(
        PREC.MEMBER,
        seq(choice($.identifier, $.nested_identifier), ".", $.identifier)
      ),

    class: ($) =>
      seq(
        repeat(field("decorator", $.decorator)),
        "class",
        field("name", optional($._type_identifier)),
        // field("type_parameters", optional($.type_parameters)),
        optional($.class_heritage),
        field("body", $.class_body)
      ),

    class_declaration: ($) =>
      prec(
        PREC.DECLARATION,
        seq(
          repeat(field("decorator", $.decorator)),
          "class",
          field("name", $.identifier),
          // field("type_parameters", optional($.type_parameters)),
          optional($.class_heritage),
          field("body", $.class_body),
          optional($._automatic_semicolon)
        )
      ),

    //abstract_class_declaration: ($) =>
    //  seq(
    //    repeat(field("decorator", $.decorator)),
    //    "abstract",
    //    "class",
    //    field("name", $._type_identifier),
    //    field("type_parameters", optional($.type_parameters)),
    //    optional($.class_heritage),
    //    field("body", $.class_body)
    //  ),

    //abstract_method_signature: ($) =>
    //  seq(
    //    optional($.accessibility_modifier),
    //    "abstract",
    //    optional(choice("get", "set", "*")),
    //    field("name", $._property_name),
    //    optional("?"),
    //    $._call_signature
    //  ),

    class_heritage: ($) =>
      choice(
        seq($.extends_clause, optional($.implements_clause)),
        $.implements_clause
      ),

    implements_clause: ($) => seq("implements", commaSep1($._type)),
    extends_clause: ($) =>
      prec(
        PREC.EXTENDS,
        seq(
          "extends",
          commaSep1(
            choice(
              prec(
                PREC.TYPE_REFERENCE,
                choice(
                  $._type_identifier,
                  $.nested_type_identifier,
                  $.generic_type
                )
              ),
              $._expression
            )
          )
        )
      ),

    function: ($) =>
      seq(
        optional("async"),
        "function",
        field("name", optional($.identifier)),
        $._call_signature,
        field("body", $.statement_block)
      ),

    function_signature: ($) =>
      seq(
        optional("async"),
        "function",
        field("name", $.identifier),
        $._call_signature,
        $._semicolon
      ),

    function_declaration: ($) =>
      prec.right(
        PREC.DECLARATION,
        seq(
          optional("async"),
          "function",
          field("name", $.identifier),
          $._call_signature,
          field("body", $.statement_block),
          optional($._automatic_semicolon)
        )
      ),

    generator_function: ($) =>
      seq(
        optional("async"),
        "function",
        "*",
        field("name", optional($.identifier)),
        $._call_signature,
        field("body", $.statement_block)
      ),

    generator_function_declaration: ($) =>
      prec.right(
        PREC.DECLARATION,
        seq(
          optional("async"),
          "function",
          "*",
          field("name", $.identifier),
          $._call_signature,
          field("body", $.statement_block),
          optional($._automatic_semicolon)
        )
      ),

    arrow_function: ($) =>
      seq(
        optional("async"),
        choice(
          field(
            "parameter",
            choice(alias($._reserved_identifier, $.identifier), $.identifier)
          ),
          $._call_signature
        ),
        "=>",
        field("body", choice($._expression, $.statement_block))
      ),

    call_expression: ($) =>
      choice(
        prec(
          PREC.CALL,
          seq(
            field("function", $._expression),
            field("type_arguments", optional($.type_arguments)),
            field("arguments", choice($.arguments, $.template_string))
          )
        ),
        prec(
          PREC.MEMBER,
          seq(
            field("function", $._primary_expression),
            "?.",
            field("type_arguments", optional($.type_arguments)),
            field("arguments", $.arguments)
          )
        )
      ),

    new_expression: ($) =>
      prec.right(
        PREC.NEW,
        seq(
          "new",
          field("constructor", $._primary_expression),
          field("type_arguments", optional($.type_arguments)),
          field("arguments", optional($.arguments))
        )
      ),

    await_expression: ($) => seq("await", $._expression),

    member_expression: ($) =>
      prec(
        PREC.MEMBER,
        seq(
          field("object", choice($._expression, $._primary_expression)),
          choice(".", "?."),
          field("property", alias($.identifier, $.property_identifier))
        )
      ),

    subscript_expression: ($) =>
      prec.right(
        PREC.MEMBER,
        seq(
          field("object", choice($._expression, $._primary_expression)),
          optional("?."),
          "[",
          field("index", $._expressions),
          "]"
        )
      ),

    _lhs_expression: ($) =>
      choice(
        $.member_expression,
        $.subscript_expression,
        $.identifier,
        alias($._reserved_identifier, $.identifier),
        $._destructuring_pattern
      ),

    //assignment_expression: ($) =>
    //  prec.right(
    //    PREC.ASSIGN,
    //    seq(
    //      field("left", choice($.parenthesized_expression, $._lhs_expression)),
    //      "=",
    //      field("right", $._expression)
    //    )
    //  ),

    //_augmented_assignment_lhs: ($, previous) =>
    //  choice(previous, $.non_null_expression),

    //augmented_assignment_expression: ($) =>
    //  prec.right(
    //    PREC.ASSIGN,
    //    seq(
    //      field("left", $._augmented_assignment_lhs),
    //      choice(
    //        "+=",
    //        "-=",
    //        "*=",
    //        "/=",
    //        "%=",
    //        "^=",
    //        "&=",
    //        "|=",
    //        ">>=",
    //        ">>>=",
    //        "<<=",
    //        "**=",
    //        "&&=",
    //        "||=",
    //        "??="
    //      ),
    //      field("right", $._expression)
    //    )
    //  ),

    _initializer: ($) => seq("=", field("value", $._expression)),

    _destructuring_pattern: ($) =>
      choice(
        alias($.object, $.object_pattern),
        alias($.array, $.array_pattern)
      ),

    spread_element: ($) => seq("...", $._expression),

    //ternary_expression: ($) =>
    //  prec.right(
    //    PREC.TERNARY,
    //    seq(
    //      field("condition", $._expression),
    //      "?",
    //      field("consequence", $._expression),
    //      ":",
    //      field("alternative", $._expression)
    //    )
    //  ),

    //binary_expression: ($) =>
    //  choice(
    //    ...[
    //      ["&&", PREC.AND],
    //      ["||", PREC.OR],
    //      [">>", PREC.TIMES],
    //      [">>>", PREC.TIMES],
    //      ["<<", PREC.TIMES],
    //      ["&", PREC.AND],
    //      ["^", PREC.OR],
    //      ["|", PREC.OR],
    //      ["+", PREC.PLUS],
    //      ["-", PREC.PLUS],
    //      ["*", PREC.TIMES],
    //      ["/", PREC.TIMES],
    //      ["%", PREC.TIMES],
    //      ["**", PREC.EXP],
    //      ["<", PREC.REL],
    //      ["<=", PREC.REL],
    //      ["==", PREC.REL],
    //      ["===", PREC.REL],
    //      ["!=", PREC.REL],
    //      ["!==", PREC.REL],
    //      [">=", PREC.REL],
    //      [">", PREC.REL],
    //      ["??", PREC.TERNARY],
    //      ["instanceof", PREC.REL],
    //      ["in", PREC.REL],
    //    ].map(([operator, precedence]) =>
    //      prec.left(
    //        precedence,
    //        seq(
    //          field("left", $._expression),
    //          field("operator", operator),
    //          field("right", $._expression)
    //        )
    //      )
    //    )
    //  ),

    //unary_expression: ($) =>
    //  choice(
    //    ...[
    //      ["!", PREC.NOT],
    //      ["~", PREC.NOT],
    //      ["-", PREC.NEG],
    //      ["+", PREC.NEG],
    //      ["typeof", PREC.TYPEOF],
    //      ["void", PREC.VOID],
    //      ["delete", PREC.DELETE],
    //    ].map(([operator, precedence]) =>
    //      prec.left(
    //        precedence,
    //        seq(field("operator", operator), field("argument", $._expression))
    //      )
    //    )
    //  ),

    //update_expression: ($) =>
    //  prec.left(
    //    PREC.INC,
    //    choice(
    //      seq(
    //        field("argument", $._expression),
    //        field("operator", choice("++", "--"))
    //      ),
    //      seq(
    //        field("operator", choice("++", "--")),
    //        field("argument", $._expression)
    //      )
    //    )
    //  ),

    sequence_expression: ($) =>
      prec(
        PREC.COMMA,
        seq(
          field("left", $._expression),
          ",",
          field("right", choice($.sequence_expression, $._expression))
        )
      ),

    optional_parameter: ($) =>
      seq(
        $._parameter_name,
        "?",
        // optional($.type_annotation),
        optional($._initializer)
      ),

    ////
    //// Primitives
    ////
    string: ($) =>
      choice(
        seq(
          '"',
          repeat(
            choice(
              token.immediate(prec(PREC.STRING, /[^"\\\n]+|\\\r?\n/)),
              $.escape_sequence
            )
          ),
          '"'
        ),
        seq(
          "'",
          repeat(
            choice(
              token.immediate(prec(PREC.STRING, /[^'\\\n]+|\\\r?\n/)),
              $.escape_sequence
            )
          ),
          "'"
        )
      ),

    escape_sequence: ($) =>
      token.immediate(
        seq(
          "\\",
          choice(
            /[^xu0-7]/,
            /[0-7]{1,3}/,
            /x[0-9a-fA-F]{2}/,
            /u[0-9a-fA-F]{4}/,
            /u{[0-9a-fA-F]+}/
          )
        )
      ),

    //// http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: ($) =>
      token(
        prec(
          PREC.COMMENT,
          choice(seq("//", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"))
        )
      ),

    template_string: ($) =>
      seq(
        "`",
        repeat(
          choice($._template_chars, $.escape_sequence, $.template_substitution)
        ),
        "`"
      ),

    template_substitution: ($) => seq("${", $._expressions, "}"),

    regex: ($) =>
      seq(
        "/",
        field("pattern", $.regex_pattern),
        token.immediate("/"),
        optional(field("flags", $.regex_flags))
      ),

    regex_pattern: ($) =>
      token.immediate(
        repeat1(
          choice(
            seq(
              "[",
              repeat(
                choice(
                  seq("\\", /./), // escaped character
                  /[^\]\n\\]/ // any character besides ']' or '\n'
                )
              ),
              "]"
            ), // square-bracket-delimited character class
            seq("\\", /./), // escaped character
            /[^/\\\[\n]/ // any character besides '[', '\', '/', '\n'
          )
        )
      ),

    regex_flags: ($) => token.immediate(/[a-z]+/),

    number: ($) => {
      const hex_literal = seq(choice("0x", "0X"), /[\da-fA-F](_?[\da-fA-F])*/);
      const decimal_digits = /\d(_?\d)*/;
      const signed_integer = seq(optional(choice("-", "+")), decimal_digits);
      const exponent_part = seq(choice("e", "E"), signed_integer);
      const binary_literal = seq(choice("0b", "0B"), /[0-1](_?[0-1])*/);
      const octal_literal = seq(choice("0o", "0O"), /[0-7](_?[0-7])*/);
      const bigint_literal = seq(
        choice(hex_literal, binary_literal, octal_literal, decimal_digits),
        "n"
      );
      const decimal_integer_literal = choice(
        "0",
        seq(
          optional("0"),
          /[1-9]/,
          optional(seq(optional("_"), decimal_digits))
        )
      );
      const decimal_literal = choice(
        seq(
          decimal_integer_literal,
          ".",
          optional(decimal_digits),
          optional(exponent_part)
        ),
        seq(".", decimal_digits, optional(exponent_part)),
        seq(decimal_integer_literal, exponent_part),
        seq(decimal_digits)
      );
      return token(
        choice(
          hex_literal,
          decimal_literal,
          binary_literal,
          octal_literal,
          bigint_literal
        )
      );
    },

    identifier: ($) => {
      const alpha = /[^\x00-\x1F\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/;
      const alphanumeric = /[^\x00-\x1F\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/;
      return token(seq(alpha, repeat(alphanumeric)));
    },

    this: ($) => "this",
    super: ($) => "super",
    true: ($) => "true",
    false: ($) => "false",
    null: ($) => "null",

    ////
    //// Expression components
    ////

    arguments: ($) =>
      prec(
        PREC.CALL,
        seq(
          "(",
          commaSep(optional(choice($._expression, $.spread_element))),
          ")"
        )
      ),

    decorator: ($) =>
      seq(
        "@",
        choice(
          $.identifier,
          alias($.decorator_member_expression, $.member_expression),
          alias($.decorator_call_expression, $.call_expression)
        )
      ),

    decorator_member_expression: ($) =>
      prec(
        PREC.MEMBER,
        seq(
          field(
            "object",
            choice(
              $.identifier,
              alias($.decorator_member_expression, $.member_expression)
            )
          ),
          ".",
          field("property", alias($.identifier, $.property_identifier))
        )
      ),

    decorator_call_expression: ($) =>
      prec(
        PREC.CALL,
        seq(
          field(
            "function",
            choice(
              $.identifier,
              alias($.decorator_member_expression, $.member_expression)
            )
          ),
          field("arguments", $.arguments)
        )
      ),

    class_body: ($) =>
      seq(
        "{",
        repeat(
          choice(
            // $.decorator,
            seq($.method_definition, optional($._semicolon)),
            seq(
              choice(
                // $.abstract_method_signature,
                // $.index_signature,
                // $.method_signature,
                $.public_field_definition
              ),
              choice($._semicolon, ",")
            )
          )
        ),
        "}"
      ),

    abstract_method_signature: ($) =>
      seq(
        optional($.accessibility_modifier),
        "abstract",
        optional(choice("get", "set", "*")),
        field("name", $._property_name),
        optional("?"),
        $._call_signature
      ),

    public_field_definition: ($, previous) =>
      seq(
        optional($.accessibility_modifier),
        choice(
          seq(optional("static"), optional($.readonly)),
          seq(optional("abstract"), optional($.readonly)),
          seq(optional($.readonly), optional("abstract"))
        ),
        field("name", $._property_name),
        optional(choice("?", "!")),
        // field("type", optional($.type_annotation)),
        optional($._initializer)
      ),

    formal_parameters: ($) =>
      seq(
        "(",
        optional(seq(commaSep1($._formal_parameter), optional(","))),
        ")"
      ),

    _formal_parameter: ($) =>
      seq(
        repeat(field("decorator", $.decorator)),
        choice($.required_parameter, $.rest_parameter, $.optional_parameter)
      ),

    required_parameter: ($) =>
      seq(
        $._parameter_name,
        // optional($.type_annotation),
        optional($._initializer)
      ),

    _parameter_name: ($) =>
      seq(
        optional($.accessibility_modifier),
        optional($.readonly),
        choice(
          $.identifier,
          alias($._reserved_identifier, $.identifier),
          // $._destructuring_pattern,
          $.this
        )
      ),

    //// _formal_parameter: ($) =>
    ////   choice(
    ////     $.identifier,
    ////     alias($._reserved_identifier, $.identifier),
    ////     $._destructuring_pattern,
    ////     $.assignment_pattern,
    ////     $.rest_parameter
    ////   ),

    rest_parameter: ($) =>
      seq("...", choice($.identifier, $._destructuring_pattern)),

    method_definition: ($) =>
      prec.left(
        seq(
          optional($.accessibility_modifier),
          optional("static"),
          optional($.readonly),
          optional("async"),
          optional(choice("get", "set", "*")),
          field("name", $._property_name),
          optional("?"),
          $._call_signature,
          field("body", $.statement_block)
        )
      ),

    pair: ($) =>
      seq(field("key", $._property_name), ":", field("value", $._expression)),

    _property_name: ($) =>
      choice(
        alias(
          choice($.identifier, $._reserved_identifier),
          $.property_identifier
        ),
        $.string,
        $.number,
        $.computed_property_name
      ),

    computed_property_name: ($) => seq("[", $._expression, "]"),

    _reserved_identifier: ($) => choice("get", "set", "async", "static"),

    _semicolon: ($) => choice($._automatic_semicolon, ";"),

    ////  ----------- all things type ------------------
    //type_assertion: ($) =>
    //  prec(PREC.TYPE_ASSERTION, seq($.type_arguments, $._expression)),
    //type_arguments: ($) => seq("<", commaSep1($._type), optional(","), ">"),

    _type: ($) =>
      choice(
        // $._primary_type,
        $.union_type
        // $.intersection_type,
        // $.function_type,
        // $.constructor_type
      ),

    //type_annotation: ($) => seq(":", $._type),
    //// primary type
    //_primary_type: ($) =>
    //  choice(
    //    $.parenthesized_type,
    //    $.predefined_type,
    //    $._type_identifier,
    //    $.nested_type_identifier,
    //    $.generic_type,
    //    $.object_type,
    //    $.array_type,
    //    $.tuple_type,
    //    $.flow_maybe_type,
    //    $.type_query,
    //    $.index_type_query,
    //    $.existential_type,
    //    $.literal_type,
    //    $.lookup_type,
    //    $.conditional_type
    //  ),

    //parenthesized_type: ($) => seq("(", $._type, ")"),
    //predefined_type: ($) =>
    //  choice("number", "boolean", "string", "symbol", "void"),

    _type_identifier: ($) => alias($.identifier, $.type_identifier),

    nested_type_identifier: ($) =>
      prec(
        PREC.MEMBER,
        seq(
          field("module", choice($.identifier, $.nested_type_identifier)),
          ".",
          field("name", $._type_identifier)
        )
      ),

    generic_type: ($) =>
      seq(
        choice($._type_identifier, $.nested_type_identifier),
        $.type_arguments
      ),

    object_type: ($) =>
      seq(
        choice("{", "{|"),
        optional(
          seq(
            optional(choice(",", ";")),
            sepBy1(
              choice(",", $._semicolon),
              choice(
                $.export_statement,
                $.property_signature,
                $.call_signature,
                $.construct_signature,
                $.index_signature,
                $.method_signature
              )
            ),
            optional(choice(",", $._semicolon))
          )
        ),
        choice("}", "|}")
      ),

    call_signature: ($) => $._call_signature,
    _call_signature: ($) =>
      seq(
        field("type_parameters", optional($.type_parameters)),
        field("parameters", $.formal_parameters)
        // field(
        //   "return_type",
        //   optional(
        //     choice($.type_annotation, $.asserts, $.type_predicate_annotation)
        //   )
        // )
      ),

    //index_signature: ($) =>
    //  seq(
    //    optional(seq(field("sign", optional("-")), $.readonly)),
    //    "[",
    //    choice(
    //      seq(
    //        choice($.identifier, alias($._reserved_identifier, $.identifier)),
    //        ":",
    //        $._type
    //      ),
    //      $.mapped_type_clause
    //    ),
    //    "]",
    //    choice(
    //      $.type_annotation,
    //      $.omitting_type_annotation,
    //      $.opting_type_annotation
    //    )
    //  ),

    //omitting_type_annotation: ($) => seq("-?:", $._type),
    //opting_type_annotation: ($) => seq("?:", $._type),

    //mapped_type_clause: ($) => prec(1, seq($._type_identifier, "in", $._type)),

    //construct_signature: ($) =>
    //  seq(
    //    "new",
    //    optional($.type_parameters),
    //    $.formal_parameters,
    //    optional($.type_annotation)
    //  ),

    //method_signature: ($) =>
    //  seq(
    //    optional($.accessibility_modifier),
    //    optional("static"),
    //    optional($.readonly),
    //    optional("async"),
    //    optional(choice("get", "set", "*")),
    //    field("name", $._property_name),
    //    optional("?"),
    //    $._call_signature
    //  ),

    //type_predicate: ($) => seq(choice($.identifier, $.this), "is", $._type),
    //type_predicate_annotation: ($) => seq(seq(":", $.type_predicate)),
    //asserts: ($) => seq(":", "asserts", choice($.identifier, $.type_predicate)),

    //property_signature: ($) =>
    //  seq(
    //    optional($.accessibility_modifier),
    //    optional("static"),
    //    optional($.readonly),
    //    field("name", $._property_name),
    //    optional("?"),
    //    field("type", optional($.type_annotation))
    //  ),
    accessibility_modifier: ($) =>
      prec.left(PREC.ACCESSIBILITY, choice("public", "private", "protected")),

    readonly: ($) => "readonly",

    //array_type: ($) =>
    //  prec(
    //    PREC.ARRAY_TYPE,
    //    choice(
    //      seq($.readonly, $._primary_type, "[", "]"),
    //      prec(PREC.ARRAY_TYPE + 1, seq($._primary_type, "[", "]"))
    //    )
    //  ),

    //tuple_type: ($) =>
    //  choice(
    //    prec.left(PREC.ARRAY_TYPE + 1, $._tuple_type_body),
    //    prec.left(PREC.ARRAY_TYPE + 2, seq($.readonly, $._tuple_type_body))
    //  ),

    //_tuple_type_body: ($) =>
    //  seq("[", optional(commaSep1($._tuple_type_member)), "]"),

    //_tuple_type_member: ($) =>
    //  choice($._tuple_type_identifier, $.labeled_tuple_type_member),

    //_tuple_type_identifier: ($) =>
    //  choice($.identifier, $.optional_identifier, $.rest_identifier),

    //rest_identifier: ($) => seq("...", $.identifier),
    //optional_identifier: ($) => seq($.identifier, "?"),

    //flow_maybe_type: ($) =>
    //  prec.right(PREC.FLOW_MAYBE_TYPE, seq("?", $._primary_type)),

    //type_query: ($) =>
    //  prec(
    //    PREC.TYPEOF,
    //    seq("typeof", choice($.identifier, $.nested_identifier, $.generic_type))
    //  ),

    //index_type_query: ($) =>
    //  seq(
    //    "keyof",
    //    prec.left(
    //      PREC.DECLARATION,
    //      choice(
    //        $.generic_type,
    //        $._type_identifier,
    //        $.nested_type_identifier,
    //        $.type_query
    //      )
    //    )
    //  ),

    //existential_type: ($) => "*",
    //literal_type: ($) =>
    //  choice(
    //    alias($._number, $.unary_expression),
    //    $.number,
    //    $.string,
    //    $.true,
    //    $.false
    //  ),

    //lookup_type: ($) =>
    //  prec(PREC.ARRAY_TYPE, seq($._primary_type, "[", $._type, "]")),

    //conditional_type: ($) =>
    //  prec.left(
    //    PREC.CONDITIONAL_TYPE,
    //    seq(
    //      field("left", $._type),
    //      "extends",
    //      field("right", $._type),
    //      "?",
    //      field("consequence", $._type),
    //      ":",
    //      field("alternative", $._type)
    //    )
    //  ),

    //// other complex types
    //union_type: ($) =>
    //  prec.left(PREC.UNION, seq(optional($._type), "|", $._type)),

    //intersection_type: ($) =>
    //  prec.left(PREC.INTERSECTION, seq(optional($._type), "&", $._type)),

    //function_type: ($) =>
    //  prec.left(
    //    PREC.FUNCTION_TYPE,
    //    seq(optional($.type_parameters), $.formal_parameters, "=>", $._type)
    //  ),
    // type_parameters: ($) =>
    //   seq("<", commaSep1($.type_parameter), optional(","), ">"),

    // type_parameter: ($) =>
    //   seq($._type_identifier, optional($.constraint), optional($.default_type)),
    // constraint: ($) => seq(choice("extends", ":"), $._type),
    // default_type: ($) => seq("=", $._type),

    //constructor_type: ($) =>
    //  prec.left(
    //    PREC.CONSTRUCTOR_TYPE,
    //    seq(
    //      "new",
    //      optional($.type_parameters),
    //      $.formal_parameters,
    //      "=>",
    //      $._type
    //    )
    //  ),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
