#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_LEN(xs) ((xs[0]) / sizeof(xs))

typedef enum {
  T_OPEN_PAR,
  T_CLOS_PAR,
  T_SIN,
  T_COS,
  T_TAN,
  T_VAR,
  T_SUM,
  T_MUL,
  T_DIV,
  T_SUB,
  T_INT,
  T_POINT,
  T_FLOAT,
  T_POWER,
  T_UNKNOWN,
  T_END,

  T_COUNT,
} Token_Kind;

static_assert(T_COUNT == 16, "Kind has been modified");
const char *token_kind_names[T_COUNT] = {[T_OPEN_PAR] = "(",
                                         [T_CLOS_PAR] = ")",
                                         [T_SUM] = "+",
                                         [T_MUL] = "*",
                                         [T_DIV] = "/",
                                         [T_SUB] = "-",
                                         [T_UNKNOWN] = "t_unknown",
                                         [T_INT] = "t_int",
                                         [T_END] = "t_end",
                                         [T_POWER] = "^",
                                         [T_COS] = "cos",
                                         [T_SIN] = "sin",
                                         [T_VAR] = "t_var",
                                         [T_TAN] = "tan",
                                         [T_FLOAT] = "t_float",
                                         [T_POINT] = "."};

typedef struct {
  float value;
  bool has_error;
  const char *error_message;
} EvalResult;

typedef struct {
  Token_Kind kind;
  union {
    int int_value;
    float float_value;
    char *var_value;
  } value;
  char *start;
  size_t length;
} Token;

typedef struct Node {
  Token_Kind kind;
  union {
    char *var;
    int int_value;
    float float_value;
  } value;
  struct Node *lhs;
  struct Node *rhs;
} Node;

Node *parse_expression(char **formula);
Node *parse_factor(char **formula);
Node *parse_term(char **formula);
EvalResult eval_ast(Node *ast);
void print_ast(Node *ast, int level);

char *shift(char ***argv, int *argc) {
  if (*argc <= 0) {
    return NULL;
  }
  char *res = **argv;
  (*argv)++;
  (*argc)--;

  return res;
}

bool is_float(const char *str) {
  while (isdigit(*str))
    str++;

  if (*str == '.') {
    str++;
    return isdigit(*str);
  }
  return false;
}

Token peek_token(char *formula) {
  Token token = {0};
  char *chr = formula;

  while (isspace(*chr))
    chr++;

  token.start = chr;

  if (*chr == '\0') {
    token.kind = T_END;
    return token;
  }

  if (*chr == '(') {
    token.kind = T_OPEN_PAR;
    token.length = 1;
    return token;
  }

  if (*chr == ')') {
    token.kind = T_CLOS_PAR;
    token.length = 1;
    return token;
  }

  if (*chr == '+') {
    token.kind = T_SUM;
    token.length = 1;
    return token;
  }

  if (*chr == '-') {
    token.kind = T_SUB;
    token.length = 1;
    return token;
  }

  if (*chr == '*') {
    token.kind = T_MUL;
    token.length = 1;
    return token;
  }

  if (*chr == '/') {
    token.kind = T_DIV;
    token.length = 1;
    return token;
  }

  if (*chr == '^') {
    token.kind = T_POWER;
    token.length = 1;
    return token;
  }

  if (isdigit(*chr)) {
    char *endptr;
    if (is_float(chr)) {
      token.kind = T_FLOAT;
      token.value.float_value = strtod(chr, &endptr);
    } else {
      token.kind = T_INT;
      token.value.int_value = strtol(chr, &endptr, 10);
    }
    token.length = endptr - chr;
    return token;
  }

  if (isalpha(*chr)) {
    const char *start = chr;
    while (isalnum(*chr))
      chr++;
    size_t len = chr - start;

    if (len == 3) {
      if (strncmp(start, "sin", 3) == 0) {
        token.kind = T_SIN;
        token.length = 3;
        return token;
      }
      if (strncmp(start, "cos", 3) == 0) {
        token.kind = T_COS;
        token.length = 3;
        return token;
      }
      if (strncmp(start, "tan", 3) == 0) {
        token.kind = T_TAN;
        token.length = 3;
        return token;
      }
    }

    token.kind = T_VAR;
    token.value.var_value = strndup(start, len);
    token.length = len;
    return token;
  }

  token.kind = T_UNKNOWN;
  token.length = 1;
  return token;
}

Token consume_token(char **formula) {
  Token token = peek_token(*formula);
  *formula += token.length;
  return token;
}

Node *parse_factor(char **formula) {
  Token token = peek_token(*formula);

  if (token.kind == T_INT || token.kind == T_FLOAT) {
    *formula += token.length;
    Node *num_node = malloc(sizeof(Node));
    num_node->kind = token.kind;
    if (token.kind == T_FLOAT) {
      num_node->value.float_value = token.value.float_value;
    } else {
      num_node->value.int_value = token.value.int_value;
    }
    return num_node;
  }

  if (token.kind == T_VAR) {
    *formula += token.length;
    Node *var_node = malloc(sizeof(Node));
    var_node->kind = T_VAR;
    var_node->value.var = token.value.var_value;
    return var_node;
  }

  if (token.kind == T_OPEN_PAR) {
    *formula += token.length;
    Node *expr_node = parse_expression(formula);
    token = consume_token(formula);
    if (token.kind != T_CLOS_PAR) {
      fprintf(stderr, "ERROR: Expected closing parenthesis\n");
      exit(1);
    }
    return expr_node;
  }

  if (token.kind == T_SIN || token.kind == T_COS || token.kind == T_TAN) {
    *formula += token.length;
    Node *func_node = malloc(sizeof(Node));
    func_node->kind = token.kind;
    func_node->lhs = parse_factor(formula);
    func_node->rhs = NULL;
    return func_node;
  }

  fprintf(stderr, "ERROR: Unexpected token: %s\n",
          token_kind_names[token.kind]);
  exit(1);
}

Node *parse_term(char **formula) {
  Node *lhs = parse_factor(formula);

  while (true) {
    Token token = peek_token(*formula);
    if (token.kind != T_MUL && token.kind != T_DIV) {
      break;
    }

    *formula += token.length;
    Node *op_node = malloc(sizeof(Node));
    op_node->kind = token.kind;
    op_node->lhs = lhs;
    op_node->rhs = parse_factor(formula);
    lhs = op_node;
  }

  return lhs;
}

Node *parse_expression(char **formula) {
  Node *lhs = parse_term(formula);

  while (true) {
    Token token = peek_token(*formula);
    if (token.kind != T_SUM && token.kind != T_SUB) {
      break;
    }

    *formula += token.length;
    Node *op_node = malloc(sizeof(Node));
    op_node->kind = token.kind;
    op_node->lhs = lhs;
    op_node->rhs = parse_term(formula);
    lhs = op_node;
  }

  return lhs;
}

void print_ast(Node *ast, int level) {
  if (ast == NULL) {
    return;
  }
  for (int i = 0; i < level; i++) {
    printf(i == level - 1 ? "|-> " : " ");
  }
  if (ast->kind == T_INT) {
    printf("%d\n", ast->value.int_value);
  } else if (ast->kind == T_VAR) {
    printf("%s\n", ast->value.var);
  } else if (ast->kind == T_FLOAT) {
    printf("%g\n", ast->value.float_value);
  } else {
    printf("%s\n", token_kind_names[ast->kind]);
  }

  print_ast(ast->lhs, level + 1);
  print_ast(ast->rhs, level + 1);
}

EvalResult eval_ast(Node *node) {
  if (node == NULL) {
    return (EvalResult){0.0f, true, "Null node encountered"};
  }

  EvalResult left, right;

  switch (node->kind) {
  case T_SUM:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    right = eval_ast(node->rhs);
    if (right.has_error)
      return right;
    return (EvalResult){left.value + right.value, false, NULL};

  case T_SUB:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    right = eval_ast(node->rhs);
    if (right.has_error)
      return right;
    return (EvalResult){left.value - right.value, false, NULL};

  case T_MUL:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    right = eval_ast(node->rhs);
    if (right.has_error)
      return right;
    return (EvalResult){left.value * right.value, false, NULL};

  case T_DIV:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    right = eval_ast(node->rhs);
    if (right.has_error)
      return right;
    if (right.value == 0.0f) {
      return (EvalResult){0.0f, true, "Division by zero"};
    }
    return (EvalResult){left.value / right.value, false, NULL};

  case T_INT:
    return (EvalResult){(float)node->value.int_value, false, NULL};

  case T_FLOAT:
    return (EvalResult){node->value.float_value, false, NULL};

  case T_SIN:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    return (EvalResult){sinf(left.value), false, NULL};

  case T_COS:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    return (EvalResult){cosf(left.value), false, NULL};

  case T_TAN:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    return (EvalResult){tanf(left.value), false, NULL};

  case T_POWER:
    left = eval_ast(node->lhs);
    if (left.has_error)
      return left;
    right = eval_ast(node->rhs);
    if (right.has_error)
      return right;
    return (EvalResult){powf(left.value, right.value), false, NULL};

  default:
    return (EvalResult){0.0f, true, "Unknown operator"};
  }
}

void parse_formula(char *formula) {
  Node *ast = parse_expression(&formula);
  print_ast(ast, 0);
  EvalResult result = eval_ast(ast);
  if (result.has_error) {
    printf("Error: %s\n", result.error_message);
  } else {
    printf("Result: %f\n", result.value);
  }
}

int main(int argc, char **argv) {

  char *program_name;
  if (!(program_name = shift(&argv, &argc))) {
    fprintf(stderr, "ERROR: Trying to shift an empty array\n");
    return 1;
  }
  if (argc < 1) {
    printf("ERROR: Usage %s <formula>\n", program_name);
    printf("- Example: 1+1\n");
    return 1;
  }

  char *formula = shift(&argv, &argc);

  parse_formula(formula);

  return 0;
}
