######################  SIMPLE PASCAL INTERPRETER ############################

# This is a basic interpreter that does Multiplication, division for all integers. This program is just
#  for understanding how lexer and interpreter works on a high level view.
# This program uses lexer to parse the text and spit out the tokens which in turn are used by interpreter which knows
# the grammar and makes sense out of the tokens that it receives.and interpreter
# Part 5 added addition and substraction and operator precedence and association.

# The modules of the program are lexer, parser, interpreter.
# Lexer converts the text in to tokens and passes it on to the parser.
# Parser up on receiving these tokens builds the abstract syntax tree(AST).
# This Abstract Syntax Tree is different from the parse tree. Parser feeds the
# AST to the interpreter which makes sense out of it and does some calculation.


# Token is something that can't be broken apart into pieces.
INTEGER, MUL, DIV, EOF, PLUS, MINUS, LPAREN, RPAREN = ('INTEGER', 'MUL', 'DIV',
                                    'EOF', 'PLUS', 'MINUS', 'LPAREN', 'RPAREN')


class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return 'Token({type},{value})'.format(type=self.type, value=self.value)

    def __repr__(self):
        return self.__str__()


# Lexer is the class to that retrieves the sentence and parses the sentence and
# sends the tokens with respective value to the interpreter.
class Lexer:
    # It gets the tokens from the sentense by parsing the sentenceself.
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception("Invalid Character")
    # This method advances the pointer by 1 and stores the current character in
    # to the current character of Lexer.

    def advance(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]
    # This method skips all the white spaces in the given text.

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # extracts the integer represented by the current token.
    def integer(self):
        result = ''
        while self.current_char is not None and self.text[self.pos].isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def get_nextToken(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char.isdigit():
                return Token(INTEGER, self.integer())
            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')
            if self.current_char == '/':
                self.advance()
                return Token(DIV, '/')
            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')
            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')
            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')
            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')
            # If current char is none of the above it means the current
            # character is not in the grammer defined.
            # Therefore, it makes sense to throw an error.
            self.error()
        return Token(EOF, None)

###
# Parser code.
####


# Data structure for representing the abstract syntax tree.
class AST:
    pass

# This datastructure holds binary operators, like addition, substraction,
# multiplication.
class BinOp(AST):
        def __init__(self, left, op, right):
            self.left = left
            self.right = right
            # This holds the token for binary operator.
            self.token = self.op = op


# This data structure holds the integer number for AST.
class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

# The above is the lexer code which helps in retrieving the tokens from
# the sentence.
# The interpreter module communicates with the lexer module to retrieve the
# next token from the sentence self.
# This class represents the parser, which takes in tokens from the lexer. This
# Abstract syntax tree is fed to the interpreter which analyzes the tree to get
# the proper meaning out of it.


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = lexer.get_nextToken()

    def error(self):
        raise Exception('Invalid Syntax')

    # compare the current token with the passed token if it matches then
    # advance the current token.
    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_nextToken()
        else:
            self.error()

    # Defining the term which is part of an expression.
    def term(self):
        node = self.factor()
        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if(token.type == MUL):
                self.eat(MUL)
            if(token.type == DIV):
                self.eat(DIV)
            node = BinOp(left=node, op=token, right=self.factor())
        return node

    # This method defines a factor which is a non terminating token.
    def factor(self):
        token = self.current_token
        if token.type == INTEGER:
            self.eat(INTEGER)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node

    # This method defines an expression which is a non terminating token.
    # Grammar.
    # Order
    # expr = term ((PLUS/MINUS)term)*
    # fact = Integer.
    def expr(self):
        node = self.term()
        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if(token.type == PLUS):
                self.eat(PLUS)
            if(token.type == MINUS):
                self.eat(MINUS)
            node = BinOp(left=node, op=token, right=self.term())
        return node

# parse method for the parser that parses the text in to AST.
    def parse(self):
        return self.expr()


###
# Interpreter: The code for the interpreter.
# input : AST
# output : value of the syntax tree.
###
class NodeVisitor:
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


# Interpreter
class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

# method for visiting the binary operator node.
    def visit_BinOp(self, node):
        # The commented code is for spitting out the preorder and postorder
        # notation of the expression.
        # left_val = self.visit(node.left)
        # right_val = self.visit(node.right)
        # return "{} {}{}".format(node.op.type, left_val, right_val)
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        if node.op.type == DIV:
            return self.visit(node.left)/self.visit(node.right)

# method for visiting the integer numbers.
    def visit_Num(self, node):
        return node.value

# method for interpreter.
    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


# Driver of the program.
def main():
    while True:
        try:
            text = input('spi>')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print(result)
    # Boiler plate code.


if __name__ == '__main__':
        main()
