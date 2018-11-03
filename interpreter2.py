######################  SIMPLE PASCAL INTERPRETER ############################

# This is a basic interpreter that does Multiplication, division for all integers. This program is just
#  for understanding how lexer and interpreter works on a high level view.
# This program uses lexer to parse the text and spit out the tokens which
# in turn are used by interpreter which knows the grammar
# and makes sense out of the tokens that it receives.and interpreter
# Part 5 added addition and substraction and operator precedence and association.

# The modules of the program are lexer, parser, interpreter.
# Lexer converts the text in to tokens and passes it on to the parser.
# Parser up on receiving these tokens builds the abstract syntax tree(AST).
# This Abstract Syntax Tree is different from the parse tree. Parser feeds the
# AST to the interpreter which makes sense out of it and does some calculation.


# Token is something that can't be broken apart into pieces.
INTEGER, MUL, DIV, EOF, PLUS, MINUS, LPAREN, RPAREN, BEGIN, END, DOT, ID, ASSIGN, SEMI, PROGRAM, VAR, COLON, COMMA, REAL, INTEGER_CONST, REAL_CONST, INTEGER_DIV, FLOAT_DIV = ('INTEGER', 'MUL', 'DIV',
                                    'EOF', 'PLUS', 'MINUS', 'LPAREN', 'RPAREN', 'BEGIN', 'END', 'DOT', 'ID', 'ASSIGN', 'SEMI','PROGRAM', 'VAR', 'COLON', 'COMMA', 'REAL', 'INTEGR_CONST', 'REAL_CONST','INTEGER_DIV', 'FLOAT_DIV')


# This is the datastructure for holding the various tokens.
class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return 'Token({type},{value})'.format(type=self.type, value=self.value)

    def __repr__(self):
        return self.__str__()


# Set of reserverd keywords for identifiers. These are the set of keywords used
# by the program. These are predefined so that any program doesn't use these
# reserved keywords as their variable namesself.abs
# The trick is to identify which keywords are used in your program and put them
# in the reserverd keywords variable.
RESERVED_KEYWORDS = {
    'PROGRAM': Token('PROGRAM','PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'DIV': Token('INTEGER_DIV', 'DIV'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'REAL': Token('REAL', 'REAL'),
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END'),
    #'PRINT': Token('PRINT', 'PRINT')
    }


# Lexer is the class to that retrieves the sentence and parses the sentence and
# sends the tokens with respective value to the interpreter.
class Lexer:

    # It gets the tokens from the sentense by parsing the sentenceself.
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    # Skip comments method to skip the comments from the pascal program. comments
    # are enclosed inside the curly brackets.
    def skip_comment(self):
        while(self.current_char != '}'):
            self.advance()
        self.advance()

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

    #This method returns the number(may be real or integer) from the string.
    # getNexttoken method from the lexer calls this method to retrieve the number.
    def number(self):
        result = ''
        while self.current_char is not None and self.text[self.pos].isdigit():
            result += self.current_char
            self.advance()
        if(self.current_char == '.'):
            result += self.current_char
            self.advance()
            while(self.current_char is not None and self.current_char.isdigit()):
                result += self.current_char
                self.advance()
            return Token(REAL_CONST, float(result))
        else:
            return Token(INTEGER_CONST, int(result))

# Method for peeking the next character used for assign statements.
    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

# Method for returning the token relating to the Identifier.abs
    def _id(self):
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()
        # Retrieving the token from reserver keywords or creating a new
        # identifier incase it is not in the reserved keywords.
        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

# Returns the next token
    def get_nextToken(self):
        while self.current_char is not None:
            # returning the identifier[like begin and end] of the program.
            if self.current_char.isalpha():
                return self._id()
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            # returning the number token of the program.
            if self.current_char.isdigit():
                return self.number()
            # returning the binary operators of the program.
            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')
            if self.current_char == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')
            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')
            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')
            # returning the left and right paranthesis of the program.
            # This is for the operator precedence.
            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')
            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')
            # returning the assignment token for the program.
            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')
            if(self.current_char == ':'):
                self.advance()
                return Token(COLON, ':')
            # returning the semicolon token which is the seperator
            # the each statements.
            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')
            # returning the dot token which marks the end of the program.
            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.')
            # returning the comment part of the program which is a comment token.
            # This method doesn't return any token because the parser doesn't
            # require any token for the comment.Moreover, when the parser asks
            # for the next token the lexer should spit out any token other than
            # comment token(Because parser should build the abstract syntax tree.)
            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue
            # returning the comma token of the program.
            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',')
            # returning float division operator token.
            if self.current_char == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')
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


# This datastructure for holding unary operator, like +, -.
class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = op
        self.expr = expr


# This data structure holds the integer number for AST.
class Num(AST):
    def __init__(self, token):
        self.token = self.op = token
        self.value = token.value


# This datastructure holds the comound statement. This compound statement has
# list of children which are statement list.
class Compound(AST):
    def __init__(self):
        self.children = []


# This datastructure holds the assign statement.
class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


# This datastructure holds the variable.
class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


# This datastructure for holding the empty variable.
class NoOp(AST):
    pass


# This is the top node that contains the blocks.
# program : PROGRAM VARIABLE SEMI block DOT.
class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block

# This is the block node of the program.
# BLOCK: declarations compoundstatement
class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


# Node representing the varDecl token.
class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


# Node representing the type node.
class Type(AST):
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
        print(self.current_token.type, token_type)
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_nextToken()
        else:
            self.error()

    # Defining the term which is part of an expression.
    def term(self):
        node = self.factor()
        while self.current_token.type in (MUL, INTEGER_DIV, FLOAT_DIV):
            token = self.current_token
            if(token.type == MUL):
                self.eat(MUL)
            if(token.type == INTEGER_DIV):
                self.eat(INTEGER_DIV)
            if(token.type == FLOAT_DIV):
                self.eat(FLOAT_DIV)
            # Creating a node of the term.
            node = BinOp(left=node, op=token, right=self.factor())
        return node

    # This method defines a factor which is a non terminating token.
    def factor(self):
        token = self.current_token
        print("Printing the testing token type ", token.type)
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        if token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
            return node
        # Adding the integer constant datatype to the parser.
        if token.type == INTEGER_CONST:
            self.eat(INTEGER_CONST)
            return Num(token)
        # Adding the real constant data type to the parser.
        if token.type == REAL_CONST:
            self.eat(REAL_CONST)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
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

    # This method returns the compound statement node.
    def compound_statement(self):
        '''compound_statement : BEGIN statement_list END'''
        self.eat(BEGIN)
        nodes = self.statement_list()
        print("compound statement type", self.current_token.type)
        self.eat(END)
        root = Compound()
        for node in nodes:
            root.children.append(node)
        return root

    # This method returns the list of statements that is in the form of
    # list of nodes.
    def statement_list(self):
        node = self.statement()
        results = [node]
        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())
        # **didn't check if current token type is ID**
        return results

    # This method returns the statement
    '''Statement : compound_statement| assignement_statement | empty'''

    def assignement_statement(self):
        #if(self.current_token.value == 'PRINT'):
        #    self.eat
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    # This method returns the statement node which is part of the grammer.
    def statement(self):
        if self.current_token.type == BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == ID:
            node = self.assignement_statement()
        else:
            node = self.empty()
        return node

    # This method returns the variable/identifier related to a value.
    def variable(self):
        node = Var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        '''An empty production.'''
        return NoOp()

    # This returns the main node which is the entry point in to the
    # abstarct syntax tree.
    def program(self):
        self.eat(PROGRAM)
        var_node = self.variable()
        self.eat(SEMI)
        prog_name = var_node.value
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(DOT)
        return program_node

    # parse method for the parser that parses the text in to AST.
    def parse(self):
        return self.program()

    # parse methof for the block.
    def block(self):
        declaration_nodes = self.declarations()
        compound_statement = self.compound_statement()
        node = Block(declaration_nodes, compound_statement)
        return node

    # This method contains the bunch of variable declarations which constitute
    # a big declaration of the program.
    def declarations(self):
        '''declarations: VAR(variable_declaraion SEMI)+ | EMPTY'''
        declarations = []
        if self.current_token.type == VAR:
            self.eat(VAR)
            while self.current_token.type == ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(SEMI)
        return declarations

    # This method contains the grammar rules for variable declaration.
    def variable_declaration(self):
        var_nodes = [Var(self.current_token)]
        self.eat(ID)
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)
        self.eat(COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    # This method return the type specification of the variable.
    def type_spec(self):
        token = self.current_token
        if token.type == INTEGER:
            self.eat(INTEGER)
        if token.type == REAL:
            self.eat(REAL)
        node = Type(token)
        return node
# Interpreter: The code for the interpreter.
# input : AST
# output : value of the syntax tree.
###
class NodeVisitor:
    def visit(self, node):
        print(" Type of the visited node: ", type(node))
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


# Interpreter
class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

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

        # Added INTEGER_DIV and FLOAT_DIV for integer and float division.
        if node.op.type == INTEGER_DIV:
            return self.visit(node.left)//self.visit(node.right)
        if node.op.type == FLOAT_DIV:
            return self.visit(node.left)/self.visit(node.right)

# method for visiting the integer numbers.
    def visit_Num(self, node):
        return node.value

# method for visiting unary operator.
    def visit_UnaryOp(self, node):
        op = node.token.type
        if op == PLUS:
            return +self.visit(node.expr)
        if op == MINUS:
            return -self.visit(node.expr)

# method for visiting compound statements.
    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

# method for visiting the empty operator.
    def visit_NoOp(self, node):
        pass

# This method for the assignment operator to work.
# Theglobal symbol table stores the all variable names and their values.
# When a request for a variable comes up the value of the variable is looked up
# in the symbol table. If the key is not found in the symbol table error is
# thrown. Essentially this symbol table is implemented by using the dictionary
# datastructure of Python.
    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is not None:
            return val
        else:
            raise NameError(repr(var_name))

    def visit_Program(self, node):
        self.visit(node.block)

    # Interpreter visits the block node of the AST.
    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass
    def visit_Type(self, node):
        pass
# method for interpreter.
    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


# Driver of the program.
def main():
    #while True:
    text = ""
    while True:
        fileName = input("sand>")
        text = ""
        # try:
        file_handler = open(fileName)
        for line in file_handler:
            text += line
        #except EOFError:
        #    break
        #if not text:
            #break
        print("*******************")
        print('Text from the file :')
        print(text)
        print("*******************")
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print("*******************")
        print("Global scope variable", interpreter.GLOBAL_SCOPE)
    # Boiler plate code.


if __name__ == '__main__':
        main()
