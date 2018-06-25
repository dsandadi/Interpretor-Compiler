# This is a basic interpreter that does Multiplication, division for all integers. This program is just
#  for understanding how lexer and interpreter works on a high level view.
# This program uses lexer to parse the text and spit out the tokens which in turn are used by interpreter which knows
# the grammar and makes sense out of the tokens that it receives.and interpreter
# Part 5 added addition and substraction and operator precedence and association.

# Token is something that can't be broken apart into pieces.
INTEGER, MUL, DIV, EOF, PLUS, MINUS = ('INTEGER', 'MUL', 'DIV', 'EOF', 'PLUS', 'MINUS')


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

            # If current char is none of the above it means the current
            # character is not in the grammer defined.
            # Therefore, it makes sense to throw an error.
            self.error()
        return Token(EOF, None)
# The above completes the lexer code which helps in retrieving the tokens from
# the sentence.
# The interpreter module communicates with the lexer module to retrieve the
# next token from the sentence self.


class Interpreter:
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
        result = self.factor()
        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if(token.type == MUL):
                self.eat(MUL)
                result *= self.factor()
            if(token.type == DIV):
                self.eat(DIV)
                result /= self.factor()
        return result

    # This method defines a factor which is a non terminating token.
    def factor(self):
        token = self.current_token
        self.eat(INTEGER)
        return token.value

    # This method defines an expression which is a non terminatin token.
    # Grammar.
    # Order
    # expr = term ((PLUS/MINUS)term)*
    # fact = Integer.
    def expr(self):
        result = self.term()
        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if(token.type == PLUS):
                self.eat(PLUS)
                result += self.term()
            if(token.type == MINUS):
                self.eat(MINUS)
                result -= self.term()
        return result


# Driver of the program.
def main():
    while True:
        try:
            text = input('calc>')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr()
        print(result)
    # Boiler plate code.


if __name__ == '__main__':
        main()
