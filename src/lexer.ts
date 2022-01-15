export enum LexerFlags {
    InString,
    Backslash,
    InComment
}

export enum TokenType {
    Null,
    EOF,
    String,
    Number,
    Identifier,
    PropertyAccessor,
    Operator,
    TypeSpecifier,
    OpenParenthesis,
    CloseParenthesis,
    OpenBlock,
    CloseBlock,
    Delimeter
}

export class LexerToken {
    constructor(
        public type: TokenType,
        public value: string,
        public pos: number,
        public end: number
    ) {}
}

export class LexerError extends Error {
    constructor(
        public readonly charIdx: number,
        message: string
    ) {
        super(message);
    }
}

const operatorCombos = new Set([
    "==",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "!=",
    ">=",
    "<=",
    "++",
    "--",
    "&&",
    "||"
]);

export class CowsLexer {
    private _charIdx: number;
    private _chars: string[];

    private _flags: Set<LexerFlags>;
    private _tokens: LexerToken[];

    constructor(public readonly code: string, public readonly fileName = "(eval)") {
        this._charIdx = 0;
        this._chars = code.split("");

        this._flags = new Set;
        this._tokens = [];
    }

    advance() {
        if (this._charIdx >= this._chars.length) {
            this.addToken(TokenType.EOF, "EOF");
            return false;
        }

        const currentChar = this._chars[this._charIdx];

        if (this.isBackslashCharacter(currentChar)) {
            this.onBackspaceCharacter(currentChar);
        } else if (this.isWhitespaceCharacter(currentChar)) {
            this.onWhitespaceCharacter(currentChar);
        } else if (this.isStringCharacter(currentChar)) {
            this.onStringCharacter(currentChar);
        } else if (this.isPropertyAccessorCharacter(currentChar)) {
            this.onPropertyAccessorCharacter(currentChar);
        } else if (this.isNumberCharacter(currentChar)) {
            this.onNumberCharacter(currentChar);
        } else if (this.isIdentifierCharacter(currentChar)) {
            this.onIdentifierCharacter(currentChar);
        } else if (this.isOperatorCharacter(currentChar)) {
            this.onOperatorCharacter(currentChar);
        } else if (this.isTypeApplierCharacter(currentChar)) {
            this.onTypeApplierCharacter(currentChar);
        } else if (this.isOpenParenthesisCharacter(currentChar)) {
            this.onOpenParenthesisCharacter(currentChar);
        } else if (this.isCloseParenthesisCharacter(currentChar)) {
            this.onCloseParenthesisCharacter(currentChar);
        } else if (this.isOpenBlockCharacter(currentChar)) {
            this.onOpenBlockCharacter(currentChar);
        } else if (this.isCloseBlockCharacter(currentChar)) {
            this.onCloseBlockCharacter(currentChar);
        } else if (this.isOperatorCharacter(currentChar)) {
            this.onOperatorCharacter(currentChar);
        } else if (this.isDelimeterCharacter(currentChar)) {
            this.onDelimeterCharacter(currentChar);
        }

        this._charIdx++;

        return true;
    }

    digest() {
        for (let i = 0; i < this._tokens.length; i++) {
            if (this._tokens[i].type === TokenType.Null) {
                this._tokens.splice(i, 1);
                i--;
            }
        }

        return this._tokens;
    }

    getLastToken(): LexerToken|undefined {
        return this._tokens[this._tokens.length - 1];
    }

    createToken(type: TokenType, char: string) {
        this._tokens.push(new LexerToken(type, char, this._charIdx, this._charIdx));
    }

    addToken(type: TokenType, char: string) {
        const lastToken = this.getLastToken();

        if (!lastToken) {
            return this.createToken(type, char);
        }

        if (lastToken.type === TokenType.Null) {
            lastToken.value = char;
            lastToken.type = type;
            lastToken.pos = this._charIdx;
            lastToken.end = this._charIdx;
            return;
        }

        return this.createToken(type, char);
    }

    appendToLast(type: TokenType, char: string, overrideType = false) {
        const lastToken = this.getLastToken();

        if (!lastToken) {
            return this.createToken(type, char);
        }

        if (lastToken.type === TokenType.Null) {
            lastToken.value = char;
            lastToken.type = type;
            lastToken.pos = this._charIdx;
            lastToken.end = this._charIdx;
            return;
        }

        if (overrideType) {
            lastToken.value += char;
            lastToken.type = type;
            lastToken.end = this._charIdx;
            return;
        }

        if (lastToken.type === type) {
            lastToken.value += char;
            lastToken.end = this._charIdx;
            return;
        }

        return this.createToken(type, char);
    }

    addNull() {
        const lastToken = this.getLastToken();

        if (!lastToken || lastToken.type !== TokenType.Null) {
            this.createToken(TokenType.Null, "");
        }
    }

    isStringCharacter(char: string) {
        return char === "\"" || this._flags.has(LexerFlags.InString);
    }

    onStringCharacter(char: string) {
        if (char !== "\"" && this._flags.has(LexerFlags.InString)) {
            this.appendToLast(TokenType.String, char);
            return;
        }

        if (this._flags.has(LexerFlags.InString)) {
            if (this._flags.has(LexerFlags.Backslash)) {
                return this.appendToLast(TokenType.String, char);
            }

            this._flags.delete(LexerFlags.InString);
            this.addNull();
            return;
        }

        this._flags.add(LexerFlags.InString);
    }

    isWhitespaceCharacter(char: string) {
        return " \n".indexOf(char) !== -1;
    }

    onWhitespaceCharacter(char: string) {
        if (char === "\n" && this._flags.has(LexerFlags.InString)) {
            throw new LexerError(this._charIdx, "String not terminated before newline");
        }

        this.addNull();
    }

    isBackslashCharacter(char: string) {
        return char === "\\";
    }

    onBackspaceCharacter(char: string) {
        if (!this._flags.has(LexerFlags.InString)) {
            throw new LexerError(this._charIdx, "Unexpected token \\");
        }

        if (this._flags.has(LexerFlags.Backslash)) {
            this.appendToLast(TokenType.String, "\\", false);
            return;
        }

        this._flags.add(LexerFlags.Backslash);
    }

    isNumberCharacter(char: string) {
        const charCode = char.charCodeAt(0);
        return charCode >= 48 && charCode <= 57;
    }

    onNumberCharacter(char: string) {
        const lastToken = this.getLastToken();
        const lastLastToken = this._tokens[this._tokens.length - 2];

        if (!lastToken) {
            return this.appendToLast(TokenType.Number, char);
        }

        if (lastToken.type === TokenType.Identifier) {
            return this.appendToLast(TokenType.Identifier, char);
        }

        if (lastLastToken && lastToken.type === TokenType.PropertyAccessor && lastLastToken.type === TokenType.Number) {
            lastLastToken.value += "." + char;
            this._tokens.splice(this._tokens.length - 1, 1);
            return;
        }
        
        return this.appendToLast(TokenType.Number, char);
    }

    isIdentifierCharacter(char: string) {
        const charCode = char.charCodeAt(0);
        return (charCode >= 65 && charCode <= 90) // uppercase
            || (charCode >= 97 && charCode <= 122) // lowercase
            || charCode === 36 // dollar sign
            || charCode === 95; // under score
    }

    onIdentifierCharacter(char: string) {
        this.appendToLast(TokenType.Identifier, char);
    }

    isPropertyAccessorCharacter(char: string) {
        return char === ".";
    }

    onPropertyAccessorCharacter(char: string) {
        this.addToken(TokenType.PropertyAccessor, char);
    }

    isOperatorCharacter(char: string) {
        return "=+-*/%!><&|".indexOf(char) !== -1;
    }

    onOperatorCharacter(char: string) {
        for (let i = Math.min(this._tokens.length, 2); i > 0; i--) {
            let combo = "";
            let flag = false;
            for (let j = 1; j < i + 1; j++) {
                const token = this._tokens[this._tokens.length - j];
                if (token.type !== TokenType.Operator) {
                    flag = true;
                    break;
                }
                combo += token.value;
            }
            if (flag)
                continue;

            combo += char;
            if (operatorCombos.has(combo)) {
                const first = this._tokens[this._tokens.length - i];
                this._tokens.splice(this._tokens.length - i, i, new LexerToken(TokenType.Operator, combo, first.pos, this._charIdx));
                return;
            }
        }
        this.addToken(TokenType.Operator, char);
    }

    isTypeApplierCharacter(char: string) {
        return char === ":";
    }

    onTypeApplierCharacter(char: string) {
        this.addToken(TokenType.TypeSpecifier, char);
    }

    isOpenParenthesisCharacter(char: string) {
        return char === "(";
    }

    onOpenParenthesisCharacter(char: string) {
        this.addToken(TokenType.OpenParenthesis, char);
    }

    isCloseParenthesisCharacter(char: string) {
        return char === ")";
    }

    onCloseParenthesisCharacter(char: string) {
        this.addToken(TokenType.CloseParenthesis, char);
    }

    isOpenBlockCharacter(char: string) {
        return char === "{";
    }

    onOpenBlockCharacter(char: string) {
        this.addToken(TokenType.OpenBlock, char);
    }

    isCloseBlockCharacter(char: string) {
        return char === "}";
    }

    onCloseBlockCharacter(char: string) {
        this.addToken(TokenType.CloseBlock, char);
    }

    isDelimeterCharacter(char: string) {
        return char === "," || char === ";";
    }

    onDelimeterCharacter(char: string) {
        this.addToken(TokenType.Delimeter, char);
    }
}