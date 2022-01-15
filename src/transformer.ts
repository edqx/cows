/**
 * The transformer works like the following:
 * 
 * 5 * 3 + 2
 * => [ 5, *, 3, +, 2 ]
 * 
 * (in order of reverse precedence)
 * find the first + and split tokens on either side
 * [ 5, *, 3 ], [ 2 ]
 * 
 * for each list, run the transformation again
 * [ Arithmetic * (NumberLiteral 5, NumberLiteral 3) ], [ NumberLiteral 2 ]
 * Use the last token on the left hand side as the left operand
 * Use the first token on the right hand side as the right operand
 * 
 * return [
 *   rest on left hand side,
 *   Arithmetic + (
 *     Arithmetic * (NumberLiteral 5, NumberLiteral 3),
 *     NumberLiteral 2
 *   ),
 *   rest on right hand side
 * ];
 * 
 * 
 * if there is no +, find the first number literal and split tokens on either side
 * [ ] [ ]
 * 
 * return [
 *   left hand side expressions,
 *   NumberLiteral 5,
 *   right hand side expressions
 * ];
 */

import { LexerToken, TokenType } from "./lexer";

export enum ExpressionType {
    SourceFile,
    Group,
    Identifier,
    StringLiteral,
    NumberLiteral,
    BooleanLiteral,
    VariableDeclaration,
    FunctionDeclaration,
    ParameterDeclaration,
    IfStatement,
    Assignment,
    ArithmeticOperator,
    EqualityOperator,
    LogicalOperator,
    TypeOperator,
    UnaryArithmeticOperator,
    UnaryLogicalOperator,
    ReferenceExpression,
    PropertyAccessor,
    Return,
    FunctionCall,
    WhileLoop
}

export class Expression {
    constructor(
        public readonly type: ExpressionType,
        public readonly pos: number,
        public readonly end: number
    ) {}
}

export class GroupExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly expressions: Expression[]
    ) {
        super(ExpressionType.Group, pos, end);
    }
}

export class SourceFileExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly file: string,
        public readonly expressions: Expression[]
    ) {
        super(ExpressionType.SourceFile, pos, end);
    }
}

export class IdentifierExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly text: string
    ) {
        super(ExpressionType.Identifier, pos, end);
    }
}

export class StringLiteralExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly text: string
    ) {
        super(ExpressionType.StringLiteral, pos, end);
    }
}

export class BooleanLiteralExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly text: string
    ) {
        super(ExpressionType.BooleanLiteral, pos, end);
    }
}

export class NumberLiteralExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly text: string
    ) {
        super(ExpressionType.NumberLiteral, pos, end);
    }
}

export enum DeclarationFlags {
    Cloud = "cloud",
    Local = "local",
    Async = "async"
}

export class VariableDeclarationExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly name: IdentifierExpression,
        public readonly flags: DeclarationFlags[],
        public readonly typeSpecifier: IdentifierExpression|TypeOperatorExpression|undefined,
        public readonly initializer: Expression
    ) {
        super(ExpressionType.VariableDeclaration, pos, end);
    }
}

export class FunctionDeclarationExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly name: IdentifierExpression,
        public readonly flags: DeclarationFlags[],
        public readonly parameters: TypedSpecifierExpression[],
        public readonly typeSpecifier: IdentifierExpression|TypeOperatorExpression|undefined,
        public readonly body: Expression[]
    ) {
        super(ExpressionType.FunctionDeclaration, pos, end);
    }
}

export class TypedSpecifierExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly name: IdentifierExpression,
        public readonly typeSpecifier: IdentifierExpression|TypeOperatorExpression
    ) {
        super(ExpressionType.ParameterDeclaration, pos, end);
    }
}

export class IfStatementExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly condition: Expression,
        public readonly body: Expression[]
    ) {
        super(ExpressionType.IfStatement, pos, end);
    }
}

export class AssignmentExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly name: IdentifierExpression,
        public readonly value: Expression
    ) {
        super(ExpressionType.Assignment, pos, end);
    }
}

export class ArithmeticOperatorExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly left: Expression,
        public readonly right: Expression
    ) {
        super(ExpressionType.ArithmeticOperator, pos, end);
    }
}

export class EqualityOperatorExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly left: Expression,
        public readonly right: Expression
    ) {
        super(ExpressionType.EqualityOperator, pos, end);
    }
}

export class LogicalOperatorExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly left: Expression,
        public readonly right: Expression
    ) {
        super(ExpressionType.LogicalOperator, pos, end);
    }
}

export class TypeOperatorExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly left: Expression,
        public readonly right: Expression
    ) {
        super(ExpressionType.TypeOperator, pos, end);
    }
}

export class UnaryArithmeticExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly operand: Expression
    ) {
        super(ExpressionType.UnaryArithmeticOperator, pos, end);
    }
}

export class UnaryLogicalExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly operator: string,
        public readonly operand: Expression
    ) {
        super(ExpressionType.UnaryLogicalOperator, pos, end);
    }
}

export class PropertyAccessorExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly base: Expression,
        public readonly property: IdentifierExpression|PropertyAccessorExpression
    ) {
        super(ExpressionType.PropertyAccessor, pos, end);
    }
}

export class ReturnExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly value: Expression|undefined
    ) {
        super(ExpressionType.Return, pos, end);
    }
}

export class FunctionCallExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly identifier: PropertyAccessorExpression|IdentifierExpression,
        public readonly args: Expression[]
    ) {
        super(ExpressionType.FunctionCall, pos, end);
    }
}

export class WhileLoopExpression extends Expression {
    constructor(
        public readonly pos: number,
        public readonly end: number,
        public readonly condition: Expression,
        public readonly body: Expression[]
    ) {
        super(ExpressionType.WhileLoop, pos, end);
    }
}

export class TransformerError extends Error {
    constructor(
        public readonly token: LexerToken,
        public readonly message: string
    ) {
        super();
    }
}

function findFirst(tokens: LexerToken[], filter: (token: LexerToken) => boolean): [ LexerToken, number ]|undefined {
    let scopeStack = [];
    for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];

        if (token.type === TokenType.OpenParenthesis) {
            scopeStack.push("(");
        } else if (token.type === TokenType.OpenBlock) {
            scopeStack.push("{");
        } else if (token.type === TokenType.CloseParenthesis) {
            const lastParenthesis = scopeStack[scopeStack.length - 1];

            if (!lastParenthesis)
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing bracket that was not opened.");

            if (lastParenthesis === "{")
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing code block with bracket.");

            scopeStack.pop();
        } else if (token.type === TokenType.CloseBlock) {
            const lastParenthesis = scopeStack[scopeStack.length - 1];

            if (!lastParenthesis)
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing code block that was not opened.");

            if (lastParenthesis === "(")
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing parenthesis with bracket.");

            scopeStack.pop();
        } else if (filter(token)) {
            return [ token, i ];
        }
    }

    return undefined;
}

function isValueOrReference(expression: Expression): expression is IdentifierExpression|PropertyAccessorExpression {
    return expression.type === ExpressionType.Identifier
        || expression.type === ExpressionType.PropertyAccessor
        || expression.type === ExpressionType.StringLiteral
        || expression.type === ExpressionType.NumberLiteral
        || expression.type === ExpressionType.BooleanLiteral
        || expression.type === ExpressionType.FunctionCall;
}

const unaryTokens = new Set([
    TokenType.Operator,
    TokenType.TypeSpecifier,
    TokenType.OpenParenthesis,
    TokenType.OpenBlock,
    TokenType.CloseBlock,
    TokenType.Delimeter 
]);

function transformTokens(tokens: LexerToken[]): Expression[] {
    const allDeclares: number[] = [];

    let firstConditional: number|undefined = undefined,
        firstReturn: number|undefined = undefined,
        firstTypeSpecifier: number|undefined = undefined,
        firstAssignment: number|undefined = undefined,
        firstLogicalOr: number|undefined = undefined,
        firstLogicalAnd: number|undefined = undefined,
        firstTypeUnion: number|undefined = undefined,
        firstTypeIntersection: number|undefined = undefined,
        firstEquality: number|undefined = undefined,
        firstRangeEquality: number|undefined = undefined,
        firstUnaryArithmetic: number|undefined = undefined,
        firstAdditionArithmetic: number|undefined = undefined,
        firstMultiplyArithmetic: number|undefined = undefined,
        firstOpenParenthesis: number|undefined = undefined,
        firstUnaryLogical: number|undefined = undefined,
        firstPropertyAccessor: number|undefined = undefined,
        firstLiteralString: number|undefined = undefined,
        firstLiteralNumber: number|undefined = undefined,
        firstIdentifier: number|undefined = undefined;

    let scopeStack: ("("|"{")[] = [];
    for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];

        if (token.type === TokenType.OpenParenthesis) {
            if (firstOpenParenthesis === undefined) {
                firstOpenParenthesis = i;
            }
            scopeStack.push("(");
        } else if (token.type === TokenType.OpenBlock) {
            scopeStack.push("{");
        } else if (token.type === TokenType.CloseParenthesis) {
            const lastParenthesis = scopeStack[scopeStack.length - 1];

            if (!lastParenthesis)
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing bracket that was not opened.");

            if (lastParenthesis === "{")
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing code block with bracket.");

            scopeStack.pop();
        } else if (token.type === TokenType.CloseBlock) {
            const lastParenthesis = scopeStack[scopeStack.length - 1];

            if (!lastParenthesis)
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing code block that was not opened.");

            if (lastParenthesis === "(")
                throw new TransformerError(token, "Unbalanced parenthesis; tried closing parenthesis with bracket.");

            scopeStack.pop();
        } else if (scopeStack.length === 0) {
            if (token.type === TokenType.Operator) {
                if (token.value === "=") {
                    if (firstAssignment === undefined)
                        firstAssignment = i;
                } else if (token.value === "||") {
                    if (firstLogicalOr === undefined)
                        firstLogicalOr = i;
                } else if (token.value === "&&") {
                    if (firstLogicalAnd === undefined)
                        firstLogicalAnd = i;
                } else if (token.value === "|") {
                    if (firstTypeUnion === undefined)
                        firstTypeUnion = i;
                } else if (token.value === "&") {
                    if (firstTypeIntersection === undefined)
                        firstTypeIntersection = i;
                } else if (token.value === "==" || token.value === "!=") {
                    if (firstEquality === undefined)
                        firstEquality = i;
                } else if (token.value === ">" || token.value === "<" || token.value === ">=" || token.value === "<=") {
                    if (firstRangeEquality === undefined)
                        firstRangeEquality = i;
                } else if (token.value === "+" || token.value === "-") {
                    if (unaryTokens.has(tokens[i - 1]?.type)) {
                        if (firstUnaryArithmetic === undefined)
                            firstUnaryArithmetic = i;
                        continue;
                    }
                    if (firstAdditionArithmetic === undefined)
                        firstAdditionArithmetic = i;
                } else if (token.value === "*" || token.value === "/" || token.value === "%") {
                    if (firstMultiplyArithmetic === undefined)
                        firstMultiplyArithmetic = i;
                } else if (token.value === "!") {
                    if (firstUnaryLogical === undefined)
                        firstUnaryLogical = i;
                }
            } else if (token.type === TokenType.TypeSpecifier) {
                if (firstTypeSpecifier === undefined)
                    firstTypeSpecifier = i;
            } else if (token.type === TokenType.PropertyAccessor) {
                if (firstPropertyAccessor === undefined)
                    firstPropertyAccessor = i;
            } else if (token.type === TokenType.String) {
                if (firstLiteralString === undefined)
                    firstLiteralString = i;
            } else if (token.type === TokenType.Number) {
                if (firstLiteralNumber === undefined)
                    firstLiteralNumber = i;
            } else if (token.type === TokenType.Identifier) {
                if (token.value === "return") {
                    if (firstReturn === undefined)
                        firstReturn = i;
                    continue;
                } else if (token.value === "declare") {
                    allDeclares.push(i);
                    continue;
                } else if (token.value === "while" || token.value === "if") {
                    if (firstConditional === undefined)
                        firstConditional = i;
                    continue;
                }
                firstIdentifier = i;
            }
        }
    }

    for (let i = 0; i < allDeclares.length; i++) {
        const nextDeclare = allDeclares[i];
        const token = tokens[nextDeclare];

        const declarationFlags = [];
        for (let j = nextDeclare; j < tokens.length; j++) {
            if (tokens[j].type !== TokenType.Identifier)
                break;

            if (tokens[j].value === "function") {
                const leftHandSideExpressions = transformTokens(tokens.slice(0, nextDeclare));
                const functionNameToken = tokens[j + 1];
                const functionName = new IdentifierExpression(
                    functionNameToken.pos,
                    functionNameToken.end,
                    functionNameToken.value
                );

                if (!tokens[j + 2] || tokens[j + 2].type !== TokenType.OpenParenthesis)
                    throw new TransformerError(tokens[j + 1], "Expected parameter declaration list following function name.");
                
                const parameterListTokens: LexerToken[][] = [];
                let collectedParameterTokens: LexerToken[] = [];
                let k = j + 3;
                for (; k < tokens.length; k++) {
                    if (tokens[k].type === TokenType.CloseParenthesis) {
                        if (collectedParameterTokens.length) {
                            parameterListTokens.push(collectedParameterTokens);
                        }
                        break;
                    }
                    if (tokens[k].type === TokenType.Delimeter) {
                        parameterListTokens.push(collectedParameterTokens);
                        collectedParameterTokens = [];
                        continue;
                    }

                    if (tokens[k].type !== TokenType.Identifier && tokens[k].type !== TokenType.TypeSpecifier)
                        throw new TransformerError(tokens[k], "Unexpected token in parameter declaration list.");

                    collectedParameterTokens.push(tokens[k]);
                }

                const parameterDeclarations: TypedSpecifierExpression[] = [];
                for (let l = 0; l < parameterListTokens.length; l++) {
                    const typedIdentifier = transformTokens(parameterListTokens[l])[0];

                    if (!(typedIdentifier instanceof TypedSpecifierExpression))
                        throw new TransformerError(parameterListTokens[0][0], "Expected typed identifier for parameter declaration.");

                    parameterDeclarations.push(typedIdentifier);
                }
                
                let typeSpecifier: IdentifierExpression|TypeOperatorExpression|undefined = undefined;
                k += 1;
                if (tokens[k].type === TokenType.TypeSpecifier) {
                    const typeSpecifierTokens: LexerToken[] = [];
                    k++;
                    for (; k < tokens.length; k++) {
                        if (tokens[k].type == TokenType.OpenBlock) {
                            break;
                        }
                        typeSpecifierTokens.push(tokens[k]);
                    }

                    const typeSpecifierExpr = transformTokens(typeSpecifierTokens)[0];

                    if (!typeSpecifierExpr || !(typeSpecifierExpr instanceof IdentifierExpression) && !(typeSpecifierExpr instanceof TypeOperatorExpression))
                        throw new TransformerError(typeSpecifierTokens[0], "Expected type specifier for return expression");

                    typeSpecifier = typeSpecifierExpr;
                }
                
                k++;
                const blockTokens = [];
                let currentBlockLevel = 0;
                for (; k < tokens.length; k++) {
                    if (tokens[k].type === TokenType.OpenBlock) {
                        currentBlockLevel++;
                    } else if (tokens[k].type === TokenType.CloseBlock) {
                        if (currentBlockLevel === 0) {
                            break;
                        }
                        currentBlockLevel--;
                    }
                    blockTokens.push(tokens[k]);
                }

                const rightHandSideExpressions = transformTokens(tokens.slice(k + 1));

                return [
                    ...leftHandSideExpressions,
                    new FunctionDeclarationExpression(
                        token.pos,
                        tokens[k].end,
                        functionName,
                        declarationFlags as DeclarationFlags[],
                        parameterDeclarations,
                        typeSpecifier,
                        transformTokens(blockTokens)
                    ),
                    ...rightHandSideExpressions
                ];
            } else {
                declarationFlags.push(tokens[j].value);
            }
        }
    }

    if (firstConditional !== undefined) {
        const token = tokens[firstConditional];
        
        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstConditional));

        const groupedTokens: LexerToken[][] = [];
        let tokenCollect: LexerToken[] = [];
        let currentBracketLevel = 0;
        let j = firstConditional + 2;
        for (; j < tokens.length; j++) {
            if (tokens[j].type === TokenType.Delimeter && currentBracketLevel === 0) {
                if (tokens[j].value === ";")
                    throw new TransformerError(tokens[j], "Unexpected semi-colon in condition array.");

                groupedTokens.push(tokenCollect);
                tokenCollect = [];
                continue;
            }
            if (tokens[j].type === TokenType.OpenParenthesis) {
                currentBracketLevel++;
            } else if (tokens[j].type === TokenType.CloseParenthesis) {
                if (currentBracketLevel === 0) {
                    if (tokenCollect.length) {
                        groupedTokens.push(tokenCollect);
                    }
                    tokenCollect = [];
                    break;
                }

                currentBracketLevel--;
            }
            tokenCollect.push(tokens[j]);
        }

        const conditionExpression = groupedTokens.map(tokens => transformTokens(tokens)[0])[0];

        if (!conditionExpression)
            throw new TransformerError(tokens[firstConditional + 2], "Expected conditional statement.");

        if (tokens[j + 1].type === TokenType.OpenBlock) {
            const blockTokens = [];
            j += 2;
            let currentBlockLevel = 0;
            for (; j < tokens.length; j++) {
                if (tokens[j].type === TokenType.OpenBlock) {
                    currentBlockLevel++;
                } else if (tokens[j].type === TokenType.CloseBlock) {
                    if (currentBlockLevel === 0) {
                        break;
                    }
                    currentBlockLevel--;
                }
                blockTokens.push(tokens[j]);
            }
            
            const rightHandSideExpressions = transformTokens(tokens.slice(j + 1));

            if (token.value === "while") {
                return [
                    ...leftHandSideExpressions,
                    new WhileLoopExpression(
                        token.pos,
                        tokens[j].end,
                        conditionExpression,
                        transformTokens(blockTokens)
                    ),
                    ...rightHandSideExpressions
                ];
            } else if (token.value === "if") {
                return [
                    ...leftHandSideExpressions,
                    new IfStatementExpression(
                        token.pos,
                        tokens[j].end,
                        conditionExpression,
                        transformTokens(blockTokens)
                    ),
                    ...rightHandSideExpressions
                ];
            }
        } else {
            const rightHandSideExpressions = transformTokens(tokens.slice(j + 1));
            const singleExpressionBody = rightHandSideExpressions.shift();

            if (!singleExpressionBody) {
                if (token.value === "while") {
                    throw new TransformerError(tokens[j], "Missing body after while statement");
                } else if (token.value === "if") {
                    throw new TransformerError(tokens[j], "Missing body after if statement");
                }
                throw new TransformerError(tokens[j], "Missing body after statement");
            }

            if (token.value === "while") {
                return [
                    ...leftHandSideExpressions,
                    new WhileLoopExpression(
                        token.pos,
                        tokens[j].end,
                        conditionExpression,
                        [ singleExpressionBody ]
                    ),
                    ...rightHandSideExpressions
                ];
            } else if (token.value === "if") {
                return [
                    ...leftHandSideExpressions,
                    new IfStatementExpression(
                        token.pos,
                        tokens[j].end,
                        conditionExpression,
                        [ singleExpressionBody ]
                    ),
                    ...rightHandSideExpressions
                ];
            }
        }
    }

    for (let i = 0; i < allDeclares.length; i++) {
        const nextDeclare = allDeclares[i];
        const token = tokens[nextDeclare];

        const declarationFlags = [];
        for (let j = nextDeclare; j < tokens.length; j++) {
            if (tokens[j].type !== TokenType.Identifier)
                break;

            if (tokens[j].value === "var") {
                const leftHandSideExpressions = transformTokens(tokens.slice(0, nextDeclare));
                const variableNameToken = tokens[j + 1];
                const variableName = new IdentifierExpression(
                    variableNameToken.pos,
                    variableNameToken.end,
                    variableNameToken.value
                );

                if (tokens[j + 2].type === TokenType.TypeSpecifier) {
                    const typeSpecifierTokens: LexerToken[] = [];
                    let k = j + 3;
                    for (; k < tokens.length; k++) {
                        if (tokens[k].type == TokenType.Operator && tokens[k].value === "=") {
                            break;
                        }
                        typeSpecifierTokens.push(tokens[k]);
                    }

                    const typeSpecifier = transformTokens(typeSpecifierTokens)[0];

                    if (!typeSpecifier || !(typeSpecifier instanceof IdentifierExpression) && !(typeSpecifier instanceof TypeOperatorExpression))
                        throw new TransformerError(tokens[j], "Expected type specifier");

                    const rightHandSideExpressions = transformTokens(tokens.slice(k + 1));
                    const initialiserValue = rightHandSideExpressions.shift();

                    if (!initialiserValue)
                        throw new TransformerError(tokens[j], "Expected initializer for variable declaration.");

                    return [
                        ...leftHandSideExpressions,
                        new VariableDeclarationExpression(
                            token.pos,
                            initialiserValue.end,
                            variableName,
                            declarationFlags as DeclarationFlags[],
                            typeSpecifier,
                            initialiserValue
                        ),
                        ...rightHandSideExpressions
                    ];
                } else {
                    const rightHandSideExpressions = transformTokens(tokens.slice(j + 3));
                    const initialiserValue = rightHandSideExpressions.shift();

                    if (!initialiserValue)
                        throw new TransformerError(tokens[j], "Expected initializer for variable declaration.");

                    return [
                        ...leftHandSideExpressions,
                        new VariableDeclarationExpression(
                            token.pos,
                            initialiserValue.end,
                            variableName,
                            declarationFlags as DeclarationFlags[],
                            undefined,
                            initialiserValue
                        ),
                        ...rightHandSideExpressions
                    ];
                }
            } else {
                declarationFlags.push(tokens[j].value);
            }
        }
    }

    if (firstReturn !== undefined) {
        const token = tokens[firstReturn];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstReturn));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstReturn + 1));

        if (rightHandSideExpressions.length === 0 || tokens[firstReturn + 1] && tokens[firstReturn + 1].type === TokenType.Delimeter && tokens[firstReturn + 1].value == ";") {
            return [
                ...leftHandSideExpressions,
                new ReturnExpression(
                    token.pos,
                    token.end,
                    undefined
                ),
                ...rightHandSideExpressions
            ];
        }
        
        const rightOperand = rightHandSideExpressions.shift();

        if (!rightOperand || !isValueOrReference(rightOperand))
            throw new TransformerError(token, "Expected full expression (e.g. value or reference) for return value.");

        return [
            ...leftHandSideExpressions,
            new ReturnExpression(
                token.pos,
                rightOperand.end,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstTypeSpecifier !== undefined) {
        const token = tokens[firstTypeSpecifier];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstTypeSpecifier));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstTypeSpecifier + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();
        
        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for type specifier (:).");
            
        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for type specifier (:).");

        if (!(leftOperand instanceof IdentifierExpression))
            throw new TransformerError(token, "Invalid left operand for type specifier (:), expected basic identifier.");

        if (!(rightOperand instanceof IdentifierExpression))
            throw new TransformerError(token, "Invalid right operand for type specifier (:), expected basic identifier.");

        return [
            ...leftHandSideExpressions,
            new TypedSpecifierExpression(
                leftOperand.pos,
                rightOperand.end,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstAssignment !== undefined) {
        const token = tokens[firstAssignment];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstAssignment));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstAssignment + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for assignment (=).");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for assignment (=).");

        if (!(leftOperand instanceof IdentifierExpression))
            throw new TransformerError(token, "Invalid left operand assignment (=), expected basic identifier.");

        return [
            ...leftHandSideExpressions,
            new LogicalOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstLogicalOr !== undefined) {
        const token = tokens[firstLogicalOr];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstLogicalOr));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstLogicalOr + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for logical OR (||) operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for logical OR (||) operator.");

        return [
            ...leftHandSideExpressions,
            new LogicalOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstLogicalAnd !== undefined) {
        const token = tokens[firstLogicalAnd];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstLogicalAnd));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstLogicalAnd + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for logical AND (&&) operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for logical AND (&&) operator.");

        return [
            ...leftHandSideExpressions,
            new LogicalOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstTypeUnion !== undefined) {
        const token = tokens[firstTypeUnion];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstTypeUnion));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstTypeUnion + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for type UNION (|) operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for type UNION (|) operator.");

        return [
            ...leftHandSideExpressions,
            new TypeOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstTypeIntersection !== undefined) {
        const token = tokens[firstTypeIntersection];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstTypeIntersection));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstTypeIntersection + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for type INTERSECTION (&) operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for type INTERSECTION (&) operator.");

        return [
            ...leftHandSideExpressions,
            new TypeOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstEquality !== undefined) {
        const token = tokens[firstEquality];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstEquality));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstEquality + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for equality (" + token.value + ") operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for equality (" + token.value + ") operator.");

        return [
            ...leftHandSideExpressions,
            new ArithmeticOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstRangeEquality !== undefined) {
        const token = tokens[firstRangeEquality];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstRangeEquality));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstRangeEquality + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for range equality (" + token.value + ") operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for range equality (" + token.value + ") operator.");

        return [
            ...leftHandSideExpressions,
            new ArithmeticOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstAdditionArithmetic !== undefined) {
        const token = tokens[firstAdditionArithmetic];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstAdditionArithmetic));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstAdditionArithmetic + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for addition arithmetic (" + token.value + ") operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for addition arithmetic (" + token.value + ") operator.");

        return [
            ...leftHandSideExpressions,
            new ArithmeticOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstMultiplyArithmetic !== undefined) {
        const token = tokens[firstMultiplyArithmetic];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstMultiplyArithmetic));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstMultiplyArithmetic + 1));

        const leftOperand = leftHandSideExpressions.pop();
        const rightOperand = rightHandSideExpressions.shift();

        if (!leftOperand)
            throw new TransformerError(token, "Missing left operand for multiply arithmetic (" + token.value + ") operator.");

        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for multiply arithmetic (" + token.value + ") operator.");

        return [
            ...leftHandSideExpressions,
            new ArithmeticOperatorExpression(
                leftOperand.pos,
                rightOperand.end,
                token.value,
                leftOperand,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstOpenParenthesis !== undefined) {
        const token = tokens[firstOpenParenthesis];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstOpenParenthesis));

        const groupedTokens: LexerToken[][] = [];
        let tokenCollect: LexerToken[] = [];
        let currentBracketLevel = 0;
        let j = firstOpenParenthesis + 1;
        for (; j < tokens.length; j++) {
            if (tokens[j].type === TokenType.Delimeter && currentBracketLevel === 0) {
                if (tokens[j].value === ";")
                    throw new TransformerError(tokens[j], "Unexpected semi-colon in function argument list.");

                groupedTokens.push(tokenCollect);
                tokenCollect = [];
                continue;
            }
            if (tokens[j].type === TokenType.OpenParenthesis) {
                currentBracketLevel++;
            } else if (tokens[j].type === TokenType.CloseParenthesis) {
                if (currentBracketLevel === 0) {
                    if (tokenCollect.length) {
                        groupedTokens.push(tokenCollect);
                    }
                    tokenCollect = [];
                    break;
                }

                currentBracketLevel--;
            }
            tokenCollect.push(tokens[j]);
        }

        const argumentExpressions = groupedTokens.map(tokens => transformTokens(tokens)[0]);
        const expressionsOnRight = transformTokens(tokens.slice(j + 2));
        
        const left = leftHandSideExpressions.pop();

        if (!left || !isValueOrReference(left))
            throw new TransformerError(token, "Tried to call a non literal or references.");

        return [
            ...leftHandSideExpressions,
            new FunctionCallExpression(
                left.pos,
                tokens[j].end,
                left,
                argumentExpressions
            ),
            ...expressionsOnRight
        ];
    }
    
    if (firstUnaryArithmetic !== undefined) {
        const token = tokens[firstUnaryArithmetic];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstUnaryArithmetic));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstUnaryArithmetic + 1));

        const rightOperand = rightHandSideExpressions.shift();
                
        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for unary arithmetic operator.");
    
        return [
            ...leftHandSideExpressions,
            new UnaryArithmeticExpression(
                token.pos,
                rightOperand.end,
                token.value,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstUnaryLogical !== undefined) {
        const token = tokens[firstUnaryLogical];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstUnaryLogical));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstUnaryLogical + 1));

        const rightOperand = rightHandSideExpressions.shift();
                
        if (!rightOperand)
            throw new TransformerError(token, "Missing right operand for logical NOT (!) operator.");
    
        return [
            ...leftHandSideExpressions,
            new UnaryLogicalExpression(
                token.pos,
                rightOperand.end,
                token.value,
                rightOperand
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstPropertyAccessor !== undefined) {
        const token = tokens[firstPropertyAccessor];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstPropertyAccessor));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstPropertyAccessor + 1));

        const baseExpression = leftHandSideExpressions.pop();
        const propertyIdentifier = rightHandSideExpressions.shift();
        
        if (!baseExpression)
            throw new TransformerError(token, "Missing base expression for property accessing.");
            
        if (!propertyIdentifier)
            throw new TransformerError(token, "Missing property identifier for property accessor.");

        if (!(propertyIdentifier instanceof IdentifierExpression) && !(propertyIdentifier instanceof PropertyAccessorExpression))
            throw new TransformerError(token, "Right hand expression for property accessor must be an identifier or another property accessor.");

        return [
            ...leftHandSideExpressions,
            new PropertyAccessorExpression(
                baseExpression.pos,
                propertyIdentifier.end,
                baseExpression,
                propertyIdentifier
            ),
            ...rightHandSideExpressions
        ];
    }

    if (firstLiteralString !== undefined) {
        const token = tokens[firstLiteralString];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstLiteralString));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstLiteralString + 1));

        return [
            ...leftHandSideExpressions,
            new StringLiteralExpression(
                token.pos,
                token.end,
                token.value
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstLiteralNumber !== undefined) {
        const token = tokens[firstLiteralNumber];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstLiteralNumber));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstLiteralNumber + 1));

        return [
            ...leftHandSideExpressions,
            new NumberLiteralExpression(
                token.pos,
                token.end,
                token.value
            ),
            ...rightHandSideExpressions
        ];
    }
    
    if (firstIdentifier !== undefined) {
        const token = tokens[firstIdentifier];

        const leftHandSideExpressions = transformTokens(tokens.slice(0, firstIdentifier));
        const rightHandSideExpressions = transformTokens(tokens.slice(firstIdentifier + 1));

        if (token.value === "true" || token.value === "false") {
            return [
                ...leftHandSideExpressions,
                new BooleanLiteralExpression(
                    token.pos,
                    token.end,
                    token.value
                ),
                ...rightHandSideExpressions
            ];
        }

        return [
            ...leftHandSideExpressions,
            new IdentifierExpression(
                token.pos,
                token.end,
                token.value
            ),
            ...rightHandSideExpressions
        ];
    }

    return [];
}

export function transformSource(tokens: LexerToken[], file: string) {
    const transformedTokens = transformTokens(tokens);
    const first = transformedTokens[0];

    if (!first) {
        return new SourceFileExpression(0, 0, file, []);
    }

    return new SourceFileExpression(
        first.pos,
        transformedTokens[transformedTokens.length - 1].end,
        file,
        transformedTokens
    );
}