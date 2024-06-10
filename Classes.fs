open System
open System.IO

type TokenType =
    | Identifier of string
    | Keyword of string
    | Operator of string
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | CharLiteral of char
    | NewLine
    | EOF

type Token = {
    TokenType: TokenType
    Lexeme: string
    Line: int
    Column: int
}

type Lexer(input: string) =
    let mutable position = 0
    let mutable line = 1
    let mutable column = 1

    let isWhiteSpace (ch: char) = ch = ' ' || ch = '\t'
    let isNewLine (ch: char) = ch = '\n' || ch = '\r'
    let isDigit (ch: char) = ch >= '0' && ch <= '9'
    let isAlpha (ch: char) = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_'
    let isAlphaNumeric (ch: char) = isAlpha ch || isDigit ch

    let rec skipWhiteSpace () =
        while position < input.Length && (isWhiteSpace input.[position] || isNewLine input.[position]) do
            if isNewLine input.[position] then
                if input.[position] = '\r' && position + 1 < input.Length && input.[position + 1] = '\n' then
                    position <- position + 1
                line <- line + 1
                column <- 1
            else
                column <- column + 1
            position <- position + 1

    let readWhile (predicate: char -> bool) =
        let start = position
        while position < input.Length && predicate input.[position] do
            position <- position + 1
        input.[start..position - 1]

    let readIdentifier () =
        let ident = readWhile isAlphaNumeric
        { TokenType = Identifier ident; Lexeme = ident; Line = line; Column = column }

    let readKeywordOrIdentifier () =
        let ident = readWhile isAlphaNumeric
        match ident with
        | "print" | "class" | "def" | "return" -> { TokenType = Keyword ident; Lexeme = ident; Line = line; Column = column }
        | _ -> { TokenType = Identifier ident; Lexeme = ident; Line = line; Column = column }

    let readNumber () =
        let numStr = readWhile isDigit
        if position < input.Length && input.[position] = '.' then
            position <- position + 1
            let fractionalPart = readWhile isDigit
            let floatStr = numStr + "." + fractionalPart
            { TokenType = FloatLiteral (float floatStr); Lexeme = floatStr; Line = line; Column = column }
        else
            { TokenType = IntLiteral (int numStr); Lexeme = numStr; Line = line; Column = column }

    let readString () =
        position <- position + 1
        let str = readWhile (fun ch -> ch <> '\"')
        if position >= input.Length || input.[position] <> '\"' then
            failwithf "Unterminated string literal at line %d, column %d" line column
        position <- position + 1
        { TokenType = StringLiteral str; Lexeme = "\"" + str + "\""; Line = line; Column = column }

    let readChar () =
        position <- position + 1
        let char = input.[position]
        position <- position + 1
        if position >= input.Length || input.[position] <> '\'' then
            failwithf "Unterminated char literal at line %d, column %d" line column
        else
            position <- position + 1
        { TokenType = CharLiteral char; Lexeme = "'" + char.ToString() + "'"; Line = line; Column = column }

    let rec readNextToken () =
        skipWhiteSpace ()
        if position >= input.Length then
            { TokenType = EOF; Lexeme = ""; Line = line; Column = column }
        else
            let ch = input.[position]
            match ch with
            | '(' | ')' | '[' | ']' | '{' | '}' | ':' | ',' | '.' | ';' -> 
                position <- position + 1
                { TokenType = Operator (ch.ToString()); Lexeme = ch.ToString(); Line = line; Column = column }
            | '=' | '+' | '-' | '*' | '/' | '<' | '>' | '&' | '|' | '!' | '^' | '%' ->
                let op = readWhile (fun ch -> "=+-*/<>&|!^%".Contains(ch))
                { TokenType = Operator op; Lexeme = op; Line = line; Column = column }
            | '\"' -> readString ()
            | '\'' -> readChar ()
            | _ ->
                if isDigit ch then
                    readNumber ()
                elif isAlpha ch then
                    readKeywordOrIdentifier ()
                else
                    failwithf "Unexpected character '%c' at line %d, column %d" ch line column

    member this.NextToken () =
        let token = readNextToken ()
        column <- column + token.Lexeme.Length
        token

type Parser(input: string) =
    let lexer = Lexer(input)
    let mutable currentToken = lexer.NextToken()

    let expect (expectedType: TokenType) =
        if currentToken.TokenType = expectedType then
            currentToken <- lexer.NextToken()
        else
            failwithf "Syntax error: Expected '%A' but found '%A' at line %d, column %d" expectedType currentToken.TokenType currentToken.Line currentToken.Column

    let rec parsePrimary () =
        match currentToken.TokenType with
        | Keyword "print" ->
            currentToken <- lexer.NextToken()
            expect (Operator "(")
            let expr = parseExpr()
            expect (Operator ")")
            "printfn \"%d\" (" + expr + ")"
        | Identifier ident ->
            let name = ident.Replace(".", "_")
            currentToken <- lexer.NextToken()
            name
        | IntLiteral value ->
            let intValue = value.ToString()
            currentToken <- lexer.NextToken()
            intValue
        | FloatLiteral value ->
            let floatValue = value.ToString()
            currentToken <- lexer.NextToken()
            floatValue
        | StringLiteral value ->
            let stringValue = "\"" + value + "\""
            currentToken <- lexer.NextToken()
            stringValue
        | CharLiteral value ->
            let charValue = "'" + value.ToString() + "'"
            currentToken <- lexer.NextToken()
            charValue
        | Operator op ->
            if op = "-" then
                currentToken <- lexer.NextToken()
                "-" + parsePrimary ()
            else
                failwithf "Syntax error: Unexpected operator '%s' at line %d, column %d" op currentToken.Line currentToken.Column
        | _ ->
            failwithf "Syntax error: Unexpected token '%s' at line %d, column %d" currentToken.Lexeme currentToken.Line currentToken.Column

    and parseExpr () =
        let left = parsePrimary ()
        match currentToken.TokenType with
        | Operator op when op = "+" || op = "-" || op = "*" || op = "/" ->
            currentToken <- lexer.NextToken()
            let right = parseExpr ()
            left + " " + op + " " + right
        | _ -> left

    let parseAssignment () =
        match currentToken.TokenType with
        | Identifier ident ->
            let varName = ident
            currentToken <- lexer.NextToken()
            expect (Operator "=")
            let expr = parseExpr()
            sprintf "let %s = %s" varName expr
        | _ -> failwithf "Syntax error: Expected an identifier but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column

    let rec parseClass () =
        expect (Keyword "class")
        match currentToken.TokenType with
        | Identifier className ->
            currentToken <- lexer.NextToken()
            expect (Operator ":")
            let body = parseClassBody className
            sprintf "type %s(x: int, y: int) =\n    %s" className body
        | _ -> failwithf "Syntax error: Expected a class name but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column

    and parseClassBody className =
        let mutable members = []
        while currentToken.TokenType <> TokenType.EOF do
            match currentToken.TokenType with
            | Keyword "def" -> members <- members @ [parseMethod className]
            | Identifier ident ->
                let assignment = parseAssignment()
                members <- members @ [assignment]
            | NewLine -> currentToken <- lexer.NextToken()
            | _ -> failwithf "Syntax error: Unexpected token '%s' in class body at line %d, column %d" currentToken.Lexeme currentToken.Line currentToken.Column
        String.Join("\n    ", members)

    and parseMethod className =
        expect (Keyword "def")
        match currentToken.TokenType with
        | Identifier methodName ->
            currentToken <- lexer.NextToken()
            expect (Operator "(")
            let parameters = parseParameters ()
            expect (Operator ")")
            expect (Operator ":")
            let body = parseMethodBody ()
            sprintf "member this.%s(%s) =\n        %s" methodName parameters body
        | _ -> failwithf "Syntax error: Expected a method name but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column

    and parseParameters () =
        let mutable params = []
        while currentToken.TokenType <> Operator ")" do
            match currentToken.TokenType with
            | Identifier param ->
                params <- params @ [param]
                currentToken <- lexer.NextToken()
                if currentToken.TokenType = Operator "," then
                    currentToken <- lexer.NextToken()
            | _ -> failwithf "Syntax error: Expected a parameter but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column
        String.Join(", ", params)

    and parseMethodBody () =
        let mutable body = []
        while currentToken.TokenType <> TokenType.EOF && currentToken.TokenType <> Operator "}" do
            match currentToken.TokenType with
            | Keyword "return" ->
                currentToken <- lexer.NextToken()
                let expr = parseExpr ()
                body <- body @ [sprintf "return %s" expr]
            | Identifier "self" ->
                currentToken <- lexer.NextToken()
                let memberAccess = parseMemberAccess()
                match currentToken.TokenType with
                | Operator "=" ->
                    currentToken <- lexer.NextToken()
                    let expr = parseExpr()
                    body <- body @ [sprintf "this.%s <- %s" memberAccess expr]
                | _ -> body <- body @ [sprintf "this.%s" memberAccess]
            | Identifier ident ->
                let assignment = parseAssignment()
                body <- body @ [assignment]
            | _ -> failwithf "Syntax error: Unexpected token '%s' in method body at line %d, column %d" currentToken.Lexeme currentToken.Line currentToken.Column
        String.Join("\n        ", body)

    and parseMemberAccess () =
        match currentToken.TokenType with
        | Operator "." ->
            currentToken <- lexer.NextToken()
            match currentToken.TokenType with
            | Identifier memberName ->
                currentToken <- lexer.NextToken()
                memberName
            | _ -> failwithf "Syntax error: Expected a member name but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column
        | _ -> failwithf "Syntax error: Expected '.' but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column

    let rec parseClassWithInit () =
        expect (Keyword "class")
        match currentToken.TokenType with
        | Identifier className ->
            currentToken <- lexer.NextToken()
            expect (Operator ":")
            expect (Keyword "def")
            match currentToken.TokenType with
            | Identifier "_init_" ->
                currentToken <- lexer.NextToken()
                expect (Operator "(")
                expect (Identifier "self")
                expect (Operator ",")
                let params = parseParameters()
                expect (Operator ")")
                expect (Operator ":")
                let body = parseInitBody()
                sprintf "type %s(%s) =\n    %s" className params body
            | _ -> failwithf "Syntax error: Expected '_init_' but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column
        | _ -> failwithf "Syntax error: Expected a class name but found '%A' at line %d, column %d" currentToken.TokenType currentToken.Line currentToken.Column

    and parseInitBody () =
        let mutable initBody = []
        while currentToken.TokenType <> TokenType.EOF && currentToken.TokenType <> Operator "}" do
            match currentToken.TokenType with
            | Identifier "self" ->
                currentToken <- lexer.NextToken()
                let memberAccess = parseMemberAccess()
                expect (Operator "=")
                let expr = parseExpr()
                initBody <- initBody @ [sprintf "let mutable %s = %s" memberAccess expr]
            | _ -> failwithf "Syntax error: Unexpected token '%s' in _init_ body at line %d, column %d" currentToken.Lexeme currentToken.Line currentToken.Column
        String.Join("\n    ", initBody)

    member this.Parse () =
        let mutable fsharpCode = ""
        while currentToken.TokenType <> EOF do
            let lineCode =
                match currentToken.TokenType with
                | Keyword "class" -> parseClassWithInit()
                | Keyword "print" -> parsePrimary()
                | Identifier _ -> parseAssignment()
                | _ -> failwithf "Syntax error: Unexpected token '%s' at line %d, column %d" currentToken.Lexeme currentToken.Line currentToken.Column
            fsharpCode <- fsharpCode + lineCode + "\n"
        fsharpCode

[<EntryPoint>]
let main argv =
    // Read the content of the Python file
    let path = @"C:\Users\Gomezyani Kango\Desktop\testfiles"
    let files = Directory.GetFiles(path)
    printfn "Select a file to convert:"
    files |> Array.iteri (fun i file -> printfn "%d: %s" i (Path.GetFileName file))
   
    let choice = Console.ReadLine() |> Int32.Parse
    if choice < 0 || choice >= files.Length then
        failwith "Invalid selection"

    let selectedFile = files.[choice]
    let pythonCode = File.ReadAllText(selectedFile)

    // Parse the Python code
    let parser = Parser(pythonCode)
    let fsharpCode = parser.Parse()

    // Print the translated F# code
    printfn "Translated F# Code:\n%s" fsharpCode

    0