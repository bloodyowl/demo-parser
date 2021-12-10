type token =
  | OpenParen
  | OpenBracket
  | OpenBrace
  | OpenChevron
  | CloseParen
  | CloseBracket
  | CloseBrace
  | CloseChevron

type error =
  | InvalidChar(string)
  | SyntaxError({closingToken: token, expectedClosingToken: token})
  | UnknownSyntaxError

let parse = input => {
  input
  ->String.split("")
  ->Belt.Array.reduceReverse(Ok(list{}), (list, char) => {
    list->Result.flatMap(list => {
      switch char {
      | "(" => Ok(list{OpenParen, ...list})
      | "[" => Ok(list{OpenBracket, ...list})
      | "{" => Ok(list{OpenBrace, ...list})
      | "<" => Ok(list{OpenChevron, ...list})
      | ")" => Ok(list{CloseParen, ...list})
      | "]" => Ok(list{CloseBracket, ...list})
      | "}" => Ok(list{CloseBrace, ...list})
      | ">" => Ok(list{CloseChevron, ...list})
      | token => Error(InvalidChar(token))
      }
    })
  })
}

let recover = initialTokens => {
  let rec complete = (~openedTokens=list{}, ~tokensToClose=list{}, tokens) => {
    switch (tokens, openedTokens) {
    | (list{}, list{OpenParen, ...openedTokens}) =>
      complete(~openedTokens, ~tokensToClose=list{CloseParen, ...tokensToClose}, list{})
    | (list{}, list{OpenBracket, ...openedTokens}) =>
      complete(~openedTokens, ~tokensToClose=list{CloseBracket, ...tokensToClose}, list{})
    | (list{}, list{OpenBrace, ...openedTokens}) =>
      complete(~openedTokens, ~tokensToClose=list{CloseBrace, ...tokensToClose}, list{})
    | (list{}, list{OpenChevron, ...openedTokens}) =>
      complete(~openedTokens, ~tokensToClose=list{CloseChevron, ...tokensToClose}, list{})
    | (list{}, _) => Ok(initialTokens->List.concat(tokensToClose->List.reverse))
    | (list{(OpenParen | OpenBracket | OpenBrace | OpenChevron) as openingChar, ...tokens}, _) =>
      complete(~openedTokens=list{openingChar, ...openedTokens}, ~tokensToClose, tokens)
    | (list{CloseParen, ...tokens}, list{OpenParen, ...openedTokens})
    | (list{CloseBracket, ...tokens}, list{OpenBracket, ...openedTokens})
    | (list{CloseBrace, ...tokens}, list{OpenBrace, ...openedTokens})
    | (list{CloseChevron, ...tokens}, list{OpenChevron, ...openedTokens}) =>
      complete(~openedTokens, ~tokensToClose, tokens)
    | (list{closingToken, ..._}, list{OpenParen, ..._}) =>
      Error(SyntaxError({closingToken: closingToken, expectedClosingToken: CloseParen}))
    | (list{closingToken, ..._}, list{OpenBracket, ..._}) =>
      Error(SyntaxError({closingToken: closingToken, expectedClosingToken: CloseBracket}))
    | (list{closingToken, ..._}, list{OpenBrace, ..._}) =>
      Error(SyntaxError({closingToken: closingToken, expectedClosingToken: CloseBrace}))
    | (list{closingToken, ..._}, list{OpenChevron, ..._}) =>
      Error(SyntaxError({closingToken: closingToken, expectedClosingToken: CloseChevron}))
    | _ => Error(UnknownSyntaxError)
    }
  }
  complete(initialTokens)
}

let printToken = token => {
  switch token {
  | OpenParen => "("
  | OpenBracket => "["
  | OpenBrace => "{"
  | OpenChevron => "<"
  | CloseParen => ")"
  | CloseBracket => "]"
  | CloseBrace => "}"
  | CloseChevron => ">"
  }
}

let print = (~pretty=false, input) => {
  let printIndent = indent => {
    switch (pretty, indent) {
    | (true, 0) => "\n"
    | (true, indent) => "\n" ++ " "->String.repeat(indent)
    | (false, _) => ""
    }
  }
  let rec print = (~indent=0, input) => {
    switch input {
    | list{} => "\n"
    // Print last level with opening & closing tag at the end on the same line
    | list{
        (OpenParen | OpenBracket | OpenBrace | OpenChevron) as token,
        (CloseParen | CloseBracket | CloseBrace | CloseChevron) as nextToken,
        ...rest,
      } =>
      printIndent(indent) ++ printToken(token) ++ printToken(nextToken) ++ print(~indent, rest)
    | list{(OpenParen | OpenBracket | OpenBrace | OpenChevron) as token, ...rest} =>
      printIndent(indent) ++ printToken(token) ++ print(~indent=indent + 2, rest)
    | list{(CloseParen | CloseBracket | CloseBrace | CloseChevron) as token, ...rest} =>
      printIndent(indent - 2) ++ printToken(token) ++ print(~indent=indent - 2, rest)
    }
  }
  print(input)->String.trimStart
}

let run = input =>
  switch input
  ->parse
  ->Result.flatMap(parsed => recover(parsed))
  ->Result.map(tokens => print(~pretty=true, tokens)) {
  | Ok(recovered) => Console.log(recovered)
  | Error(InvalidChar(char)) => Console.error(`Invalid character: "${char}"`)
  | Error(SyntaxError({closingToken, expectedClosingToken})) =>
    Console.error(
      `Unexpected token: "${closingToken->printToken}", expected "${expectedClosingToken->printToken}"`,
    )
  | Error(UnknownSyntaxError) => Console.error(`Unknown syntax error`)
  }

`{())`->run
`{(é))`->run
`{()`->run
`{[[()`->run
`{[[()]]([[`->run
`((((([[[[[{{{{{{<<<<`->run
