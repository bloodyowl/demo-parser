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
  | (list{}, _) => Ok(tokensToClose->List.reverse)
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

let print = input => {
  input->Belt.List.map(printToken)->Belt.List.reduce("", (a, b) => a ++ b)
}

let run = input =>
  switch input->parse->Result.flatMap(parsed => complete(parsed))->Result.map(print) {
  | Ok(recovered) => Console.log(`${input}${recovered}`)
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
`((((([[[[[{{{{{{<<<<`->run
