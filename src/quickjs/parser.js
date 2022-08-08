'use strict'

var { createSourceFile, ScriptTarget, SyntaxKind, forEachChild } = require('typescript')

globalThis.extract = function (code) {
  var acc = []
  function traverse (node) {
    if (node.kind === SyntaxKind.CallExpression
      && node.expression.escapedText === 'L'
      && node.arguments.length >= 1
      && node.arguments[0].kind === SyntaxKind.StringLiteral
    ) {
      acc.push(node.arguments[0].text)
    }
    forEachChild(node, traverse)
  }
  traverse(createSourceFile('virtual.ts', code, ScriptTarget.ES2015, false))
  return acc
}
