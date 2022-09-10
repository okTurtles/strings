'use strict'

const { createSourceFile, ScriptTarget, SyntaxKind, forEachChild } = require('typescript')

globalThis.extractFromTypeScript = function (code) {
  const acc = []
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
  return [acc, []]
}

const lex = require('pug-lexer')
const parse = require('pug-parser')
const walk = require('pug-walk')

globalThis.extractFromPug = function (code) {
  const ast = parse(lex(code))
  const strings = []
  const possibleJs = []
  walk(ast, function before(node, replace) {
    switch (node.type) {
      case 'Tag':
      case 'Filter':
        node.attrs.forEach(({ val }) => possibleJs.push(val))
        if (node.name === 'i18n') {
          node.block.nodes.forEach(({ type, val }) => {
            if (type === 'Text') {
              strings.push(val)
            }
          })
        }
        break
      case 'Code':
        possibleJs.push(node.val)
        break
      case 'InterpolatedTag':
        possibleJs.push(node.expr)
        break
      case 'Conditional':
      case 'While':
        possibleJs.push(node.test)
        break
    }
  })
  return [strings, possibleJs]
}
