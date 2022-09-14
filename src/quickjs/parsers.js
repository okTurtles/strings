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
  traverse(createSourceFile('<source>', code, ScriptTarget.ES2015, false))
  return {
    strings: acc,
    possibleScripts: [],
    escapedPossibleScripts: []
  }
}

globalThis.extractFromAttr = function (code) {
  try {
    const ast = createSourceFile('<source>', code, ScriptTarget.ES2015, false)
    if (ast.statements.length === 1
      && ast.statements[0].kind === SyntaxKind.ExpressionStatement
      && ast.statements[0].expression.kind === SyntaxKind.StringLiteral
    ) {
      return ast.statements[0].expression.text
    }
  } catch (e) {
    return null
  }
}

const lex = require('pug-lexer')
const parse = require('pug-parser')
const walk = require('pug-walk')

globalThis.extractFromPug = function (code) {
  const ast = parse(lex(code))
  const strings = []
  const possibleScripts = []
  const escapedPossibleScripts = []
  walk(ast, function before(node, replace) {
    switch (node.type) {
      case 'Tag':
      case 'Filter':
        node.attrs.forEach(({ val }) => {
          if (val !== '') {
            escapedPossibleScripts.push(val)
          }
        })
        if (node.name === 'i18n') {
          node.block.nodes.forEach(({ type, val }) => {
            if (type === 'Text' && val !== '') {
              strings.push(val)
            }
          })
        }
        break
      case 'Code':
        possibleScripts.push(node.val)
        break
      case 'InterpolatedTag':
        possibleScripts.push(node.expr)
        break
      case 'Conditional':
      case 'While':
        possibleScripts.push(node.test)
        break
    }
  })
  return { strings, possibleScripts, escapedPossibleScripts }
}
