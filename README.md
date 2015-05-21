ghcjs-from-typescript
=====================

The goal of this project is to generate Haskell FFI bindings from
TypeScript definition files.  In particular, the initial goal has been
to get convert a
[TypeScript binding for the Ace editor](https://github.com/borisyankov/DefinitelyTyped/blob/master/ace/ace.d.ts).

The conversion to a raw binding already works.  However, `ace.d.ts`
had to be modified a bit due to deficiencies in
[language-typescript](http://hackage.haskell.org/package/language-typescripte):

* top level `var` declarations with object types commented out, as
  they cause parsing to fail.

* `export` keywords are removed by string substituion, as they cause
  parsing to fail.

Running it on `ace.d.ts`
------------------------

Here's what I'm currently using to run this:

> rm -r output; runhaskell src/GHCJSFromTypeScript.hs && cd ghcjs-ace && cabal --config-file=../../../cabal-ghcjs-config install --ghcjs && cd ..
