# Script
It's looking more and more writing my own script format (which can be translated
(non-)?isomorphically to Amagami script) would be useful for this project.

  * UTF-8 encoded text
  * "AST" is a **list** of text strings and escape sequences
  * Before conversion, text may be transformed according to options (e.g.
    halfwidth ASCII -> fullwidth)
    * (AST transformations/optimizations)
    * allows us to use characters not present in Shift JIS and transform them
      before converting
    * also lets us be flexible with newlines e.g. remove actual newlines
      and only allow them via the \n escape sequence
    * heck, we should be able to do a clever transformation here which
      automatically inserts a `\{wait 150}` after a comma if it doesn't already
      have one
  * Conversion re-encodes text to Shift JIS, and transforms escape sequences
    according to rules
    * (compilation)
    * probably eg. like `\{0xXX [0x]XX ...}` is a plain bytestring with no
      checks (and arguments are hex-only), while eg. `\{wait}` tries to match to
      a recognised command which can take arguments (e.g. str, int, hex)

## Messing
  * Commands: `wait`

## Textbox
Inputs:

  * regular script textbox but with Shift JIS chars inside a `05 XX 00` UTF-8
    encoded
    * so ASCII still does commands lol
  * 

```
This is a long test,\n
to test...\{wait 

- type: text
  value: This is a long test,
- type: newline
- type: text
  value: to test...
- type: pause
  value: 450
- type: text
  value: " screwing with"
- type: newline
- type: text
  value: the script.

```

## Examples
### Game
Intro scene textboxes.

```
多分気のせいだとは思うんだけど、\{wait 150}
その日の夕焼け空は特別眩しい気がした……。

07 01 00 3A 07 05 00 63 6C 65 61 72 07 04 00 68 69 64 65
```

### Own
