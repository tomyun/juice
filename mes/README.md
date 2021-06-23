# Juice

Juice is a command line utility for parsing MES script used in most games developed by Elf and Silky's in 90s. It can decompile MES bytecodes into a textual source format and then compile it back to bytecodes. By allowing code modification at the source level, the tool is well suited for producing language translation patches for these games. Juice is currently compatible with AI5 (and AI4) engine.

## How to use

1. Decompile

To grab all .MES script files in the current directory and do decompiling,

```
$ ./juice -d *.MES
```

will generate source files with MES.rkt extension.

2. Compile

To compile bytecodes from source files we just generated,

```
$ ./juice -c *.MES.rkt
```

will generate script files with MES.rkt.mes. For example, `START.MES` would be decompiled to `START.MES.rkt` which would be compiled back to `START.MES.rkt.mes`. Ideally, the latter should be identical to the original file.

```
$ cmp START.MES START.MES.rkt.mes
```

If nothing shows up, we're good. Most of the time, differences come from minor issues like disabled text compression in the original or unused garbage code left at the end of file. Some may need more attention, but should be still okay, for example a missing or extra mark for else block which (hopefully) does not affect the game logic. If unsure, try running the game with compiled MES and see how it works. Let me know if any issues found.

3. Deduplicate

Some games are structured in a way that MES files contain repeated chunks of code. They are usually located early in the file and contains a list of procedure definitions (`define-proc`). As some procedures are programmed to display texts, for instance, in a menu, so translation should be done for each single copy of the chunk. To reduce workload, the tool supports deduplication which take a look at all existing source files and extract commonly used blocks of `define-proc` into separate files named like `_proc_NN_xxxxxxxxxxxxxxxx.rkt`. In the modified source, you would find them replaced by `include` instructions.

```
$ ./juice -D *.MES.rkt
```

Then for compiling, you'd need to run `juice -c *.MES.rkt.rkt` (note extension) to pick up deduplicated source files. Resulting MES.rkt.rkt.mes file should be identical to non-deduplicated products of MES.rkt.mes as well as the original MES file.

## Options

Type `juice -h` to see a list of available commands and options.

- `--version`, `-v` : show version.
- `--force`, `-f` : force overwriting output files
- `--charset <c>` : specify charset encoding (**pc98**, europe)
- `--dictbase <b>` : dictionary base (**80**, D0)
- `--extraop` : support incompatible opcodes found in some games
- `--no-decode` : *(decompile)* skip SJIS character decoding
- `--no-resolve` : *(decompile)* skip cmd/sys name resolution
- `--no-protag` : *(decompile)* skip text fusion with protag proc/call
- `--wordwrap <w>` : *(compile)* set threshold for word wrapping
- `--no-compress` : *(compile)* skip text compression with dict

## Guides

### Worfklow

A usual workflow to work on many standard games, such as Doukyuusei 1/2 and Dragon Knight 3/4, would be like below with no further options needed.

```
juice -df *.MES
juice -Df *.MES.rkt
juice -cf *.MES.rkt.rkt
```

### Dictionary

Dictionary is a table storing characters commonly used in the dialogue texts. Characters in the dictionary are referenced by one byte index, in contrast to a typical SJIS code encoded in two bytes, that could greatly reduce the size of compiled script. The dictionary is located at the header of MES file. In rkt source, an existing dictionary is declared with `(dict ..)` instruction found under `meta` section.

```racket
(meta
  (dict #\u3000 #\【 #\】 ..))
```

You would want to replace it with `(dict-build)` to regenerate a dictionary for translated script.

```racket
(meta
  (dict-build))
```

Some games published in later years opted to use a wider region of SJIS codes, leaving a smaller space for dictionary. `--dictbase` is an option to specify two variants of base offset for dictionary index; `80` (128) for many earlier games and `D0` (208) for later games including Jack, Isaku, Kakyuusei, and YU-NO. If `0xD0` was not set for these games, you would likely see an error message saying "list-ref: index too large for list". Since the value of `dictbase` is stored in `meta` section of the decompiled source, you don't have to provide one again for compiling.

```
juice -df --dictbase D0 *.MES
juice -Df *.MES.rkt
juice -cf *.MES.rkt.rkt
```

### Compatibility

While AI5 engine was often adapted specifically for each game release, an overall bytecode structure remained mostly unchanged. Yet, some later games had to make a questionable move and introduce an exception that a couple of new opcodes were laid out in a slightly different way. As the same opcodes were also used in earlier games, the tool needs an additional information to decide which instruction set it is currently trying to understand. Use `-extraop` option for games like Isaku and YU-NO. As the flag is stored in `meta` section, you don't have to repeat this in the remaining steps of workflow.

```
juice -df --dictbase D0 --extraop *.MES
juice -Df *.MES.rkt
juice -cf *.MES.rkt.rkt
```

### Text

Dialogue text is stored in `(text ..)` instruction like this.

```racket
(text "【景】こんにちは。")
```

Sometimes `(text ..)` contains multiple elements, not only strings but also number and others.

```racket
(text "【"0"】こんにちは。")
```

Number 0 between two strings indicate `(proc 0)` call which is often used for displaying name of protagonist. Depending on games, different convention might be used. For example, Kakyuusei uses `(proc 3`). Elle seems to use `(call Z)`. Compiler would take the example code above and turn it into three instructions as below.

```racket
(text "【")
(proc 0)
(text "】こんにちは。")
```

Some games like Doukyuusei extensively uses separate text color for each line of dialogue.

```racket
(text-color 7)
(text "【" 0 "】こんにちは。")
```

Such patterns identified by decompiler would be fused into a short-hand syntax like below using `#:color` keyword.

```racket
(text #:color 7 "【"0"】こんにちは。")
```

For translation, simply replace strings inside `(text ..)` instruction.

```racket
(text "["0"] Hello.")
```

Characters allowed for text string are defined by `(charset ..)` as described next. By choosing a different charset, one can easily work on translation for other languages.

```racket
(text "["0"] 안녕.") ; for Korean
```

```racket
(text "["0"] Allô.") ; for French
```

By the way, any writings after `;` in the same line are treated comments, so use them as needed.

### Charset

By default, characters found in original Japanese scripts are mapped via charset named `pc98` which is a mostly Shift JIS encoding with some PC-98 exclusive characters assigned in rows 12 and 13. A notable non-standard SJIS character found in the script would be the use of `Ⅱ` in Foxy 2.

Source file is encoded in UTF-8. The tool then has to figure out a mapping between said UTF-8 character found in `(text ..)` and Shift JIS bytes. A character would be first looked up in a custom charset to properly handle non-standard characters mentioned above. If not found, a standard conversion would be attempted.

For example, in the case of `Ⅱ`, its mapping would be found in the default `pc98` charset, resulting into JIS code point (13, 22) which is encoded to SJIS bytes (0x87 0x55). Without a custom mapping, a standard SJIS encoder wouldn't be able to decode this particular character as it was not a part of JIS X 0208 standard character set. Note that later extended JIS X 0213 character set adopted a few NEC special characters including `Ⅱ`, but is still not (and cannot be) fully compatible with the original PC-98 character set we're dealing here.

In addition to that, by supplying a proper custom charset mapping, we can easily target custom fonts filled with non-Japanese characters like Korean syllables or French diacritics, which would be extremely useful for translation projects. A custom charset can be specified in `(charset ..)` inside `(meta ..)` section.

```racket
(meta
  (charset "english"))
```

`(charset ..)` loads a predefined charset bundled with the tool. Currently, we have `pc98`, `english`, `europe`, and `korean-kk`.

- `pc98`: SJIS encoding extended with several NEC PC-98 exclusive characters in row 12 and 13.
- `english`: `pc98` extended with ASCII characters mapped to row 9 which was another PC-98 exclusive assigned for half-width ASCII. Requires no custom font.
- `europe`: `pc98` extended with many combinations of diacritics mapped to row 48 and 49. Requires a custom font.
- `korean-kk`: `pc98` extended with Korean syllables mapped to row 19. Used in Korean translation of Shangrlia 2, Jack, Elle, and YU-NO by K.K; Ushinawareta Rakuen, Dragon Knight 4, and Shangrlia by edenrock.

You can create a custom charset file, e.g., "_charset_chinese.rkt", yourself if you want. The charset file may contain a series of charset-related instructions. For example, bundled "_charset_english.rkt" looks like below.

```racket
(charset "pc98")
(charset* 9 1
 #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
 #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
 #\: #\; #\< #\= #\> #\? #\@
 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
 #\[ #\\ #\] #\^ #\_ #\`
 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
 #\{ #\| #\} #\~)
(fontwidth 1)
(charspc #\ )
```

`(charset ..)` in the first line can include an existing charset. `(charset* k t c ..)` in the next defines a mapping for characters listed in `c ..` to JIS code points starting from row `r` and cell `t`. A JIS code point is represented by a pair of (row, cell) which is also called (ku, ten). The pair corresponds to a position in font space, i.e., a specific location in ANEX86.BMP or FREECG98.BMP. For example, character `!` in `english` charset is mapped to JIS code point (9, 1) which translates to (0x85 0x40). Every `!` appeared inside text string would then result into this particular SJIS bytes in the output file. Note that the default SJIS encoding defines a similarly looking, but distinct character for many ASCII letters; full-width `！` (0x81 0x49) is different from regular `!` (0x21). Pay close attention to these subtle differences when typing in translated strings into `(text ..)`.

To decompile existing MES scripts with a particular charset, use `--charset` option.

```
juice -df --charset korean-kk *.MES
```

### Font Width

Most of the time original games would assume each character occupies a full-width (double space) in the screen which may not read very comfortable with Western languages. To get a narrow half-width (single space) texts in translated games, you may need to edit some instructions in the script. Here is a common line of instruction used by these games.

```racket
(set-arr~ @ 21 (+ 512 16))
```

`@` is a name of array containing system variables. The system variable whose index is `21` stores font width and height. Each system variable is two bytes (one word) wide. The first byte stores font height in pixel unit. The second byte stores font width in the unit of "character" which takes 8 pixels horizontally. In the example above, `16` is font height and `512` (= 2 << 8) corresponds to font width. Font width of `2` characters here indicates double space. What we need is to change that `2` into `1` for getting a narrow font width like this.

```racket
;; set up narrow spacing
;; 272 = 256 + 16 = (1 << 8) + 16
(set-arr~ @ 21 272)
```

You may put this code above before a paragraph of translated texts. At the end of the paragraph, you may also want it back to wide spacing.

```racket
;; back to wide spacing
;; 528 = 512 + 16 = (2 << 8) + 16
(set-arr~ @ 21 528)
```

Chances are original scripts already have that wide font setting code all over the place, especially at the beginning of each scene. Sometimes this code would be contained in a common procedure defined in other script files. As it'd wise to keep changes in original instructions small as possible to avoid unexpected results, take a look at common files such as `START1.MES` to see how the game lays out initialization code. Version control with Git would be highly recommended for keeping track of any changes introduced during the project.

### Word Wrap

A long dialogue may span out multiple lines in the text box. Depending on the text, a line break could sometimes take place at an awkward position. Let's take an example.

```racket
(text "[Me] Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris lobortis est id.")
```

In Doukyuusei, where the width of dialogue box is set to 50 single width characters, the dialogue will be rendered like below.

```
[Me] Lorem ipsum dolor sit amet, consectetur adipi
scing elit. Mauris lobortis est id.
```

For getting a nice line break, a few extra space characters could fill in.

```racket
(text "[Me] Lorem ipsum dolor sit amet, consectetur      adipiscing elit. Mauris lobortis est id.")
```

The rendered paragraph is now like this.

```
[Me] Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Mauris lobortis est id.
```

Indeed `(text ..)` instruction support optional keyword `#:wrap` for filling in extra space as needed.

```racket
(text "[Me] Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris lobortis est id." #:wrap 50)
```

To apply automatic word wrapping for an entire script, you may want to use `--wordwrap` option when compiling.

```racket
juice -c --wordwrap 50 *.MES.rkt
```

A better way would be to add `(wordwrap ..)` instruction in `(meta ..)` section.

```racket
(meta
  (wordwrap 50))
```

Each game may use a different value for the width of text box. Even in the same game, text box may have a different size. If you want to disable automatic word wrapping for some paragraphs, use `#f` (false) for the width.

```racket
(text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris lobortis est id." #:wrap #f)
```

### Editor

The syntax used in source file came from underlying Racket language. Enabling syntax highlighting for Racket (hence rkt extension) in the editor would greatly improve an overall look of the text. As a bonus, many trivial syntax errors would be easily caught in this mode. If Racket plugin is not available for your choice of editor, try finding one for Scheme or Lisp and they should also work.

## Contact

If you have any questions, please come to #pc-98_translation_discussion channel on PC-9800 Series Central Discord.

Last edited: 2021-06-21
