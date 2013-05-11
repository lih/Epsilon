Epsilon : A syntax editor
=========================

In the beginning, the programming arts were carried out using only a
small number of tools : a text editor to view and modify the code; and
a compiler to translate it. A lot of progress has been made in the
field of compilers and languages since then, but text editors are
pretty much the same nowadays as they were then.

Enter syntax editing !

Syntax editing is like text editing, except that you may only navigate
within and modify the structure of your code, while Epsilon makes it 
look good. No more whitespace wars, no more counting parens, no more 
syntax errors...

...just you and your code...

Features
--------

### Implemented:

  * Visualization and navigation within the structure
  * Save/load session (primitive, the file format will definitely change in 
    the future)
  * Modification of the structure (copy/paste, insert/delete, ...)
  * Interactive code evaluation in Epsilon Lisp (primitive, some builtins 
    have yet to be added).

### Coming soon:

  * Multiple files/buffers
  * Better text editing within symbols (instead of always appending at the end)

Installing Epsilon
------------------

<small>For this to work, you need to have 'cabal-install' installed</small>

Installing Epsilon is easy, just type `cabal install` at the root of
the directory and Cabal will do the rest.

Using Epsilon
-------------

The install went okay ? Good !

Now just enter `Epsilon` into a shell to start your first session. 
You should promptly be faced with a view similar to this :

<img src='epsilon-startup.png' alt='Epsilon Startup' width='50%' />

You may now start editing ! The following commands are available :

  * `<arrow>`: move within the structure
  * `C-<arrow>` : drag the selected node within the structure
  * `C-s`: replace current node by new symbol
  * `C-g`: wrap current node in a group
  * `C-S`/`C-G`: insert symbol/group before current node
  * `<space>`/`<enter>`: insert symbol after current node
  * `ESC`: save and quit
  * `(`: replace the current node with a group containing an empty symbol
  * `)`: move up, and insert symbol after current node
  * `C-w`: write save file
  * mouse drag: rotate the expression
  * any other letter: if a symbol is selected, then append that letter to the symbol. Do nothing otherwise.
  * `C-<space>`: add a litteral space at the end of a symbol

These may look strange, but they are very natural so that you will have no
problem getting productive after only a short while. Your code is automatically
laid out as you type, without you needing to intervene.

A dynamic environment with Epsilon Lisp
---------------------------------------

As of version 0.4, the Epsilon Lisp environment is available for your pleasure !

To interact with the environment, just create an expression as usual, then 
select that expression and press `C-e` to evaluate it.

Here is what you can currently do within the Lisp Environment:

  * Add/Remove keybindings to expand functionality (primitives `bind`
    and `unbind`)

  * Get/Modify the current focus to move around (primitives
    `get-focus` and `swap-focus`)

  * Get/Modify the selection to modify the structure (primitives
    `get-selection` and `swap-selection`)

The language itself is very simple. It is a Lisp with two special forms:

  * The `(match (pat1 expr1) ... (patn exprn))` special form creates a
    pattern-matching lambda function where each of the patterns is
    applied in turn until the first match, which will yield the
    corresponding expression.
    
    The patterns themselves are just regular syntax trees that match
    on structural equality, and where subpatterns of two shapes are
    singled out:

      * `(? sym)` acts as a wildcard that captures the matched value
        in the local variable `sym`
      *	`(| sym)` matches the tail of the current list and captures it
        in `sym`
  
  * The backquote (`` ` ``) special form does the opposite of `match`,
    that is, contructs a value. It follows the same semantics as
    Lisp's backquote except that the comma (`,`) is replaced by `!`.

A couple of builtin functions are made available as well to help you
do other useful stuff:

  * `macro(f)` promotes the lambda function `f` to a macro
  * Arithmetic builtins: `+`,`-`,`*`,`/`
  * Other builtins: `map`,`print`,`do`

That is not a lot yet but others are coming !
