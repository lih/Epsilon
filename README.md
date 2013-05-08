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
  * `C-s`/`C-g`: insert symbol/group before current node
  * `C-S`: replace current node by new symbol
  * `C-G`: wrap current node in a group
  * `' '`/`<enter>`: insert symbol after current node
  * `ESC`: save and quit
  * `(`: replace the current node with a group containing an empty symbol
  * `)`: move up, and insert symbol after current node
  * mouse drag: rotate the viewpoint
  * any other letter: if a symbol is selected, then append that letter to the symbol. Do nothing otherwise.

These may look strange, but they are very natural so that you will have no
problem getting productive after only a short while. Your code is automatically
laid out as you type, without you needing to intervene.