MIT: Structure and Interpretation of Computer Programs
======================================================

### Syllabus:

- Watch all lecture video
- Read the book
- Do all exercies in the book
- _Try_ projects & assignments in [MIT OCW][3]

### Original book:

[SICP][1]

### More beautiful book:

[SICP-PDF][2]

### Courseware (Video included):

[MIT OCW][3]

### Environment setup:

- Get Racket, Tmux, Vim and follow this [guide][4]. You can use a packages manager for Vim (I use Vundle) to install a bunch of packages at once. The last time I start coding, I only need to run `tmux` file in `exercises` folder, and all are set!

_Notice_: You may don't want to use `neil/sicp` package like the [guide][4] because of some bugs. Instead you could use `(include)` to include some code from the book resouces or more challenging, write it for yourself!

- In section 1.2 when dealing with random integer larger than 4294967087, install `random-source` package for Racket ( `(require (planet williams/science/random-source))` ) to use `random-integer` function instead of `random`.

[1]: http://mitpress.mit.edu/sicp/
[2]: https://github.com/sarabander/sicp-pdf
[3]: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/syllabus/
[4]: http://crash.net.nz/posts/2014/08/configuring-vim-for-sicp/


