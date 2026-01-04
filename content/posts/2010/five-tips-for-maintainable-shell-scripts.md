---
title: "Five tips for maintainable shell scripts"
date: 2010-03-12 09:00:07
slug: five-tips-for-maintainable-shell-scripts
categories: [Toolbox]
comments:
    - id: 164
      author: bartman
      date: "2010-03-12 10:16:23"
      content: |
        I prefer to use
        
          something | xargs -n1 -i{} echo "foo {} bar"
        
        instead of using while-read-do-done
    - id: 166
      author: Edward Z. Yang
      date: "2010-03-12 10:47:26"
      content: "That can work, although it gets a little hairy if you want to have multiple commands execute per line."
    - id: 168
      author: nelhage
      date: "2010-03-12 10:53:44"
      content: |
        The read loop has a number of caveats, including:
        
        <ul>
        <li>read processes backslashes at end-of-line, which is rarely a problem, but is probably not what you  need
        <li>If you use a command inside the loop that reads from stdin, it's very easy to accidentally slurp the rest of the lines, terminating your loop early. A classic example is 'ssh', which eats all of its stdin, even if you don't open a pty or spawn a command that uses its own stdin.
        </ul>
        
        An alternative to the xargs strategy is to set IFS to a newline and use a for loop, but that's bordering on arcane.
    - id: 171
      author: Michael
      date: "2010-03-12 15:35:38"
      content: |
        More on the `blah | while read var; do something_with "$var"; done` trick.
        
        First, the obvious (but annoying) point: you can't usefully set shell variables inside the loop and expect them to be useful outside (because you're inside a pipe).
        
        Second, if you really care about what `blah` is feeding you, it's a good idea to use `read -r` which gives you the raw line: very useful, though of course using `read` for simple parsing is also helpful.
    - id: 174
      author: Iain
      date: "2010-03-12 23:01:52"
      content: |
        Why not do
        
        for name in `pgrep bash`; do
          echo "PID: $name"
        done
        
        instead?
    - id: 175
      author: Edward Z. Yang
      date: "2010-03-12 23:07:24"
      content: "Iain, for this particular case, the semantics are the same; however, for a command that may output lines with spaces in them, \"for\" will split them up."
---

When I was seventeen, I wrote my [very first shell script](http://repo.or.cz/w/htmlpurifier-web.git/blob/136caa2d941e51e5a742df3b05fb3e596f778636:/releases/build.bat). It was a Windows batch file, bits and pieces very carefully cargo-culted from various code samples on the web. I had already had the *exquisite* pleasure of futzing with `pear.bat`, and the thought of scripting was not something I relished; "why not write the damn thing in a *real* programming language!" (The extra delicious bit was "a real programming language" was PHP. Hee.)

Eventually I came around to an all-Unix environment, and with it I began to use bash extensively. And suddenly, shell scripting made a lot more sense: you've been writing the damn commands day in and day out, just write them to a script instead! There was, however, still the pesky little problem that shell scripts are forever; like it or not, they've become pieces of maintained code. Entire build infrastructures have been built on top of shell scripts. They breed like rabbits; you have to be careful about the little buggers.

Here are five tips and tricks to keep in mind when tossing commands into a shell script that will make maintenance in the long-run much more pleasant!

1.  Learn and love to use `set`. There is almost always no good reason not to use the `-e` flag, which causes your script to error out if any command returns with a nonzero exit code, and `-x` can save you hours of debugging by printing precisely what command the script is executing before executing it. With the two enabled, you get very simple "assertions" in your shell script:

        check_some_condition
        ! [ -s "$1" ]

    although, if at all possible, you should write error messages to accompany them.

2.  Just because you don't define subprocedures when you're at your terminal (or do you? see `alias` and friends) and use reverse command history search with `C-r` doesn't mean it's acceptable to repeat commands over and over again your shell script. In particular, if you have a set of commands that *might* go into a separate script, but you feel funny about making a separate file, stuff them in a subprocedure like this:

        subcommand() {
          do_something_with "$1" "$2"
        }

    In particular, argument passing acts exactly the same way it does in a real shell script, and generally you can treat the subcommand as if it were it's own script; standard input and output work the way you expect them to. The only differences is are that `exit` exits the whole script, so if you'd like to break out of a command use `return` instead.

3.  Argument quoting in shell scripts is a strange and arcane domain of knowledge (although it doesn't have to be; [check out Waldman's notes on shell quoting](http://www.mpi-inf.mpg.de/~uwe/lehre/unixffb/quoting-guide.html)). The short version is you *always* want to wrap variables that will be interpolated with quotes, unless you actually want multiple arguments semantics. I have mixed feelings about whether or not literals should be quoted, and of late have fallen to the dismal habit of not quoting them.

4.  Believe it or not, shell scripting has functional programming leanings. `xargs`, for example, is the quintessential "map" functionality. However, if the command you are pushing arguments to doesn't take multiple arguments, you can use this trick:

        pgrep bash | while read name; do
          echo "PID: $name"
        done

5.  Shell scripting feels incredibly natural when speaking imperatively, and mostly remains this way when you impose control flow. However, it is absolutely a *terrible* language for any data processing (exhibit 1: sed and perl pipelines) and you should avoid doing too much data crunching in it. Creating utility scripts in more reasonable languages can go a long way to keeping your shell scripts pretty.
