# UNIX for CS 200/201 - Spring 2024.

## UNIX. A toe in the water.

We are assuming that you guys are UNIX neophytes.

## Interactive Tutorial

As a complement to the document which you are now reading, we have an online, interactive tutorial that mirrors the topics in this tutorial. You may run it on the zoo by issuing the following command:

    /c/cs201/www/unixtutorial.py

The tutorial is written in Python (version 3). If you have Python on your own machine, you may download the code at [unixtutorial.py](https://zoo.cs.yale.edu/classes/cs201/unixtutorial.py). The code was written by Nathan Lin, (Yale 2019), who was also a ULA for CS 201. To run it, please make sure that you are in your home directory on the zoo.

## CS 201 YouTube Channel UNIX video

See [Ansel Bobrow's UNIX introduction for CS 201.](https://www.youtube.com/watch?v=QnVfDX2FciM)

## History

*   1969\. Bell Labs. Dennis Ritchie, Ken Thompson.
    
*   PDP-7. (Programmed Data Processor from DEC Digital Equipment Corporation. It was one of the first mini-computers and cost $72,000.) to write Space Travel, a spacewar video game.
    
*   1970\. Ported to PDP-11 to support edit and text formatting (roff variant of DEC runoff), and then troff for typesetting.
    
*   1972\. UNIX rewritten in C. (Brian Kernighan)
    
*   Inspired by MIT operating system: Multics (Multiplexed Information and Computing Service), which included
    
    *   hierarchical file system
        
    *   command processor (aka, shell)
        
    *   access control lists at the file level
    
*   Thompson felt that Multics was far too complex. The name Unics (uniplexed information and computing service) or UNIX (castrated multics) conveyed this scaled back ambition.
    
*   1973\. Paper presented at conference. Began to get requests from outside ATT.
    
*   Bell Labs was part of ATT, which was operating under a 1956 consent decree by the Justice Department to avoid anti-trust action. Under that decree, ATT was not allowed to compete in the computer market. Therefore, it could not commercialize UNIX or C. Instead, it licensed the software to educational and research institutions. (I first used UNIX at a university in 1975.)
    
*   UNIX and C flourished in the educational community. The price was right (free) and it ran on inexpensive (for the time) hardware.
    
*   It was relatively easy to port to new hardware. At the time, when you created a new computer, you had to write the operating system software as well. There was no such thing as a portable operating system. UNIX changed that. Most of system was written in C. Only a small fraction had to be written in machine language. That meant to port UNIX to a new machine, you only had to port the C compiler. In fact, you could use a C compiler on another machine and simply change the code generation module. See [The Portable C Compiler](https://en.wikipedia.org/wiki/Portable_C_Compiler) for more information.
    
*   A compiler does the following:
    
    *   Input: computer program source code
        
    *   Step 1: Lexical analysis: convert string of characters to tokens. E.g.,
        
        def add1(n):
          return n+1
        
        becomes the string of tokens
        
        def, add1, leftparen, n, rightparen, colon, return, n, plus, 1
        
    *   Step 2: Parsing: The tokens are parsed into a structure, like a tree,
        
    *   Step 3: Code Generation: The parse tree is traversed, and machine code is generated at each node of the tree.
        
    *   Output: Machine code, which can be executed on the target machine. (Python, like Java, generates byte code, not machine code. The byte code is run using a virtual machine, in both Python and Java. This increases portability.)
    
*   For porting UNIX, you needed to change only the code generation step to target the machine language of the new computer.
    
*   As a result, writing an operating system for a new machine was no longer a 500 person-month job. It was more like a 5 person-month project. The speed-up was a couple of orders of magnitude.
    
*   1975\. Began shipping to commercial companies for $20,000 licensing fee.
    
*   1980\. ARPA (Advanced Research Projects Agency) has University of California at Berkeley port UNIX to the DEC VAX (BSD - Berkeley System Distribution). Wanted to use VAX to support ARPANET. Included the ex and vi editors and the csh, as well as networking support (TCP/IP stack). Bill Joy, a graduate student, was the prime force. He later founded Sun Computers, where he helped develop Java and other stuff. Note that Joy, like Page and Brin, never completed his doctorate. What a waste.
    
*   1983\. Microsoft starts selling Xenix, which ran on 8086, as provided an alternative to DOS.
    
*   1983\. The Justice Department lifts the consent decree as part of breaking up the Bell System. ATT can now be in the computer business and sell UNIX.
    
*   1983\. Richard Stallman starts the GNU foundation and more or less invents open source software. (Shortly after, the author witnessed a debate in which Stallman argued that computer software should be free. By the end of the debate, the person arguing in favor of commercial software was convinced of Stallman's position.)

### UNIX: A Video History.

*   [Unix: History and Memoir](https://www.youtube.com/watch?v=nS-0Vrmok6Y) Brian Khernighan
*   [Unix History](https://www.youtube.com/watch?v=_2NI6t2r_Hs) Rob Pike
*   [UNIX System](https://www.youtube.com/watch?v=tc4ROCJYbm0) from AT&T Archives
*   [Early History of Unix](https://www.youtube.com/watch?v=XuzeagzQwRs) Warner Losh
*   [Unix50: The Origin of Unix](https://www.youtube.com/watch?v=l03CF9_078I) Brian Kernighan, Doug McIlroy, Peter Weinberger, Jon Bentley, Stephen Johnson

## UNIX Principle 1: the file system is a tree.

We next examine the UNIX file system and related commands. Actually, you are already familiar with the UNIX file system because it is congruent with the file systems on Macs and Windows, namely, it is a hierarchical or tree structure.

Some observers have noted that the biggest single design flaw in computer software was Microsoft's decision to use the backslash (\\) instead of the forward slash (/) as the node delimiter in DOS.

*   The tree has a single root node, aka, a directory.
    
*   Each node can contain files and directories, that is, additional nodes.
    
*   Within a directory, each name (file or directory) must be unique. That is, you cannot have two files named data.txt However, UNIX is case sensitive, which means that you can have all of the following files in the same directory: data.txt Data.txt data.Txt datA.txt but I don't recommend it.
    
*   Using all possible combinations of upper and lower case letters, how many versions of data.txt are possible? What is the formula?
    
*   The UNIX root node is /
    
*   In any directory, . refers to the directory itself.
    
*   In any directory (except root), .. refers to the parent directory.
    
*   Each user has a home directory, e.g., /home/accts/netid. You can reference the home directory using the tilde ~ character: ~/ is your own home directory. ~usernetid is the home directory of user usernetid

Below are some common UNIX commands related to files and directories:

| command | description |
|-|-|
| pwd | present working directory. Where am I?
| cd | Change directory. With no argument, goes to home directory. Otherwise, moves to specified directory.
| mkdir| Make a new directory: `mkdir dirname`
| mv | Move. Rename a file or directory: `mv oldname newname`
| ls | List the contents of a directory.
| ls -l | List the detailed contents of a directory, including access rights, size, modification date.
|ls -a | List all files, including hidden files that start with a period (.)
| date | Print the current date and time. `date`
| touch | Reset the modification date and time to now. Useful with make build process. `touch filename`
cp | Copy a file: `cp filename copyoffile`
rm | Remove. Delete a file: `rm filename`
rmdir | Remove directory. Delete a directory (which needs to be empty): `rmdir dirname`
cat | Concatenate. Print out the contents of a file: `cat filename`
head | Print out the first lines of a file: `head filename`
tail | Print out the last lines of a file: `tail filename`
more | Print out the contents of file one screen at a time: `more filename`
less | Print out the contents of file one screen at a time: less filename. Like more, but allows backward movement.
man | Manual. Displays the online manual page for the given argument: man man displays the documentation for the man command itself. Typically used more or less

### Digression: IBM Job Control Language

As mentioned in the history section above, UNIX gained popularity in part because it ran on computers that were far less expensive than mainframe computers. Another reason is that the command language was far simpler. The dominant command language of the 1960's and 1970's was [IBM Job Control Language](https://en.wikipedia.org/wiki/Job_Control_Language) or simply JCL.

(from Wikipedia): For example, to copy a file on Unix operating system, the user would enter a command like:

    cp oldFile newFile

The following example, using JCL, might be used to copy a file on OS/360:

    //IS198CPY JOB (IS198T30500),'COPY JOB',CLASS=L,MSGCLASS=X
    //COPY01   EXEC PGM=IEBGENER
    //SYSPRINT DD SYSOUT=\*
    //SYSUT1   DD DSN=OLDFILE,DISP=SHR
    //SYSUT2   DD DSN=NEWFILE,
    //            DISP=(NEW,CATLG,DELETE),
    //            SPACE=(CYL,(40,5),RLSE),
    //            DCB=(LRECL=115,BLKSIZE=1150)
    //SYSIN  DD DUMMY

Programmers who knew JCL were reluctant to transition to UNIX because their arcane knowledge would no longer be critical.

#### Sample UNIX transcript (type this in yourself)

Here is a transcript of most of these in action.

    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ cd
    bash-4.2$ pwd
    /home/accts/sbs5
    bash-4.2$ cd cs201/www
    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ mkdir test
    bash-4.2$ ls
    >>> cs201\_web\_root <<<	Fall\_2015  index.html  Spring\_2016  style.css  test  UNIX.html
    bash-4.2$ mv test larry
    bash-4.2$ ls
    >>> cs201\_web\_root <<<	Fall\_2015  index.html  larry  Spring\_2016  style.css  UNIX.html
    bash-4.2$ ls -l
    total 36
    -rw-r--r--  1 sbs5    cs201ta    0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta 4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta 2673 Jan  1 15:33 index.html
    drwxrwsr-x  2 sbs5    cs201ta 4096 Jan 18 10:35 larry
    drwxrwsr-x  5 sbs5    cs201ta 4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta  460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 8634 Jan 18 10:33 UNIX.html
    bash-4.2$ ls -a
    .  ..  >>> cs201\_web\_root <<<  Fall\_2015  .htaccess  index.html  larry	Spring\_2016  style.css	UNIX.html
    bash-4.2$ ls -al
    total 48
    drwxrwsr-x  6 sbs5    cs201ta 4096 Jan 18 10:36 .
    drwxrwsr-x 76 root    faculty 4096 Sep 13 15:06 ..
    -rw-r--r--  1 sbs5    cs201ta    0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta 4096 Dec  9 16:54 Fall\_2015
    -rw-rw-r--  1 sbs5    cs201ta   53 Nov 20  2001 .htaccess
    -rw-r--r--  1 sbs5    cs201ta 2673 Jan  1 15:33 index.html
    drwxrwsr-x  2 sbs5    cs201ta 4096 Jan 18 10:35 larry
    drwxrwsr-x  5 sbs5    cs201ta 4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta  460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 8634 Jan 18 10:33 UNIX.html
    bash-4.2$ cp style.css new.css
    bash-4.2$ ls -l
    total 40
    -rw-r--r--  1 sbs5    cs201ta    0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta 4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta 2673 Jan  1 15:33 index.html
    drwxrwsr-x  2 sbs5    cs201ta 4096 Jan 18 10:35 larry
    -rw-rw-r--  1 sbs5    cs201ta  460 Jan 18 10:36 new.css
    drwxrwsr-x  5 sbs5    cs201ta 4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta  460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 8634 Jan 18 10:33 UNIX.html
    bash-4.2$ rm new.css 
    bash-4.2$ ls
    >>> cs201\_web\_root <<<	Fall\_2015  index.html  larry  Spring\_2016  style.css  UNIX.html
    bash-4.2$ rmdir larry
    bash-4.2$ ls
    >>> cs201\_web\_root <<<	Fall\_2015  index.html  Spring\_2016  style.css  UNIX.html
    bash-4.2$ cat style.css
    
    pre {
    	background: #ddd;
    	border: 1px solid #000;
    	padding: 1px;
    	margin-left: 50px;
    	margin-right: 150px;
    	display: block;
    }
    
    blockquote {
    	background: #dfd;
    	border: 1px solid #000;
    	padding: 1px;
    	margin-left: 50px;
    	margin-right: 150px;
    	display: block;
    }
    
    .lecture {
    	display: table-row;
    }
    
    .date, .contents {
    	display: table-cell;
    	padding: 2px;
    }
    
    .date {
    	font-weight: bold;
    }
    
    .contents h3 {
    	font-size: 100%;
    	font-weight: bold;
    	margin-top: 0px;
    }
    bash-4.2$ head style.css 
    
    pre {
    	background: #ddd;
    	border: 1px solid #000;
    	padding: 1px;
    	margin-left: 50px;
    	margin-right: 150px;
    	display: block;
    }
    
    bash-4.2$ tail style.css 
    
    .date {
    	font-weight: bold;
    }
    
    .contents h3 {
    	font-size: 100%;
    	font-weight: bold;
    	margin-top: 0px;
    }
    bash-4.2$ 

Most of these commands are available as systems commands in programming languages

UNIX Command | [Python (import os module)](https://docs.python.org/2/library/os.html) | [Racket](https://docs.racket-lang.org/reference/os.html)
-|-|-
pwd | os.getcwd() | (current-directory)
cd | os.chdir(path) | (current-directory path)
chmod | os.chmod(path,mode) | (file-or-directory-permissions path mode)
mkdir | os.mkdir(path\[,mode\]) | (make-directory path)
mv | os.rename(source, destination) | (rename-file-or-directory old new)
ls | os.listdir(path) | (directory-list path)
rm | os.remove(path) | (delete-file path)
rmdir | os.rmdir(path) | (delete-directory path)
(any command) | os.system(command) | (system command)

## UNIX Principle 2: input/output is a character stream.

*   Using ASCII or Unicode (UTF-8) most data is read and written as a stream of characters.
    
*   Input is read from a conceptual standard input or stdin. stdin defaults to the terminal, but can be redirected, for example with "<" on the command line. Details: standard input is implemented as [file descriptor](https://en.wikipedia.org/wiki/File_descriptor) 0, or fd 0. You can redirect with "0<".
    
*   Output is written to a conceptual standard output or stdout. stdout defaults to the terminal, but can be redirected, for example with ">" on the command line. Details: standard output is implemented as [file descriptor](https://en.wikipedia.org/wiki/File_descriptor) 1, or fd 1. You can redirect with "1>".
    
*   Error messages are written to a conceptual standard error output or stderr. stderr defaults to the terminal (or stdout), but can be redirected, for example, with "&>" on the command line (which also redirects standard output.) Details: standard error is implemented as [file descriptor](https://en.wikipedia.org/wiki/File_descriptor) 2, or fd 2. You can redirect with "2>".
    
*   You can plug the output of one process into the input of another process using pipes, with "|" on the command line.

Here are some useful operators and commands related to standard i/o.

command | description
-|-
wc | Word count. Return number of lines, words, and characters: `wc filename`
echo | Echo. Copy arguments to standard output: `echo hello world > newfile`
< or 0< | Redirect standard input: `cat < filename` (same as `cat filename`)
\> or 1> | Redirect standard output: `cat < filename > newfile` (same as `cp filename newfile`)
\>> | Append to standard output: `cat < filename >> oldfile`
2> | Redirect standard error: `cd /foo 2> newfile`
&> or \>& | Redirect standard output and standard error: `cd /foo &> newfile`
\| | Pipe standard output of lefthand side to standard input of righthand side: `ls \| wc`
tee | Reads standard input and writes it to both standard output and one or more files, effectively duplicating its input. It is primarily used in conjunction with pipes and filters. `ls | tee ls.output | wc`
\* | Wildcard for command line. Match all files: `wc \*`
? | Single character wildcard for command line. Match all characters at that position: `ls \*.htm?`
sort | Sort input.
uniq | Remove duplicates from input, which must be adjacent. Use -c to add count of occurrences.
tr | Translate or delete characters. (-d for delete, -s for squeeze, -c for complement): `ls \| tr A-Z a-z`, `ls \| tr -d A-Z`, `tr -sc a-zA-Z '\n' < UNIX.html \| sort \| uniq -c \| head`, The last example is useful in calculating word frequencies for computational linguistics.
    
    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ ls
    >>> cs201_web_root <<<	Fall\_2015  index.html  Spring\_2016  style.css  UNIX.html
    bash-4.2$ wc UNIX.html 
      417  2124 14651 UNIX.html
    bash-4.2$ echo hello world
    hello world
    bash-4.2$ echo hello world > world
    bash-4.2$ cat world
    hello world
    bash-4.2$ wc world
     1  2 12 world
    bash-4.2$ echo hello world | wc
          1       2      12
    bash-4.4$ ls --help | tee ls.help | head
    Usage: ls [OPTION]... [FILE]...
    List information about the FILEs (the current directory by default).
    Sort entries alphabetically if none of -cftuvSUX nor --sort is specified.
    
    Mandatory arguments to long options are mandatory for short options too.
      -a, --all                  do not ignore entries starting with .
      -A, --almost-all           do not list implied . and ..
          --author               with -l, print the author of each file
      -b, --escape               print C-style escapes for nongraphic characters
          --block-size=SIZE      scale sizes by SIZE before printing them; e.g.,
    bash-4.4$ head ls.help
    Usage: ls [OPTION]... [FILE]...
    List information about the FILEs (the current directory by default).
    Sort entries alphabetically if none of -cftuvSUX nor --sort is specified.
    
    Mandatory arguments to long options are mandatory for short options too.
      -a, --all                  do not ignore entries starting with .
      -A, --almost-all           do not list implied . and ..
          --author               with -l, print the author of each file
      -b, --escape               print C-style escapes for nongraphic characters
          --block-size=SIZE      scale sizes by SIZE before printing them; e.g.,
    bash-4.2$ wc \
          0       0       0 >>> cs201_web_root <<<
    wc: Fall_2015: Is a directory
          0       0       0 Fall_2015
        124     249    2673 index.html
    wc: Spring_2016: Is a directory
          0       0       0 Spring_2016
         37      62     460 style.css
        438    2191   15187 UNIX.html
          1       2      12 world
        600    2504   18332 total
    bash-4.2$ wc *.html
      124   249  2673 index.html
      438  2191 15187 UNIX.html
      562  2440 17860 total
    bash-4.2$ wc ?orld
     1  2 12 world
    bash-4.2$ ls *.???
    style.css
    bash-4.2$ ls *.????
    index.html  UNIX.html  

**Danger: be careful using \* with rm command.** Consider rm \*.bak vs rm \* .bak The former deletes all backup files. The latter deletes all files. It is safer to use rm -i version of rm which enforces interactive confirmation of each deletion.

## UNIX Principle 3: There are many ways to find out stuff about UNIX.

Here are some useful operators and commands that help you explore the world of UNIX.

command | description
-|-
diff | Difference. Find differences, if any, between two files: `diff file1 file2`
grep | Generate regular expression and print. Search using regular expressions. `ls | grep "html"`
file | File type. Determine type of file: `file ..`
\--help | Help command. In addition to the man command (above), most UNIX commands support the \--help command line argument to give a succinct synopsis of the command: `man --help`
whoami | Who am I? Return the netid of the user: `whoami`
id | Identify. Return the various integer ids associated with the user: `id`
uptime | Uptime. How long has the system been running since last reboot: `uptime`
who | Who is currently logged in? `who` (Also, `who -r` to show what they are running.)
w | What are users running? `w`
last | When did users last access the system? `last` (defaults to reboot. Use `last -n` to view most recent n users.
uname | What flavor of UNIX are we running? `uname`
lsb\_release | What release of Linux are we running? `lsb_release`
du | Disk usage. How much disk space is being used under this directory? `du`
quota | Disk quota. How much disk space am I allowed? `quota`
free | Display amount of free and used memory in the system. `free`
finger | The finger command displays information about the system users. `finger sbs5`
info | The info command displays information on most of the core UNIX commands using the emacs editor. If you use emacs, you should use info. `info ls`

    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ ls
    >>> cs201_web_root <<< Fall_2015  index.html  Spring_2016  style.css  UNIX.html  world
    bash-4.2$ cat world
    hello world
    bash-4.2$ echo Hello World > World
    bash-4.2$ ls | grep html
    index.html
    UNIX.html
    bash-4.2$ diff world world
    bash-4.2$ diff world World 
    1c1
    < hello world
    ---
    > Hello World
    bash-4.2$ file world
    world: ASCII text
    bash-4.2$ file *
    >>> cs201_web_root <<<: empty
    Fall_2015:              setgid directory
    index.html:             HTML document, ASCII text
    Spring_2016:            setgid directory
    style.css:              ASCII text
    UNIX.html:              Python script, ASCII text executable
    world:                  ASCII text
    World:                  ASCII text
    bash-4.2$ ls -l
    total 44
    -rw-r--r--  1 sbs5    cs201ta     0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta  4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta  2673 Jan  1 15:33 index.html
    drwxrwsr-x  5 sbs5    cs201ta  4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta   460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 16204 Jan 18 11:57 UNIX.html
    -rw-rw-r--  1 sbs5    cs201ta    12 Jan 18 11:45 world
    -rw-rw-r--  1 sbs5    cs201ta    12 Jan 18 11:57 World
    bash-4.2$ file --help
    Usage: file [OPTION...] [FILE...]
    Determine type of FILEs.
    
          --help                 display this help and exit
      -v, --version              output version information and exit
      -m, --magic-file LIST      use LIST as a colon-separated list of magic
                                   number files
      -z, --uncompress           try to look inside compressed files
      -b, --brief                do not prepend filenames to output lines
      -c, --checking-printout    print the parsed form of the magic file, use in
                                   conjunction with -m to debug a new magic file
                                   before installing it
      -e, --exclude TEST         exclude TEST from the list of test to be
                                   performed for file. Valid tests are:
                                   ascii, apptype, compress, elf, soft, tar, tokens, troff
      -f, --files-from FILE      read the filenames to be examined from FILE
      -F, --separator STRING     use string as separator instead of \`:'
      -i, --mime                 output MIME type strings (--mime-type and
                                   --mime-encoding)
          --apple                output the Apple CREATOR/TYPE
          --mime-type            output the MIME type
          --mime-encoding        output the MIME encoding
      -k, --keep-going           don't stop at the first match
      -l, --list                 list magic strength
      -L, --dereference          follow symlinks (default)
      -h, --no-dereference       don't follow symlinks
      -n, --no-buffer            do not buffer output
      -N, --no-pad               do not pad output
      -0, --print0               terminate filenames with ASCII NUL
      -p, --preserve-date        preserve access times on files
      -r, --raw                  don't translate unprintable chars to \\ooo
      -s, --special-files        treat special (block/char devices) files as
                                 ordinary ones
      -C, --compile              compile file specified by -m
      -d, --debug                print debugging messages
    
    Report bugs to http://bugs.gw.com/
    bash-4.2$ whoami
    sbs5
    bash-4.2$ id
    uid=37645(sbs5) gid=26038(sbs5) groups=26038(sbs5),11760(cs458),31955(zookeep),49258(cs458ta),63505(cs201ta)
    bash-4.2$ uptime
     11:59:14 up 2 days, 20:40,  1 user,  load average: 0.00, 0.01, 0.05
    bash-4.2$ who
    sbs5     pts/0        2016-01-17 11:01 (akw410.cs.yale.internal)
    bash-4.2$ w
     11:59:19 up 2 days, 20:40,  1 user,  load average: 0.00, 0.01, 0.05
    USER     TTY        LOGIN@   IDLE   JCPU   PCPU WHAT
    sbs5     pts/0     Sun11   21:43   3.23s  0.00s sshd: sbs5 \[priv\]   
    bash-4.2$ who -r
             run-level 5  2016-01-15 15:24
    bash-4.2$ last -10
    mps57    pts/2        ip-64-134-177-19 Mon Jan 18 10:54 - 10:56  (00:01)    
    mb2669   pts/1        vpn172022122007. Mon Jan 18 01:31 - 03:46  (02:14)    
    sbs5     pts/0        akw410.cs.yale.i Sun Jan 17 11:01   still logged in   
    pg438    pts/0        c-67-186-59-170. Fri Jan 15 15:23 - 15:24  (00:00)    
    reboot   system boot  3.10.0-123.8.1.e Fri Jan 15 15:18 - 11:59 (2+20:40)   
    db692    pts/0        backebergs-mbp.w Fri Jan 15 15:16 - 15:16  (00:00)    
    pg438    pts/0        c-67-186-59-170. Thu Jan 14 22:23 - 22:24  (00:00)    
    rg462    pts/0        eric.cs.yale.int Thu Jan 14 12:49 - 16:38  (03:48)    
    rjh27    pts/0        rebeccas-mbp.wir Tue Jan 12 17:56 - 17:56  (00:00)    
    mps57    pts/1        mce2336d0.tmodns Tue Jan 12 16:23 - 18:38  (02:15)    
    
    wtmp begins Sat Oct 17 03:23:11 2015
    bash-4.2$ uname
    Linux
    bash-4.2$ lsb\_release
    bash: lsb\_release: command not found
    bash-4.2$ du
    ... lots of output ...
    168896	.
    bash-4.2$ quota
    Disk quotas for user sbs5 (uid 37645): 
         Filesystem  blocks   quota   limit   grace   files   quota   limit   grace
    artemis.zoo.cs.yale.edu:/home
                     224428       0 5000000            4773       0       0        
    bash-5.0$ finger sbs5
    Login: sbs5           			Name: Slade Stephen
    Directory: /home/accts/sbs5         	Shell: /bin/bash
    On since Wed Nov  6 08:32 (EST) on pts/0 from 172.28.229.190
       1 day 15 hours idle
    Mail last read Tue Aug 15 10:21 2017 (EDT)
    Plan:
    This is my plan (in ~/.plan) .
    bash-5.0$ free
                  total        used        free      shared  buff/cache   available
    Mem:       65761688     3364052    53018576      428696     9379060    61238124
    Swap:      67108860           0    67108860

## UNIX Principle 4: Security exists at the owner, group, and world levels

You may recall that the ls -l produced an initial column of output containing dashes, r, w, and x, as well as the third and fourth columns which looked like netids.

    bash-4.2$ ls -l
    total 36
    -rw-r--r--  1 sbs5    cs201ta    0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta 4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta 2673 Jan  1 15:33 index.html
    drwxrwsr-x  2 sbs5    cs201ta 4096 Jan 18 10:35 larry
    drwxrwsr-x  5 sbs5    cs201ta 4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta  460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 8634 Jan 18 10:33 UNIX.html

The first column for each file or directory listed indicates if the item is a directory, and what access rights belong to the owner, the group, and the world. There are three triplets of rights rwx indicating read, write, or execute permission. (s is a setuid permission, allowing executing to pretend it is another user.) The third column is the id of the owner (sbs5) and the fourth column is the id of the group (cs201ta). World rights belong to everyone. There is no need explicitly to indicate that role.

In the above example for UNIX.html (this document), the owner, sbs5, has both read and write privileges. Everyone else has just read rights. No one else can modify this file.

The three triplets of rights can be interpreted as a 9-bit binary number. A 1 indicates the presence of a right and a 0 indicates no right. Thus

`rw-r--r--`  is the same as

`110100100`

Moreover, each triplet of binary numbers can itself be interpreted as an octal (base 8) number.

`110100100` is the same as

`6   4   4`   in octal

The following commands are used to change permissions. Note: you need the proper rights to change permissions. Don't be surprised if you are not allowed to use some commands.

command | description
-|-
chmod | Change / modify permission. Change the access rights for a file or directory: `chmod 755 filename` makes the rights `rwxr-xr-x` giving the world read and execute rights, but not write permission. For those of you who don't like octal, `chmod` has symbolic options. Look at the man page.
chown | Change owner. Change the owner for a file or directory: `chown`
chgrp | Change group. Change the group for a file or directory: `chgrp`
getent | get entries from Name Service Switch libraries. See [man getent](http://man7.org/linux/man-pages/man1/getent.1.html) (many options). `getent group cs201ta`

Here are some feeble examples.

    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ ls -l
    total 52
    -rw-r--r--  1 sbs5    cs201ta     0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta  4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta  2673 Jan  1 15:33 index.html
    drwxrwsr-x  5 sbs5    cs201ta  4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta   460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 23764 Jan 18 12:23 UNIX.html
    -rw-rw-r--  1 sbs5    cs201ta    12 Jan 18 11:45 world
    -rw-rw-r--  1 sbs5    cs201ta    12 Jan 18 11:57 World
    bash-4.2$ chmod 755 world
    bash-4.2$ ls -l world 
    -rwxr-xr-x 1 sbs5 cs201ta 12 Jan 18 11:45 world
    bash-4.2$ chmod 600 world
    bash-4.2$ ls -l world 
    -rw------- 1 sbs5 cs201ta 12 Jan 18 11:45 world
    bash-4.2$ chgrp faculty world
    chgrp: changing group of ‘world’: Operation not permitted
    bash-4.2$ chown faculty world
    chown: invalid user: ‘faculty’
    bash-4.4$ getent group cs201ta
    cs201ta:*:63505:angluin,sbs5,crb84,hr77,swk2
    bash-4.4$ 

Below is another interpretation of UNIX permissions.

![Moral Alignment Matrix for Unix Permissions](https://zoo.cs.yale.edu/classes/cs201/moralunixpermissions.jpg)

## UNIX Principle 5: UNIX has a robust command processor and associated environment variables

The command processor for UNIX is known as the shell. It comes in different flavors.

| command | description |
|---------|-------------|
| sh | The original shell, also known as the Bourne shell, for its writer, Steve Bourne. |
| csh | The C shell, (get it), written by Bill Joy for BSD UNIX. It was written in C. |
| bash | The Born Again Shell, (get it), written by the GNU guys. This is what commonly used, including on the zoo |
| chsh | Change shell. The command for you to change your default shell. |
| set | View or change environment variables. There are lots of them. Try `set | less` |
| PATH | The search path for commands. When you type command at the prompt, UNIX first sees if it is a built in command, like `cd`. If not, it sees if it is an executable, fully qualified path name, such as `/usr/bin/ls`. Otherwise, it assumes that it is an executable file that resides in one of your PATH list of directories. It searches that list, left to right, until it finds the command or fails. `echo $PATH` prints out the value of your PATH variable. You may append to the PATH variable this way: `PATH=$PATH:/usr/local/special/bin` |
| which | Searches your PATH to find directory containing the specified command, or reports failure. `which ls` should return `/usr/bin/ls` |
| alias | Defines an alias command, e.g., `alias ll='ls -l'` lets you type `ll` instead of `ls -l` Typing `alias` without any arguments gives you a list of your current alias definitions. You can define your standard aliases in your `~/.bashrc` file in your home directory, aka, `~/.` |
| type | Displays information about command type. It displays if command is an alias, shell function, shell builtin, disk file, or shell reserved word. Also may show if command path has been hashed (memoized). `type cd` should return `cd is a shell builtin` Builtin commands include `cd`, `source`, `set`, `alias`, process control commands (`jobs`, `fg`, `bg`, `wait` - see principle 6 below), and `type` itself. |
| apropos | What commands are like the given string? If you cannot remember the precise command name, but know what it does, you can use `apropos` to get a list of related commands. `apropos directory` You may notice the numbers in parentheses for each item, e.g., (1), (5). Those numbers refer to the section of the online manual pages, which are organized as follows: 1. General commands 2. System calls 3. Library functions, covering in particular the C standard library 4. Special files (usually devices, those found in /dev) and drivers 5. File formats and conventions 6. Games and screensavers 7. Miscellanea 8. System administration commands and daemons |
| HOME | The user's home directory. `echo $HOME` |
| PWD | The user's present working directory. `echo $PWD` |
| OLDPWD | Is the value of PWD before the most recent `cd` command. `cd -` should change your working directory to its previous value, e.g., OLDPWD. Typing `cd -` twice takes you back where you started. |
| RANDOM | A random number in the range of 0 - 32,767. `echo $RANDOM` |
| SHELL | The user's shell. `echo $SHELL` |
| USER | The user's netid. `echo $USER` |
| UID | The user's numeric user id. `echo $UID` |
| export | Define your own environment variable. `TODAY="Monday"` You may then use `export` to allow the variable to be accessed by future processes. `export TODAY` |
| unset | Remove the environment variable binding. `unset TODAY` Do not try this with system variables. |

Here are some examples.

    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ set | wc
         80     101    3794
    bash-4.2$ echo $PATH
    /usr/lib64/qt-3.3/bin:/home/accts/sbs5/perl5/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin
    bash-4.2$ which ls
    /usr/bin/ls
    bash-4.2$ which which
    /usr/bin/which
    bash-4.2$ apropos apropos
    apropos (1)          - search the manual page names and descriptions
    bash-4.2$ apropos python
    abrt-action-analyze-python (1) - Calculate and save UUID and duplicate hash for a problem data directory DIR with Pytho...
    abrt-python (5)      - abrt-python Documentation
    abrt-python.conf (5) - Configuration file for ABRT's python crash hook
    f2py (1)             - Fortran to Python interface generator
    nosetests (1)        - Nicer testing for Python
    perf-script-python (1) - Process trace data with a Python script
    python (1)           - an interpreted, interactive, object-oriented programming language
    python2 (1)          - an interpreted, interactive, object-oriented programming language
    python2.7 (1)        - an interpreted, interactive, object-oriented programming language
    python3.4 (1)        - an interpreted, interactive, object-oriented programming language
    python\_event.conf (5) - configuration file for libreport.
    bash-4.2$ apropos racket
    drracket (1)         - the Racket programming environment
    gracket (1)          - the GUI Racket implementation
    mred (1)             - compatibility executable for GRacket
    mzc (1)              - compatibility Racket compiler tool
    mzscheme (1)         - compatibility executable for Racket
    plt-help (1)         - compatibility Racket documentation tool
    racket (1)           - core Racket implementation
    raco (1)             - the RAcket COmmand-line tool
    setup-plt (1)        - compatibility Racket setup tool
    bash-4.2$ which racket
    /usr/local/bin/racket
    bash-4.2$ echo $HOME
    /home/accts/sbs5
    bash-4.2$ echo $PWD
    /home/accts/sbs5/cs201/www
    bash-4.2$ echo $SHELL
    /bin/bash
    bash-4.2$ echo $USER
    sbs5
    bash-4.2$ echo $UID
    37645
    bash-4.2$ TODAY=Monday
    bash-4.2$ echo $TODAY
    Monday
    bash-4.2$ unset TODAY
    bash-4.2$ echo $TODAY
    
    bash-4.2$ X=\`whoami\`
    bash-4.2$ echo $X
    sbs5
    bash-4.2$ XX=$(whoami)
    bash-4.2$ echo $XX
    sbs5
    bash-4.2$ R=$RANDOM
    bash-4.2$ echo $R
    2497
    bash-4.2$ echo $(($R%10))
    7
    bash-4.4$ type cd source set alias ps jobs fg bg wait
    cd is a shell builtin
    source is a shell builtin
    set is a shell builtin
    alias is a shell builtin
    ps is /usr/bin/ps
    jobs is a shell builtin
    fg is a shell builtin
    bg is a shell builtin
    wait is a shell builtin
    bash-4.4$ type ls
    ls is /usr/bin/ls
    bash-4.4$ ls
    2014-lectures.html  lectures		       materials		UNIX.html~
    announcements.html  lectures.html	       old\_index.html		zoo-annex
    assignments.html    lectures.old.html	       Racket-style-guide.html
    contact.html	    lectures.Spring\_2014.html  syllabus.html
    faq.html	    make-index.py	       UNIX.html
    bash-4.4$ type ls
    ls is hashed (/usr/bin/ls)
    bash-4.4$ cd
    bash-4.4$ pwd
    /home/accts/sbs5
    bash-4.4$ echo $OLDPWD
    /home/accts/sbs5/cs201/www
    bash-4.4$ cd -
    /home/accts/sbs5/cs201/www
    bash-4.4$ pwd
    /home/accts/sbs5/cs201/www

Note the little trick of enclosing a UNIX command in backticks (\`) which results in **executing** the UNIX command, here whoami. The $() syntax is preferred. For one thing, it can be nested, unlike the backticks.

You can evaluate an expression in bash using the expr command, using double parens, or using square brackets, as shown below.
    
    bash-4.4$ R=$RANDOM
    bash-4.4$ echo $R
    89
    bash-4.4$ expr $R % 20
    9
    bash-4.4$ echo $((R % 20))
    9
    bash-4.4$ echo $\[R % 20\]
    9

### Shell Scripts

If there are commands that you regularly use, you can save them in a file and execute them indirectly, like a batch (.BAT) file in Windows. These files are known as _shell scripts_.

For example, the file `script` prints out the current date and time and then lists first 10 times the USER accessed this machine per the log files.

```bash
date
last | grep $USER | tail
```
    
    bash-4.2$ cat script
    date
    last | grep $USER | tail
    bash-4.2$ source script
    bash: source: /usr/bin/script: cannot execute binary file
    bash-4.2$ source ./script
    Mon Jan 18 13:42:03 EST 2016
    sbs5     :0                            Mon Nov  2 12:38 - 12:41  (00:02)    
    sbs5     pts/1        akw410.cs.yale.i Mon Nov  2 09:59 - 15:47  (05:48)    
    sbs5     pts/2        acrphhw3dhnjm1.c Fri Oct 30 16:32 - 16:41  (00:08)    
    sbs5     pts/4        akw410.cs.yale.i Fri Oct 30 13:49 - crash  (20:45)    
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 25 14:31 - crash (1+23:45)   
    sbs5     pts/1        mobile-107-107-5 Sun Oct 25 09:13 - 09:30  (00:16)    
    sbs5     pts/1        c-65-96-74-176.h Sun Oct 25 08:16 - 08:28  (00:12)    
    sbs5     pts/0        akw410.cs.yale.i Wed Oct 21 11:51 - 14:20 (4+02:28)   
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 18 11:44 - 13:19 (1+01:35)   
    sbs5     pts/0        akw410.cs.yale.i Sat Oct 17 16:22 - 10:20  (17:57)    
    bash-4.4$ cat script2
    date
    whoami
    bash-4.4$ source script2
    Sun May 20 18:47:40 EDT 2018
    sbs5
    bash-4.4$ . script2
    Sun May 20 18:47:46 EDT 2018
    sbs5
    bash-4.4$ type .
    . is a shell builtin

The `source` command tells the shell to execute the commands in the file as if the user had typed them at the terminal. (Note "." is a synonym for source. This may be confusing, since . is also the name of the current directory.)

We can convert this shell script into an executable command by specifying the interpreter in the first line following the "shebang" (#!) sequence. In this case, the interpreter is bash itself.

    bash-4.2$ cat script
    #! /usr/bin/bash 
    date
    uptime
    last | grep $USER | tail
    bash-4.2$ ./script
    bash: ./script: Permission denied
    bash-4.2$ ls -l script
    -rw-rw-r-- 1 sbs5 cs201ta 55 Jan 18 13:47 script
    bash-4.2$ chmod 755 script
    bash-4.2$ ./script
    Mon Jan 18 13:48:42 EST 2016
     13:48:42 up 2 days, 22:30,  1 user,  load average: 0.00, 0.01, 0.05
    sbs5     :0                            Mon Nov  2 12:38 - 12:41  (00:02)    
    sbs5     pts/1        akw410.cs.yale.i Mon Nov  2 09:59 - 15:47  (05:48)    
    sbs5     pts/2        acrphhw3dhnjm1.c Fri Oct 30 16:32 - 16:41  (00:08)    
    sbs5     pts/4        akw410.cs.yale.i Fri Oct 30 13:49 - crash  (20:45)    
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 25 14:31 - crash (1+23:45)   
    sbs5     pts/1        mobile-107-107-5 Sun Oct 25 09:13 - 09:30  (00:16)    
    sbs5     pts/1        c-65-96-74-176.h Sun Oct 25 08:16 - 08:28  (00:12)    
    sbs5     pts/0        akw410.cs.yale.i Wed Oct 21 11:51 - 14:20 (4+02:28)   
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 18 11:44 - 13:19 (1+01:35)   
    sbs5     pts/0        akw410.cs.yale.i Sat Oct 17 16:22 - 10:20  (17:57)    
    
    We can make our shell scripts less dependent on the directory structure by using the env program to find the executable for bash.
    
    bash-4.2$ cat script
    #! /usr/bin/env bash 
    date
    uptime
    last | grep $USER | tail
    bash-4.2$ ./script
    Mon Jan 18 13:49:47 EST 2016
     13:49:47 up 2 days, 22:31,  1 user,  load average: 0.00, 0.01, 0.05
    sbs5     :0                            Mon Nov  2 12:38 - 12:41  (00:02)    
    sbs5     pts/1        akw410.cs.yale.i Mon Nov  2 09:59 - 15:47  (05:48)    
    sbs5     pts/2        acrphhw3dhnjm1.c Fri Oct 30 16:32 - 16:41  (00:08)    
    sbs5     pts/4        akw410.cs.yale.i Fri Oct 30 13:49 - crash  (20:45)    
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 25 14:31 - crash (1+23:45)   
    sbs5     pts/1        mobile-107-107-5 Sun Oct 25 09:13 - 09:30  (00:16)    
    sbs5     pts/1        c-65-96-74-176.h Sun Oct 25 08:16 - 08:28  (00:12)    
    sbs5     pts/0        akw410.cs.yale.i Wed Oct 21 11:51 - 14:20 (4+02:28)   
    sbs5     pts/0        akw410.cs.yale.i Sun Oct 18 11:44 - 13:19 (1+01:35)   
    sbs5     pts/0        akw410.cs.yale.i Sat Oct 17 16:22 - 10:20  (17:57)    

[Click here for more shell script examples](http://zoo.cs.yale.edu/classes/cs200/lectures/sh/)

More details at [Introduction to BASH Programming](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)

Whenever you login or create a new shell process, bash executes the commands in the special shell script: `~/.bashrc`, assuming it exists. One very useful trick is to create custom command names using the alias command, demonstrated below.

    bash-4.2$ cat aliases
    alias ll="ls -l"
    alias rm="rm -i"
    alias now=date
    alias me=whoami
    bash-4.2$ source aliases
    bash-4.2$ alias
    alias ll='ls -l'
    alias me='whoami'
    alias now='date'
    alias rm='rm -i'
    bash-4.2$ me
    sbs5
    bash-4.2$ now
    Mon Jan 18 13:59:42 EST 2016
    bash-4.2$ ll
    total 100
    -rw-rw-r--  1 sbs5    cs201ta    66 Jan 18 13:59 aliases
    -rw-r--r--  1 sbs5    cs201ta     0 Jan 10  1997 >>> cs201\_web\_root <<<
    drwxrwsr-x  5 sbs5    cs201ta  4096 Dec  9 16:54 Fall\_2015
    -rw-r--r--  1 sbs5    cs201ta  2673 Jan  1 15:33 index.html
    -rwxr-xr-x  1 sbs5    cs201ta    59 Jan 18 13:49 script
    -rwxr-xr-x  1 sbs5    cs201ta    55 Jan 18 13:47 script~
    drwxrwsr-x  5 sbs5    cs201ta  4096 Jan 15 13:19 Spring\_2016
    -rw-rw-r--  1 sbs5    cs201ta   460 Jan 13  2015 style.css
    -rw-r--r--  1 sbs5    cs201ta 34617 Jan 18 13:59 UNIX.html
    bash-4.2$ rm aliases
    rm: remove regular file ‘aliases’? n

ll is the same as "ls -l". rm now is safer, requiring interactive confirmation. now invokes the data command and me calls whoami. If you place these alias commands in your ~/.bashrc file, they will be available to you at all times.

![](https://imgs.xkcd.com/comics/tar.png)

There are few more useful UNIX and bash commands.

command | description
-|-
tar | Tape archive. Create or extract a tar ball. `tar -xf foo.tar`
gzip | Compress a file, including tar files. `gzip foo.tar`
gunzip | Uncompress a zip file. `gunzip foo.tar.gz`
find | Find a file in the directory tree. It has lots of options. `find . -perm 644`
history | List the command history. `history 10`
!! | Execute the previous command. `!!`
!-3 | Execute the third previous command. `!-3`
!grep | Execute the most recent previous grep command. `!grep`

    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www
    bash-4.2$ mkdir x
    bash-4.2$ cd x
    bash-4.2$ ls .. > one
    bash-4.2$ cp one two
    bash-4.2$ cp two three
    bash-4.2$ ls
    one  three  two
    bash-4.2$ tar -cf xxx.tar *
    bash-4.2$ ls
    one  three  two  xxx.tar
    bash-4.2$ gzip xxx.tar 
    bash-4.2$ ls
    one  three  two  xxx.tar.gz
    bash-4.2$ mkdir y
    bash-4.2$ cp xxx.tar.gz y
    bash-4.2$ cd y
    bash-4.2$ ls
    xxx.tar.gz
    bash-4.2$ gunzip xxx.tar.gz 
    bash-4.2$ ls
    xxx.tar
    bash-4.2$ tar -tfv xxx.tar 
    -rw-rw-r-- sbs5/cs201ta    100 2016-01-18 19:19 one
    -rw-rw-r-- sbs5/cs201ta    100 2016-01-18 19:20 three
    -rw-rw-r-- sbs5/cs201ta    100 2016-01-18 19:20 two
    bash-4.2$ ls
    xxx.tar
    bash-4.2$ tar -xf xxx.tar 
    bash-4.2$ ls
    one  three  two  xxx.tar
    bash-4.2$ cd ..
    bash-4.2$ ls
    one  three  two  xxx.tar.gz  y
    bash-4.2$ pwd
    /home/accts/sbs5/cs201/www/x
    bash-4.2$ ls
    one  three  two  xxx.tar.gz  y
    bash-4.2$ find . one
    .
    ./three
    ./two
    ./y
    ./y/three
    ./y/two
    ./y/one
    ./y/xxx.tar
    ./xxx.tar.gz
    ./one
    one
    bash-4.2$ find .
    .
    ./three
    ./two
    ./y
    ./y/three
    ./y/two
    ./y/one
    ./y/xxx.tar
    ./xxx.tar.gz
    ./one
    bash-4.2$ find . -name one
    ./y/one
    ./one
    bash-4.2$ find . -perm 644
    bash-4.2$ ls -l
    total 20
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:19 one
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:20 three
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:20 two
    -rw-rw-r-- 1 sbs5 cs201ta  261 Jan 18 19:21 xxx.tar.gz
    drwxrwsr-x 2 sbs5 cs201ta 4096 Jan 18 19:23 y
    bash-4.2$ find -perm 664
    ./three
    ./two
    ./y/three
    ./y/two
    ./y/one
    ./y/xxx.tar
    ./xxx.tar.gz
    ./one
    bash-4.2$ !!
    find -perm 664
    ./three
    ./two
    ./y/three
    ./y/two
    ./y/one
    ./y/xxx.tar
    ./xxx.tar.gz
    ./one
    bash-4.2$ !ls
    ls -l
    total 20
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:19 one
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:20 three
    -rw-rw-r-- 1 sbs5 cs201ta  100 Jan 18 19:20 two
    -rw-rw-r-- 1 sbs5 cs201ta  261 Jan 18 19:21 xxx.tar.gz
    drwxrwsr-x 2 sbs5 cs201ta 4096 Jan 18 19:23 y
    bash-4.2$ !-4
    find . -perm 644
    bash-
    bash-4.2$ history 10
     1035  ls
     1036  find . one
     1037  find .
     1038  find . -name one
     1039  find . -perm 644
     1040  ls -l
     1041  find -perm 664
     1042  ls -l
     1043  find . -perm 644
     1044  history 10

## UNIX Principle 6: UNIX provides support for process control

Every time you issue a command, that is not a built in bash command like cd, UNIX creates a new, separate child process in which to execute the command. Normally, the process will read from standard input and write to standard output -- the terminal in both cases. However, it is possible to run processes in the background, while you execute some other command in the foreground. UNIX and bash provide commands and operators to make this easier.

command | description
-|-
sleep | Do nothing for specified time. Just wait. `sleep 100`
& | Execute the command in the background. `sleep 100 &`
ps | Process status. What is going on. `ps`
jobs | Job status. What is going on. `jobs`
fg | Foreground. Move specified job from background to the foreground. `fg %1`
bg | Background. Move specified job from foreground to the background. `bg %1`
^Z | Suspend foreground job (control-Z). `^Z`
kill | Kill the specified job. `kill %1`
top | Real time process status. Uses curses terminal graphics. `top`

    bash-4.2$ ps
      PID TTY          TIME CMD
    11561 pts/1    00:00:00 bash
    11562 pts/1    00:00:00 ps
    bash-4.2$ sleep 100 &
    \[1\] 11563
    bash-4.2$ ps
      PID TTY          TIME CMD
    11561 pts/1    00:00:00 bash
    11563 pts/1    00:00:00 sleep
    11564 pts/1    00:00:00 ps
    bash-4.2$ jobs
    \[1\]+  Running                 sleep 100 &
    bash-4.2$ sleep 50 &
    \[2\] 11567
    bash-4.2$ ps
      PID TTY          TIME CMD
    11561 pts/1    00:00:00 bash
    11563 pts/1    00:00:00 sleep
    11567 pts/1    00:00:00 sleep
    11568 pts/1    00:00:00 ps
    bash-4.2$ jobs
    \[1\]-  Running                 sleep 100 &
    \[2\]+  Running                 sleep 50 &
    bash-4.2$ kill %2
    bash-4.2$ jobs
    \[1\]-  Running                 sleep 100 &
    \[2\]+  Terminated              sleep 50
    bash-4.2$ ps
      PID TTY          TIME CMD
    11561 pts/1    00:00:00 bash
    11563 pts/1    00:00:00 sleep
    11569 pts/1    00:00:00 ps
    bash-4.2$ jobs
    \[1\]+  Running                 sleep 100 &
    bash-4.2$ fg %1
    sleep 100

![](https://imgs.xkcd.com/comics/surgery.png)