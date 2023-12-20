# Command line based text manipulator
This is an command line based APP used to manipulate text files.
## Usage
To start the APP:
```
./text_manipulator.exe
```
To display all available operations inside APP:
```
-help
```
To read a file inside APP:
```
-read <file_path>
```
To create a new file
```
-create <file_path>
```
To show the editing content inside APP:
```
-show <file_path>
```
To replace word <A> by word <B> inside APP:
```
-replaceword <A> <B>
```
To replace character <A> by word <B> inside APP:
```
-replacepart <A> <B>
```
To undo operation inside APP:
```
-undo
```
To search <A> inside APP:
```
-search <A>
```
To insert a new line inside APP:
```
-insertline <line_number>
```
To insert <A> at line x and column y inside APP:
```
-insert <line_number> <col_number> <A>
```
To delete <A> at line x and column y inside APP:
```
-delete <line_number> <col_number> <A>
```
To show diff between file <file_path> with current editing file inside APP:
```
-diff <file_path>
```
To merge all differences between file <file_path> and current editing file inside APP:
```
-mergeall <file_path>
```
To flush all changes of current edited content into file inside APP:
```
-write <file_path>
```
## Test
When running dune test, please run dune build first. Because we are using some txt file in /test folder to test, without a dune build, files will not be moved to _build/default/tests and it will report files does not exist error.
## Reference
https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/

An O(ND) Difference Algorithm and Its Variations∗ EUGENE W. MYERS

File Manipulation · OCAML documentation. (n.d.). OCaml. https://ocaml.org/docs/file-manipulation

OCaml library : Arg. (n.d.). https://v2.ocaml.org/api/Arg.html

OCaml - The toplevel system or REPL (ocaml). (n.d.). https://v2.ocaml.org/manual/toplevel.html



