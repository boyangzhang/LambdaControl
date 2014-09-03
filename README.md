Lambda Control 
============== 

Team 
----
- [Grace Wang](mailto:wangrace@seas.upenn.edu)
- [Dan Zhang](mailto:zhangb@seas.upenn.edu)


Description 
-----------

Lambda Control (LC) is a basic implementation of a version control system in
Haskell. LC allows diffing between versions, logging of version history, and
reverting to a previous version. It is currently limited to a single branch
with local functionality (i.e. no remotes).

Lambda Control also does not pack objects like Git does, so the objects
directory may grow to a large size after many commits.


Usage 
-----

In addition to the standard Haskell distribution, the following packages must 
be installed in order to use Lambda Control:

- diff
- MissingH
- SHA
- strict

Run 'make' in order to create an executable named lc. The following commands
are now available:

- ./lc init
- ./lc add [filename]
- ./lc commit "commit message"
- ./lc log
- ./lc diff [commit hash 1] [commit hash 2]
- ./lc revert [commit hash]


Architecture 
------------

### Object

This module contains the definitions of all datatypes used in Lambda Control,
as well as associated functionality such as representing each datatype in a
string form and compressing and rebuilding objects from disk.

#### Datatypes

We based our datatype representation heavily off of [Git's
implementation](http://git-scm.com/book/en/Git-Internals-Git-Objects). In
particular, files are represented by blobs and directories are represented by
trees. `Tree`s contain one or more `TreeEntry`s, each of which is a wrapper for
another `Blob` or `Tree`. Tree entries contain a SHA-1 pointer to a blob or
subtree, as well as metadata about the object (e.g. name of the directory or
file).

The last datatype is the `Commit` type, which is essentially a wrapper for a
`Tree` object. The `Commit` datatype also contains additional information such
as the parent commit and the message associated with the commit.

#### String Representations

Defining a well-formed string representation for each object type was critical
to being able to repeatedly encode and decode data to and from disk. For
convenience, we defined this string representation slightly differently from
how Git hashes
[blobs](http://alblue.bandlem.com/2011/08/git-tip-of-week-objects.html),
[trees](http://alblue.bandlem.com/2011/08/git-tip-of-week-trees.html) and
[commits](http://alblue.bandlem.com/2011/09/git-tip-of-week-commits.html):

- `Blob`: hashed from "blob\ncontent" 
    - e.g. A blob representing a file with contents "hello world" would be
      represented as 
      ```
      blob 
      hello world
      ```
- `Tree`: hashed from "tree\n", followed by a series of lines, each
  corresponding to a `TreeEntry`
    - e.g. A tree representing a directory with two files `foo` and `bar` would
      be represented as 
      ```
      tree 
      100644 blob [hash] foo 
      100644 blob [hash] bar
      ```
- `Commit`: hashed from the following representation: 
    ```
    commit
    tree [treeHash]
    parent [hash of parent commit] 
    message [commit message]
    ```


### LCIO

Contains basic constants and functions used in storing LC metadata on disk.

### Main

This module contains the primary logic for our version control system. Each
command available to users maps to a function in this module:

- `init`: initializes metadata files and directories in .lc
- `add`: 
    - constructs the current staging area (represented by a `Tree`) from
      .lc/staging
    - adds the specified file to be added to the staging tree
    - encodes updated staging area back to .lc/staging
    - encodes the specified file to .lc/objects
- `commit`: takes changes and pushes it to filesystem
    - constructs a `Commit` object from the staging area (decoded from
      .lc/staging)
    - encodes the commit to .lc/objects
    - updates the head commit in .lc/head
- `log`: 
    - reads the head commit from .lc/head
    - finds the corresponding commit from the .lc/objects folder and finds the
      parent commit
    - repeats for the specified number of commits (default 10)
- `diff`:
    - decodes the trees corresponding to the two specified commits
    - finds the specified file in those trees
    - uses diffing functionality provided in Data.Algorithm.Diff
- `revert`: 
    - deletes all files in the current staging area
    - creates all files in the tree corresponding to the specified commit

The staging area is stored as a normal tree in the objects directory; the hash
of this tree is stored in .lc/staging.


Additional Libraries 
--------------------
- Code.Compression.Zlib
- Digest.Digest.Pure.SHA 
- Data.Algorithm.Diff and Data.Algorithm.DiffOutput
- Data.Bytestring.Lazy.Char8
- System.Directory and System.Environment 
- IO monad and System.IO.Strict 


Testing 
-------

### HUnit

### QuickCheck 

Because our version control system relies so heavily on proper encoding and
decoding, we chose to test this functionality via QuickCheck. In order to get
this working properly, we had to use Test.QuickCheck.Monadic in order to run IO
operations in within QuickCheck.
