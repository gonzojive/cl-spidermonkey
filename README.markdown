# CL-SpiderMonkey: Common Lisp interface to Javascript

### A Common Lisp library for interacting with Javascript through the SpiderMonkey library

## Introduction

cl-spidermonkey provides a Javascript runtime environment inside of
Common Lisp by embedding a widely-used and tested Javascript engine:
Mozilla's SpiderMonkey.  

With full access to Javascript from Common Lisp, it becomes easier to
test Javascript libraries in the same breath as normal testing.  It
also allows a Lisp REPL to be used as a Javascript REPL, and for many
other combinations of lisp and JS.

## Installation

Before you do anything you need the git repostiory.

    git clone git://github.com/gonzojive/cl-spidermonkey.git

First you need to compile Spidermonkey.  It's not that bad!  Just cd
into the vendor directory and then run the install script:

    cd vendor
    sh install-spidermonkey.sh

That will download and install SpiderMonkey, and set up all the paths
properly.

Now you should be able to load the library in lisp:

    REPL> (asdf:operate 'asdf:load-op :cl-spidermonkey)

To make sure everything's peachy, run the test suite:

    REPL> (asdf:operate 'asdf:load-op :cl-spidermonkey-tests)
    REPL> (spidermonkey-tests:spidermonkey-tests)


## Usage

Right now there are only two exported symbols, so things are pretty
easy:

    REPL> (sm::with-js-context (context)
           (sm:evaluate-js "10 * 24;"))
    240

Note that you can only get doubles, ints, strings, voids (undefined),
nulls, and boolean values back from EVALUATE-JS.  Any other object
will come back as a pointer to a JS_Object whichs needs further
attention from the bindings.  If you are so inclined, lookat the
src/spidermonkey-bindings.lisp file for more info on how to deal with
native Spidermonkey objects.