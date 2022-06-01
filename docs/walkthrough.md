# How the Austral Compiler Works

This document is a walkthrough of every stage in the Austral bootstrapping
compiler's pipeline, from source to target.

# Overview

The overall pipeline looks like this:

![Compiler pipeline](pipeline.png)

# Parsing

Every Austral module is divided into two files: the module interface file and
the module body file.

Each one is parsed into a separate CST (concrete syntax tree) by the functions
in the `ParserInterface` module.

# Import Insertion Pass

Each of the module interface and module body are modified to have an implicit
import of all the symbols in the `Austral.Pervasive` module.

# Combining Pass

The combining pass takes the CST for the module interface and for the module
body and combines them into a single representation, where all declarations are
present and have visibility information derived from whether or not they appear
in the module interface.
