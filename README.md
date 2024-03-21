# The B2Scala tool

## Description

The B2Scala tool is an incarnation in Scala of a Linda-like language,
called Bach, developed at the University of Namur. It consists of a
Domain Specific Language, internal to Scala, that allows to experiment
with programs developed in Bach while benefiting from the Scala ecosystem,
in particular from its type system as well as program fragments
developed in Scala. Moreover, a Hennessy-Milner like logic is used
to restrict the executions of programs to those meeting logic
formulae. The tool is illustrated on the Needham-Schroeder security
protocol, for which we manage to rediscover the man-in-the-middle
attack first put in evidence by G. Lowe.

More information on the B2Scala tool can be found in the companion
paper ``the_b2scala_tool.pdf``, available on this repository.

A video explaining the use of the tool is available on YouTube at the
following address:

[https://youtu.be/G6b9rQLEXiM](https://youtu.be/G6b9rQLEXiM)

The source code is available from this github repository.

Moreover, a docker container (together with instructions to use it) is also
available from this github


## Package containing the program

An sbt project with the B2Scala code and a program example is contained
in the source code directory under the subdirectory
b2scala_with_NS_program.  In its ``src/main/scala`` directory, it
contains various subdirectories for the B2Scala tool but also a
directory ``bsc_program``, which contains the
``needham_schroeder.scala`` file as an example of a file to be
executed with the tool. This file contains the code for the
Needham-Schroeder protocol described in the joined article. Other
programs may be introduced in other files in this directory. Please
make sure that you rename the ``BSC_modelling`` object and declare
such an object in your new program.

Concretely, save this subdirectory on your computer, enter it 
at the level of the ``build.sbt`` file and execute in sequence

- ``sbt compile``
- ``sbt run``

The code in the ``needham_schroeder.scala`` file should execute and deliver
the behavior described in the article contained in ``the_b2scala_tool.pdf``.

Further tests can be made by modifying this file at will. A tutorial
on how to do that is given below.


## Example package with B2Scala as a library

A simple Scala project that uses B2Scala as a library is included in
the subdirectory b2scala_as_a_library_ex_of_use. The tool is in the
``lib`` folder as a jar file. The program is this time in the
directory ``src/main/scala/my_program``.

Concretely, save this subdirectory on your computer, enter it
at the level of the ``build.sbt`` file and execute in sequence

- ``sbt compile``
- ``sbt run``

The code in the ``needham_schroeder.scala`` file should execute and deliver
the behavior described in the article. Again, further tests can be made by
modifying this file at will.


## Requirements

Running the sbt projects requires to have installed Scala and its tool
sbt. We have used Scala version 2.12, sbt version 1.3 and Java
version 1.8. However the code does not use very specific features and
should run on later versions. To that end, note that the versions should be
changed in the ``build.sbt`` file and the ``build.properties`` file (this latter file being located in the subdirectory ``project``).


## Tutorial

A companion article ``the_b2scala_tool.pdf`` describing the tool is
included in this repository. Moreover the interested reader can find
the modelling of a restaurant application in the tutorial
sub-directory in this repository. It includes a ``tutorial.pdf`` file
explaining how to code and deploy the code together with a scala file
``altar.scala`` containing the code.


## Docker container

A docker container can be found in the ``DockerContainer``
subdirectory of this repository.

## Contact

Should you need any further information or would you like to report
bugs, do not hesitate to contact the authors,
D. Ouardi at doha.ouardi@unamur.be,
Manel Barkallah at manel.barkallah@unamur.be,
and J.-M. Jacquet at jean-marie.jacquet@unamur.be.

