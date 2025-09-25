# The Timed B2Scala tool

## Description

**Timed B2Scala** is an internal Domain Specific Language embedded in Scala that extends the original B2Scala tool. It implements a **timed variant of the Linda-like language Bach**, developed at the University of Namur.

The tool allows experimenting with Bach programs while leveraging Scala’s type system and programming abstractions. Programs remain Scala-friendly but support Bach control-flow operators:

- **Sequential composition**  
- **Parallel composition**  
- **Non-deterministic choice**  
- **Timed primitives**  

Timed B2Scala introduces a **logic to constrain executions**, enabling users to focus only on program runs that satisfy specific conditions. This feature allows the automatic rediscovery of attacks on protocols.

## Illustrated Protocols

- **Needham-Schroeder protocol** – modeling Alice, Bob, and Mallory  
- **Hancke-Kuhn protocol** – modeling the Verifier, Prover, and attackers  

## Advantages

- Tokens, si-terms, and local variables are typed and verified at compile time  
- Combines Bach abstractions with Scala’s ecosystem  
- Facilitates experimentation with timed protocols and asynchronous communication via a shared space


## Contact

Should you need any further information or would you like to report bugs, do not hesitate to contact the authors, D. Ouardi at doha.ouardi@unamur.be, Manel Barkallah at manel.barkallah@unamur.be, and J.-M. Jacquet at jean-marie.jacquet@unamur.be.
