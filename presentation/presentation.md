---
title: Shake Before Building
date: 05.07.2017
author: Justus Adam
toc: false
---

# Make, the Applicative

- `make` is the de-facto standard build tool
- used by many large applications
- manages complex build processes

. . .

## Limitations of Make

- Dependencies have to be specified ahead of time
- Complex patterns are hard to express
- Particularily files dynamically generated during compilation
- Example: Header files specified in a C source file
- Dependencies are limited to files

- A typical soultion is to dynamically generate new Makefiles

# Shake it up

- `shake` is a Haskell library
    - Full power of the Haskell language and ecosystem
- Combinators to define high performance build systems
    - Minimal rebuilds
    - Dynamic dependencies
    - Tracing and error reporting

# Dependency specification

## Traditional

- Each item defines a list of dependencies and an action to run
- Build rules are well defined if the constructed dependency graph is finite
- Can be checked without running any action

## Free

- Each item defines an action which either finishes or halts, waiting for dependencies
- Buld rules are defined if the dynamic graph is finite
- Actions must be run to determine the dependencies

# Minimal Builds

- Only run ever rule once
- Only run when dependencies change

. . .

- Cache guarantees no reruns
- Check for modification must be available for all target types
- Database records state at the end of the build

# Post-build State

- Record time
- Record sequential increasing number for build
    - Makes independent from the system modification time of files
- Record time of last change **and** build time (avoids rebuilds)
- Record requested dependencies
- Dependencies are used to check in next build whether rule has to be rebuilt

# Defining Rules

- Uses the `Rules` Monad
- Arbitrary predicate checks against the requested target
    - File patterns
    - Phony rules
- Only one rule may match for each target (except default rules)
- specify default targets using `want`

# Running Actions

- Each action runs in the `Actions` Monad
- Allows request for more dependencies anywhere during execution using `need`
- Allows arbitrary IO and computation

# Custom `Rule`s

- Custom rule types implemented in Haskell
- Must satisfy the `Rule` typeclass
    - Entails the `Binary` class providing serialization for the database
- wrapper functions are used to ensure correct dependency tracking

# Executing the Build

The build state machine is implemented with 6 states ^[Graph taken from the source paper "Shake Before Building", *Mitchell 2012*]

\includegraphics[width=.7\textwidth]{build-state-transition.png}

---

- Failure to build only fails the system if the dependency is still required
- An item is not rebuilt when its dependency is rebuilt without changing

---

- parallel builds are done with a thread pool
    - lower overhead for small/no builds
- and in random order
    - reduces potential resource contention
- extra threads are spawned if builds are waiting for dependencies

# Comparison with `ghc --make`

- Slower than make in single threaded mode
- Exploits parallelism much better
- En par with `make`
- Fast if nothing needs building

\includegraphics[width=0.5\textwidth]{ghc-make-comparison.png} ^[Table taken from the source paper "Shake Before Building", *Mitchell 2012*]

# Miscellaneous features

- Diagnostics, tracing and the build stack
- Journal to resume after error
- profiling tool to find bottlenecks in the build

# Evaluation at Standard Chartered

- Old `make` system 10.000 lines
- Brittle and hard to extend
- New `shake` system 1.000 lines
- Twice as fast (better parallelism since no phase barriers)

---

- Copes with multiple outputs for a single command with a custom `Rule` instance `Files`
- Transitive dependencies can be made more efficient using intermediate files

# Related Work

- Comparable tools also require a database
- All evaluated tools are restricted to files

. . .

## Redo 

- Dynamic dependencies
- Build system changes are tracked
- Non portable

. . .

## Ninja

- Two stage system
- Dynamic but restricted dependencies
- Supports multiple outputs

---

## Tup

- Faster checking for changes with OS support
- Deletes dead build results

. . .

## Fabricate

- Determines dependencies automatically by tracking input files to system commands
- Parallelism must be explicitly stated with grouping

# Example

Surprise, the build script for this presentation is actually implemented with `shake`.

# Questions for the Audience

. . .

How hard was the paper to read?

. . .

Was it too much Haskell?

# Discussion

- Being a library makes it very powerful 
    - full Haskell ecosystem available
    - computations can be done directly in the build system
- A strictly more powerful make, without any loss in performance.
- Type checking for the build script can catch errors before evaluating the script
- Modular implementation because of Haskell (helper functions etc)
- Build system has to be recompiled on each change $\rightarrow$ requires compiler and platform
- Very engineering focused paper

