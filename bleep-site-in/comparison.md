---
sidebar_position: 4
---

# Compared to other build tools

## Sbt

Sbt is in many respects an awesome tool, it really is. But it is the wrong tool.

### it is complex

The case against sbt has been made before, most eloquently by Li Haoyi in the [So, what's wrong with SBT?](https://www.lihaoyi.com/post/SowhatswrongwithSBT.html) blog post. 

### it is slow

Even worse, it is also slow. 
- Starting a JVM is slow. 
- Starting a Scala compiler is slow
- Compiling the build is slow
- Evaluating the build is slow
- Resolving dependencies is slow
- Every plugin introduces more slowness, multiplied by number of projects

### it cannot manage its own build files

The build file is arbitrarily complicated scala code. 
The most common build changes like dependency updates, project renames, combining projects are manual operations.

## Mill

Mill fixes many problems with sbt. It is still slow though, and it cannot manage its own build. 

## Scala-cli

Scala-cli is written with the same tech stack as Bleep. It is simple, it is fast, and it is awesome.

But it's a single-project build tool, so the scope is completely different. 
Bleep is meant to scale to as many projects as you want
