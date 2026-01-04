---
title: "Reasonin"
date: 2025-09-05 10:33:51
slug: reasoning-about-submeshes-of-devicemesh
draft: true
categories: [Miscellaneous]
---

*For a primer on device mesh, check out my previous post \`The Parallelism Mesh Zoo \<https://blog.ezyang.com/2025/08/the-parallelism-mesh-zoo/\>\`\_.*

Suppose we have an N-D device mesh and we want to do a communication on a specific axis of the mesh. How exactly does this translate into the actual PGs that the communication will happen on?

The points I want to make:

- What does a PG want? It wants to know the group, it wants to know WHICH group the current node is in
- Given a mesh dim, how to compute the GPUs you will compute with. The fiber concept.
- Submesh: narrowing to the origin
- Easy: 2D. Harder: 3D.

Let's talk about placement sharding - Contract with the rest of the nodes - What about a submesh? - Plain tensor as a degenerate no-mesh

Naughtiness - PyTorch APIs pull out the global mesh, create on the global mesh (because you have more guarantees!) - Cross mesh doesn't actually work

What about the CuTe stuff
