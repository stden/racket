# Agents Guidelines for Racket Repository

## Primary Agent: Antigravity
- **Role**: AI Coding Assistant for Racket Core
- **Responsibilities**:
  - Explaining internal architecture (threads, scheduler, GC).
  - Creating and running reproduction scripts/tests.
  - Modifying C and Racket source code.
  - Ensuring build integrity via `make`.

## Interaction Rules
- **Atomic Sections**: When modifying `thread.rkt` or `schedule.rkt`, respect `start-atomic` and `end-atomic` blocks. Do not introduce unrestricted blocking calls inside them.
- **Build System**: Always respect the `make` based build flow. Do not try to run `racket` on source files directly without ensuring they are compiled/setup if they depend on the C runtime shifts.
- **Benchmarks**: Use `racket/bin/racket` to run benchmarks. Measure wall-clock time for concurrency tests.

## Codev Methodology

This project uses the Codev context-driven development methodology.

### Active Protocol
- Protocol: SPIDER
- Location: codev/protocols/spider/protocol.md

### Directory Structure
- Specifications: codev/specs/
- Plans: codev/plans/
- Reviews: codev/reviews/
- Resources: codev/resources/

See codev/protocols/spider/protocol.md for full protocol details.
