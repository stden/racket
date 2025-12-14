# CLAUDE.md Structure
- **Build**: Run `make` in the root directory to build the default Racket CS implementation. `make bc` for Racket BC.
- **Run**: Use `./racket/bin/racket` to run the built executable.
- **Test**: Core thread tests are in `racket/src/thread/tests/`. Run with `racket/bin/racket <test_file>`.
- **Lint/Style**: Follow standard Lisp/Scheme conventions. Use `raco fmt` if available, otherwise manual indentation (2 spaces usually).
- **Contribution**: See `racket/src/README.txt` and `build.md` for detailed contribution guidelines.

## Development Workflow
1. Modify source in `racket/src/`.
2. Rebuild with `make` (incremental builds are supported).
3. If modifying core scheduler/threads, double-check `racket/src/thread/README.txt` for atomic/uninterruptible invariants.

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
