# Helper scripts

Build the production Go helpers with `mise run build` from the configuration root. The shell runs the compiled commands in `.local/bin`; it does not invoke `go run`, mise, or a Python interpreter for runtime helpers.

The Python helper implementations remain here as pre-migration references for the existing regression tests and rollback. They are not the production commands. New behavior belongs in `cmd/` and `internal/`, with matching Go tests. `tests/reference/` contains the frozen JavaScript search oracle.

The emoji data builder is now `.local/bin/qs-build-emoji --check --output data`. Python remains a development dependency for the Qt test runners and reference tests.

`build-lock-auth` still compiles the native PAM helper. See [the migration report](../research/23-go-migration.md) for the process contracts, dependencies, validation and rollback instructions.
