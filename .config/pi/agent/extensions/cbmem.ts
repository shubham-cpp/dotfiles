// Pi extension for codebase-memory-mcp.
//
// The installer stub registered tools with only `name` and `run`, omitting
// `parameters`. OpenAI-compatible providers reject that with 422:
//   tools[N]: missing field `parameters`
// This file takes ownership of the extension (installer markers removed)
// and registers the MCP tool schemas the server actually serves.
import { spawn } from "node:child_process";
import { Type } from "typebox";

const BIN = "/home/shubham/.local/bin/codebase-memory-mcp";

const TOOLS = [
  {
    "name": "index_repository",
    "label": "Index repository",
    "description": "Index a repository into the knowledge graph. Special mode 'cross-repo-intelligence': skip extraction, only match Routes/Channels across projects to create CROSS_HTTP_CALLS/CROSS_ASYNC_CALLS/CROSS_CHANNEL edges. Requires target_projects param. Ensure target projects have fresh indexes first. COVERAGE: the response reports files that were NOT fully indexed \u2014 'skipped' (not indexed at all: oversized/read/parse failures) and 'parse_partial' (indexed, but constructs inside the listed line ranges could not be parsed and MAY be missing from the graph). The embedded lists carry counts plus a FEW EXAMPLES only; the complete lists are in the per-run 'logfile' (path in the response) and queryable any time via index_status or structurally via query_graph(graph=\"missed\"). Both signals are best-effort: absence of a flag is NOT a completeness guarantee; prefer grep inside flagged ranges. Separately, 'excluded' + 'not_indexed_files' list what was deliberately NOT indexed (gitignore/.cbmignore/skip-lists) \u2014 by design, not failures.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "repo_path": {
          "type": "string",
          "description": "Path to the repository"
        },
        "mode": {
          "type": "string",
          "enum": [
            "full",
            "moderate",
            "fast",
            "cross-repo-intelligence"
          ],
          "default": "full",
          "description": "All modes run type-aware LSP call/usage resolution (per-file + cross-file). full: all files + similarity/semantic edges. moderate: filtered files + similarity/semantic. fast: filtered files, no similarity/semantic. cross-repo-intelligence: match Routes/Channels across projects."
        },
        "target_projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Projects to search for cross-repo links (cross-repo-intelligence mode). Use [\"*\"] for all indexed projects. Run list_projects to see available projects."
        },
        "name": {
          "type": "string",
          "description": "Override the derived project name. Non-ASCII bytes are encoded and unsafe path characters are normalized."
        },
        "persistence": {
          "type": "boolean",
          "default": false,
          "description": "Write compressed artifact to .codebase-memory/graph.db.zst for team sharing. Teammates can bootstrap from the artifact instead of full re-indexing."
        }
      },
      "required": [
        "repo_path"
      ]
    }
  },
  {
    "name": "search_graph",
    "label": "Search graph",
    "description": "Search the code knowledge graph for functions, classes, routes, and variables. Use INSTEAD OF grep/glob when finding code definitions, implementations, or relationships. Three search modes: (1) query='update settings' for BM25 ranked full-text search with camelCase splitting and structural label boosting \u2014 recommended for natural-language discovery; (2) name_pattern='.*regex.*' for exact pattern matching; (3) semantic_query=[...] for vector cosine search that bridges vocabulary (finds 'publish' when you search 'send'). The three modes are independent and can be combined in a single call. RESPONSE: prefix-grouped tree rows by default \u2014 a shared (qn-prefix, file) group header printed once, then `name label lines in out` per row (full qn = group prefix + dot + name). in/out = selected degree across CALLS, USAGE, CALL_REFERENCE, INHERITS, and IMPLEMENTS; other edge types are excluded. These are NOT caller/callee counts \u2014 use trace_path for callers. Add per-node property columns via fields (e.g. [\"complexity\",\"signature\",\"docstring\"]); format=\"json\" returns the SAME tree model as structured JSON. PAGINATION: results are capped at limit (default 50). The response always includes 'total' (full match count before limit) and 'has_more' (true when total > offset+returned). Detect truncation with has_more, then page by re-calling with offset=offset+limit until has_more is false. Narrow first via label/file_pattern/min_degree before paginating large result sets.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "query": {
          "type": "string",
          "description": "Natural-language or keyword full-text search using BM25 ranking. Tokens are split on whitespace; camelCase identifiers are indexed as individual words (updateCloudClient \u2192 update, cloud, client). Results are ranked with structural boosting: Functions/Methods +10, Routes +8, Classes/Interfaces +5. Noise labels (File/Folder/Module/Variable) are filtered out. When provided, name_pattern is ignored."
        },
        "label": {
          "type": "string"
        },
        "name_pattern": {
          "type": "string"
        },
        "qn_pattern": {
          "type": "string"
        },
        "file_pattern": {
          "type": "string"
        },
        "relationship": {
          "type": "string"
        },
        "min_degree": {
          "type": "integer"
        },
        "max_degree": {
          "type": "integer"
        },
        "exclude_entry_points": {
          "type": "boolean"
        },
        "include_connected": {
          "type": "boolean"
        },
        "semantic_query": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "MUST be an ARRAY of keyword strings (e.g. [\"send\",\"pubsub\",\"publish\"]) \u2014 NOT a single string. Each keyword is scored independently via per-keyword min-cosine; results reflect functions that score well on ALL keywords. Requires moderate/full index mode. Results appear in the 'semantic_results' field (separate from 'results')."
        },
        "limit": {
          "type": "integer",
          "description": "Max results per call. Default 50. Response carries 'total' (full match count) and 'has_more' (true if truncated) so callers can detect the limit and paginate."
        },
        "offset": {
          "type": "integer",
          "default": 0,
          "description": "Skip the first N matching nodes. Combine with 'limit' to page: increment offset by limit and re-call while has_more is true."
        },
        "format": {
          "type": "string",
          "enum": [
            "tree",
            "json"
          ],
          "default": "tree",
          "description": "Response encoding. tree (default): prefix-grouped text rows. json: the SAME tree model as structured JSON (groups + column-ordered row arrays)."
        },
        "fields": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Extra per-node property columns, e.g. complexity, cognitive, signature, docstring, return_type, is_test, lines(int). Core row columns (qn/label/file/lines/in/out) are always present \u2014 do not request them here. Missing values emit as empty cells."
        },
        "detail": {
          "type": "string",
          "enum": [
            "ids",
            "default"
          ],
          "default": "default",
          "description": "ids: bare qualified-name enumeration (one column) \u2014 cheapest form for wide sweeps where per-row metadata is noise. default: full rows."
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "query_graph",
    "label": "Query graph",
    "description": "Execute a Cypher query against the knowledge graph for complex multi-hop patterns, aggregations, and cross-service analysis. The response includes 'total' (returned row count). There is a hard 100k row ceiling \u2014 for broad queries add LIMIT in the Cypher itself or use search_graph + offset/limit pagination instead. COMPLEXITY / BOTTLENECKS: every Function and Method node carries queryable complexity properties \u2014 cyclomatic (complexity), cognitive, loop_count, loop_depth (max nested-loop depth, a polynomial-degree proxy), plus interprocedural transitive_loop_depth (worst-case nested-loop degree propagated along CALLS edges) and a recursive flag. Additional hot-path signals: linear_scan_in_loop (count of find/contains/indexOf-style scans inside a loop \u2014 the hidden O(n^2) that loop_depth misses), alloc_in_loop (allocations/appends inside a loop), recursion_in_loop (a self-call inside a loop), unguarded_recursion (recursion with no conditionally-guarded base case), param_count and max_access_depth (structure smells). Find all hot-path candidates in one query, e.g. MATCH (f:Function) WHERE f.transitive_loop_depth >= 3 OR f.linear_scan_in_loop >= 1 RETURN f.qualified_name, f.transitive_loop_depth, f.linear_scan_in_loop ORDER BY f.transitive_loop_depth DESC. MISSED GRAPH: pass graph=\"missed\" to query the best-effort miss graph instead \u2014 the file structure of ONLY the files the indexer could NOT fully index (Project \u2192 Folder \u2192 File nodes with CONTAINS_FOLDER/CONTAINS_FILE edges; each File carries kind (\"parse_partial\" = indexed but constructs in the flagged line ranges MAY be missing; or a skip phase) and detail (the line ranges / reason)). Example: MATCH (f:File) WHERE f.kind = \\\"parse_partial\\\" RETURN f.file_path, f.detail. Absence from this graph is NOT a completeness guarantee.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "query": {
          "type": "string",
          "description": "Cypher query"
        },
        "project": {
          "type": "string"
        },
        "graph": {
          "type": "string",
          "enum": [
            "code",
            "missed"
          ],
          "default": "code",
          "description": "Which graph to query: the code knowledge graph (default) or the missed graph (only files not fully indexed, laid out as their file structure)."
        },
        "max_rows": {
          "type": "integer",
          "description": "Optional row limit. Default: unlimited up to a 100k row ceiling. No offset support \u2014 use search_graph for paginated browsing."
        }
      },
      "required": [
        "query",
        "project"
      ]
    }
  },
  {
    "name": "trace_path",
    "label": "Trace path",
    "description": "Trace paths through the code graph. Modes: calls (callers/callees), data_flow (value propagation with args at each hop), cross_service (through HTTP/async Route nodes). Use INSTEAD OF grep for callers, dependencies, impact analysis, or data flow tracing. RESPONSE: prefix-grouped tree rows \u2014 callees/callers grouped under their shared qn-prefix, `name hop` per row (full qn = group prefix + dot + name); exact callees_total/callers_total on every page = ALL nodes reachable within depth (transitive, not just direct; test files excluded unless include_tests). risk/args flags use a flat table. `truncated: true` + `next` = more rows \u2014 pass next back as cursor. format=\"json\" returns the SAME tree model as structured JSON.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "function_name": {
          "type": "string"
        },
        "project": {
          "type": "string"
        },
        "direction": {
          "type": "string",
          "enum": [
            "inbound",
            "outbound",
            "both"
          ],
          "default": "both"
        },
        "depth": {
          "type": "integer",
          "default": 3
        },
        "limit": {
          "type": "integer",
          "default": 100,
          "minimum": 1,
          "maximum": 5000,
          "description": "Rows per page. callees_total/callers_total always carry the exact full counts; when a page is truncated the response carries next \u2014 see cursor."
        },
        "cursor": {
          "type": "string",
          "description": "Resume token from a previous response's 'next' field. Pass it back with ALL other arguments identical to get the following page with no duplicates. Cursors outlive nothing: after a reindex you get a stale_cursor error \u2014 just re-run the original query."
        },
        "mode": {
          "type": "string",
          "enum": [
            "calls",
            "data_flow",
            "cross_service"
          ],
          "default": "calls",
          "description": "calls: follow CALLS edges. data_flow: follow CALLS+DATA_FLOWS with arg expressions. cross_service: follow HTTP_CALLS+ASYNC_CALLS+DATA_FLOWS through Routes, plus CROSS_* cross-repo edges (CROSS_HTTP_CALLS/ASYNC_CALLS/CHANNEL/GRPC_CALLS/GRAPHQL_CALLS/TRPC_CALLS) to hop into other services."
        },
        "parameter_name": {
          "type": "string",
          "description": "For data_flow mode: scope trace to a specific parameter name"
        },
        "edge_types": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "risk_labels": {
          "type": "boolean",
          "default": false,
          "description": "Add risk classification (CRITICAL/HIGH/MEDIUM/LOW) based on hop distance"
        },
        "include_tests": {
          "type": "boolean",
          "default": false,
          "description": "Include test files in results. When false (default), test files are filtered out. When true, test nodes are included with a test column/marker."
        },
        "format": {
          "type": "string",
          "enum": [
            "tree",
            "json"
          ],
          "default": "tree",
          "description": "Response encoding. tree (default): prefix-grouped text rows. json: the SAME tree model as structured JSON (groups + column-ordered row arrays)."
        },
        "include_evidence": {
          "type": "boolean",
          "default": false,
          "description": "Add how each hop was resolved: a strategy class (lsp | language_rule | heuristic | unresolved) and the resolver's confidence. Off by default \u2014 it adds two columns per row. Use it to judge whether an edge is trustworthy, not to find edges."
        }
      },
      "required": [
        "function_name",
        "project"
      ]
    }
  },
  {
    "name": "get_code_snippet",
    "label": "Get code snippet",
    "description": "Read source code for a function/class/symbol. IMPORTANT: First call search_graph to find the exact qualified_name, then pass it here. This is a read tool, not a search tool. Accepts full qualified_name (exact match) or short function name (returns suggestions if ambiguous). If the response carries a 'coverage_note', the file was only partially indexed \u2014 constructs in the noted line ranges may be missing from the graph (best-effort signal); prefer grep there and treat the returned source as ground truth.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "qualified_name": {
          "type": "string",
          "description": "Full qualified_name from search_graph, or short function name"
        },
        "project": {
          "type": "string"
        },
        "include_neighbors": {
          "type": "boolean",
          "default": false
        }
      },
      "required": [
        "qualified_name",
        "project"
      ]
    }
  },
  {
    "name": "get_graph_schema",
    "label": "Get graph schema",
    "description": "Get the schema of the knowledge graph (node labels, edge types)",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "get_architecture",
    "label": "Get architecture",
    "description": "Get high-level architecture overview. DEFAULT (no aspects) is a compact summary \u2014 overview counts, languages, packages, entry_points; request more via aspects:[...] (structure, dependencies, routes, hotspots, boundaries, layers, clusters, file_tree) or [\"all\"]. 'clusters' runs Leiden community detection over the call/import graph, surfacing the de-facto modules (label, member count, cohesion score, representative top_nodes, binding packages/edge_types) \u2014 the real architectural seams, which often cut across the folder layout. Optional path scopes analysis to nodes under that directory prefix (file_path).",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "path": {
          "type": "string",
          "description": "Optional directory prefix to scope architecture (e.g. apps/hoa)"
        },
        "aspects": {
          "type": "array",
          "items": {
            "type": "string",
            "enum": [
              "all",
              "overview",
              "structure",
              "dependencies",
              "routes",
              "languages",
              "packages",
              "entry_points",
              "hotspots",
              "boundaries",
              "layers",
              "file_tree",
              "clusters",
              "cycles"
            ]
          },
          "description": "Aspects to include. 'all' = everything; 'overview' = compact summary (all except file_tree); omit = all. 'cycles' is opt-in ONLY (never via all/overview): it scans the whole call graph for circular CALLS dependencies (SCCs of size > 1)."
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "search_code",
    "label": "Search code",
    "description": "Graph-augmented code search. Finds text patterns via grep, then enriches results with the knowledge graph: deduplicates matches into containing functions, ranks by structural importance (definitions first, popular functions next, tests last). Modes: compact (default, signatures only \u2014 token efficient), full (source capped at a 60-line window around the first match per hit; source_truncated marks the cut \u2014 use get_code_snippet for the complete symbol), files (just file paths). Use path_filter regex to scope results. TRUNCATION: enriched results are capped at limit (default 10). Response carries 'total_grep_matches' (raw grep hit count) and 'total_results' (deduplicated function count) \u2014 compare to limit to detect truncation. There is no offset parameter; to see more, raise limit or narrow the query with file_pattern / path_filter.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "pattern": {
          "type": "string"
        },
        "project": {
          "type": "string"
        },
        "file_pattern": {
          "type": "string",
          "description": "Glob for grep --include (e.g. *.go)"
        },
        "path_filter": {
          "type": "string",
          "description": "Regex filter on result file paths (e.g. ^src/ or \\.(go|ts)$)"
        },
        "mode": {
          "type": "string",
          "enum": [
            "compact",
            "full",
            "files"
          ],
          "default": "compact",
          "description": "compact: signatures+metadata (default). full: with source. files: just file list."
        },
        "context": {
          "type": "integer",
          "description": "Lines of context around each match (like grep -C). Only used in compact mode."
        },
        "regex": {
          "type": "boolean",
          "default": false
        },
        "debug": {
          "type": "boolean",
          "default": false,
          "description": "Include scope_ms, scan_ms, and enrich_ms phase timing diagnostics."
        },
        "limit": {
          "type": "integer",
          "description": "Max enriched results per call. Default 10. Response includes 'total_grep_matches' and 'total_results' so callers can detect truncation. No offset parameter \u2014 raise limit or narrow with file_pattern / path_filter to see more.",
          "default": 10,
          "minimum": 1
        }
      },
      "required": [
        "pattern",
        "project"
      ]
    }
  },
  {
    "name": "list_projects",
    "label": "List projects",
    "description": "List indexed projects with deterministic pagination",
    "inputSchema": {
      "type": "object",
      "properties": {
        "offset": {
          "type": "integer",
          "minimum": 0,
          "default": 0
        },
        "limit": {
          "type": "integer",
          "minimum": 1,
          "maximum": 100,
          "default": 50
        },
        "include_details": {
          "type": "boolean",
          "default": false,
          "description": "Include branch, node/edge counts and database size. Slower."
        },
        "metadata_only": {
          "type": "boolean",
          "description": "Deprecated compatibility alias for include_details=false."
        }
      }
    }
  },
  {
    "name": "delete_project",
    "label": "Delete project",
    "description": "Delete a project from the index",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "index_status",
    "label": "Index status",
    "description": "Get the indexing status of a project: node/edge counts, root path, git context, and the indexing-COVERAGE report \u2014 which files the indexer could NOT fully cover (best-effort signal): 'parse_partial' files WERE indexed but contain line ranges tree-sitter could not parse \u2014 constructs there MAY be missing from the graph (some are still recovered); 'skipped' files were not indexed at all (oversized/read/parse failure). Use this before trusting graph completeness on a file: if a file is listed, ALSO grep it (especially the flagged ranges). IMPORTANT: absence from these lists is NOT a completeness guarantee \u2014 the signal only marks what the indexer can detect. For structural queries over the misses use query_graph(graph=\"missed\"). The report also carries 'not_indexed' \u2014 files/dirs excluded BY DESIGN (gitignore/.cbmignore/skip-lists): deliberate and deterministic, not failures; change the ignore rules and re-index to include them.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "verbose": {
          "type": "boolean",
          "default": false,
          "description": "Include the git context block (worktree/shadow path variants). Only needed when debugging where an index lives \u2014 omitted by default to keep the status lean."
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "check_index_coverage",
    "label": "Check index coverage",
    "description": "Check authoritative indexing-coverage metadata for exact repository-relative paths and bounded path scopes. Use this after graph discovery for every cited or operated-on file; use scopes before negative/exhaustive claims because fully skipped files cannot appear in normal graph results. Returns coverage status separately from filesystem metadata freshness, plus structured parse-error ranges and direct-source fallback actions. The signal is best-effort: indexed_no_recorded_gap is not a completeness guarantee. At least one of 'paths' or 'scopes' is required; the call is rejected at runtime if both are omitted.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "paths": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "maxItems": 128,
          "description": "Repository-relative files to check exactly. Required if 'scopes' is omitted."
        },
        "scopes": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "maxItems": 32,
          "description": "Repository-relative path prefixes; use . for the project root. Required if 'paths' is omitted."
        },
        "scope_limit": {
          "type": "integer",
          "default": 200,
          "minimum": 1,
          "maximum": 1000
        },
        "scope_offset": {
          "type": "integer",
          "default": 0,
          "minimum": 0
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "detect_changes",
    "label": "Detect changes",
    "description": "Map a git diff to its BLAST RADIUS. Resolves changed files to the symbols they define, then runs ONE multi-source graph traversal to the transitive impact set. RESPONSE: base + merge_base SHA, changed_files list, then impacted = prefix-grouped tree rows (name label hop; full qn = group prefix + dot + name) + an impacted_modules rollup; impacted_total + truncated are exact. Seeds (the changed symbols) are excluded from impacted; a changed file reached from another changed file is not counted as extra impact. format=\"json\" returns the same model as structured JSON.",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "scope": {
          "type": "string",
          "enum": [
            "files",
            "impact"
          ],
          "description": "files: changed files only (no traversal). impact (default): files + the transitive impact set."
        },
        "direction": {
          "type": "string",
          "enum": [
            "inbound",
            "outbound",
            "both"
          ],
          "default": "inbound",
          "description": "inbound (default) = the blast radius: transitive CALLERS of the changed symbols. outbound = what the changed code depends on. both = union."
        },
        "depth": {
          "type": "integer",
          "default": 2,
          "description": "Max traversal hops from the changed symbols."
        },
        "limit": {
          "type": "integer",
          "default": 200,
          "maximum": 5000,
          "description": "Per-symbol impacted rows shown (nearest hops first). impacted_total is always exact and the impacted_modules rollup always complete regardless."
        },
        "base_branch": {
          "type": "string",
          "default": "main"
        },
        "since": {
          "type": "string",
          "description": "Git ref or tag to compare from (e.g. HEAD~5, v0.5.0). Diffs <ref>...HEAD."
        },
        "format": {
          "type": "string",
          "enum": [
            "tree",
            "json"
          ],
          "default": "tree"
        }
      },
      "required": [
        "project"
      ]
    }
  },
  {
    "name": "manage_adr",
    "label": "Manage adr",
    "description": "Create or update Architecture Decision Records",
    "inputSchema": {
      "type": "object",
      "properties": {
        "project": {
          "type": "string"
        },
        "mode": {
          "type": "string",
          "enum": [
            "get",
            "update",
            "sections"
          ],
          "description": "update replaces the entire ADR document; sections only lists existing headings"
        },
        "content": {
          "type": "string",
          "description": "Complete replacement document required by update"
        }
      },
      "required": [
        "project"
      ],
      "additionalProperties": false
    }
  },
  {
    "name": "ingest_traces",
    "label": "Ingest traces",
    "description": "Ingest runtime traces to enhance the knowledge graph",
    "inputSchema": {
      "type": "object",
      "properties": {
        "traces": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "caller": {
                "type": "string"
              },
              "callee": {
                "type": "string"
              },
              "count": {
                "type": "integer"
              }
            },
            "additionalProperties": false
          }
        },
        "project": {
          "type": "string"
        }
      },
      "required": [
        "traces",
        "project"
      ]
    }
  }
] as const;

function toParameters(schema: unknown) {
  const normalized =
    schema && typeof schema === "object" && !Array.isArray(schema)
      ? { type: "object", properties: {}, ...(schema as Record<string, unknown>) }
      : { type: "object", properties: {} };
  const unsafe = (Type as { Unsafe?: (value: unknown) => unknown }).Unsafe;
  return typeof unsafe === "function" ? unsafe(normalized) : normalized;
}

function parseCliOutput(out: string): unknown {
  const trimmed = out.trim();
  if (!trimmed) return { error: "empty response from codebase-memory-mcp" };
  try {
    return JSON.parse(trimmed);
  } catch {
    /* logs mixed into stdout, or 0.10+ tree text without --json */
  }
  const lines = trimmed.split("\n").map((l) => l.trim()).filter(Boolean);
  for (let i = lines.length - 1; i >= 0; i--) {
    try {
      return JSON.parse(lines[i]);
    } catch {
      /* keep scanning */
    }
  }
  // Tree/table text is still valid tool output — do not fail the call.
  return { content: [{ type: "text", text: trimmed }], isError: false };
}

async function call(tool: string, args: unknown, signal?: AbortSignal) {
  return new Promise((resolve) => {
    // 0.10+ CLI prints tree/table text by default. --json wraps the same
    // payload in an MCP {content,isError} envelope, which toToolResult
    // already unwraps. Args go on stdin: a positional JSON blob is deprecated.
    const child = spawn(BIN, ["cli", "--json", tool], {
      stdio: ["pipe", "pipe", "pipe"],
      env: { ...process.env, CBM_LOG_LEVEL: "error" },
    });
    let out = "";
    const onAbort = () => {
      if (!child.killed) child.kill();
    };
    signal?.addEventListener("abort", onAbort, { once: true });
    child.stdout.on("data", (d) => (out += d.toString()));
    child.stdin.on("error", () => {
      /* process may exit before stdin is written */
    });
    child.stdin.end(JSON.stringify(args ?? {}));
    child.on("error", (e) => {
      signal?.removeEventListener("abort", onAbort);
      resolve({ error: String(e && e.message ? e.message : e) });
    });
    child.on("close", () => {
      signal?.removeEventListener("abort", onAbort);
      resolve(parseCliOutput(out));
    });
  });
}

function textFrom(result: unknown): string {
  if (typeof result === "string") return result;
  try {
    return JSON.stringify(result);
  } catch {
    return String(result);
  }
}

function toToolResult(result: unknown) {
  if (result && typeof result === "object") {
    const rec = result as {
      error?: unknown;
      isError?: boolean;
      content?: unknown;
      structuredContent?: unknown;
    };
    if (rec.error && rec.content == null) {
      throw new Error(textFrom(rec.error));
    }
    if (Array.isArray(rec.content)) {
      if (rec.isError) {
        const first = rec.content.find((part) => part && typeof part === "object" && "text" in part) as
          | { text?: unknown }
          | undefined;
        throw new Error(typeof first?.text === "string" ? first.text : textFrom(result));
      }
      return {
        content: rec.content,
        details: rec.structuredContent,
      };
    }
  }
  return { content: [{ type: "text" as const, text: textFrom(result) }] };
}

export default function (pi: { registerTool: (tool: unknown) => unknown }) {
  for (const tool of TOOLS) {
    pi.registerTool({
      name: tool.name,
      label: tool.label,
      description: tool.description,
      parameters: toParameters(tool.inputSchema),
      async execute(_toolCallId: string, args: unknown, signal?: AbortSignal) {
        return toToolResult(await call(tool.name, args, signal));
      },
    });
  }
}
