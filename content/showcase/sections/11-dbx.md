# SQL, checked at compile time

[stub: SQL stays in charge, no ORM, no query builder; dbx = a module, SQLite only; doobie-style query-first]

## The schema is a value

[stub: schema is a `dbx/schema` of DDL scripts; one macro call defines `query`, `update`, `update-returning`; the schema builds once in an in-memory SQLite at compile time, every statement prepares against it]

```k1
let schema: dbx/schema = .{ scripts = [`
  create table plan (
    id      integer primary key,
    name    text not null,
    is_main integer not null,
    note    text
  ) strict;`] }
$dbx/define-sql-macros(schema)
```

## One query

[stub: row type is a plain struct, fields are columns in order, kebab-case field = snake_case column; `$x`/`${expr}` holes become `?` parameters bound through `sql-value`, parameters can't drift from the SQL; `?string` reads a nullable column; `.list`/`.find`/`.unique`/`.run`]

```k1
type plan = { id: i64, name: string, is-main: bool, note: ?string }
```

```k1
  update("insert into plan (name, is_main, note) values (${"Budget"}, ${true}, null)").run(w).!
  update("insert into plan (name, is_main, note) values (${"What if"}, ${false}, ${"draft"})").run(w).!

  let main-only = false
  let plans = query[plan]("select id, name, is_main, note from plan where is_main or not $main-only order by id").list(w).!
  for plans { println("${it.id} ${it.name} main=${it.is-main} note=${it.note}") }
```

```text
1 Budget main=true note=:none
2 What if main=false note=:some draft
```

## What the compiler catches

[stub: SQL errors, unknown columns; `query` that writes / `update` that reads; column count vs fields; column names vs field names; declared type vs field type; nullable column into a non-option field; the error lands on the SQL literal at the call site, not in the macro]

```k1
type plan = { id: i64, name: string, is-main: bool, note: string }
```

```text
┌────────────────────────────────────────╴
/Users/knix/dev/k1/content/showcase/examples/dbx_query_wrong/dbx_query_wrong.k1:18:22: error
├─────
│     conn.open(":memory:", busy-timeout-ms = 0).!
│     for schema.scripts { conn.exec-script(it).! }
│   
│ ->  println(query[plan]("select id, name, is_main, note from plan order by id").list(conn.as-reader()).!.len())
│                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
│     0
│   }
├─────
│  dbx: column `note` (plan.note) can be null, and `plan.note` (string) can't hold null; make it an option
  select id, name, is_main, note from plan order by id
```

[stub: how it works: the macro is ordinary k1 running in the compiler's VM, calling sqlite3_prepare through FFI on the schema image; `column_decltype`, `table_column_metadata`; no SQL parser of its own]
