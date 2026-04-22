# ABAP Statement Coverage and Object Model

This document is the repo-level guide for:
- official ABAP statement references used by this project
- how statements are converted into objects in the current codebase
- which paths should be treated as canonical in new template work
- what should be refactored next to reduce schema drift

## 1. Scope

This parser/viewer is not a full ABAP grammar. It is a config-driven subset built from [`configs/*.json`](/e:/Documents/MyGitProject/Abap/configs) and then enriched by the viewer/export/template runtime.

Official ABAP statement overview:
- SAP Help, ABAP statements overview: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abenabap_statements.htm>

Useful statement pages that match current project coverage:
- `DATA`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapdata.htm>
- `TYPES`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abaptypes.htm>
- `IF`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapif.htm>
- `SELECT`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapselect.htm>
- `READ TABLE`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapread_table.htm>
- `LOOP AT itab`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abaploop_at_itab.htm>
- `MODIFY itab`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapmodify_itab.htm>
- `DELETE itab`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapdelete_itab.htm>
- `CALL METHOD`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapcall_method.htm>
- `CALL FUNCTION`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapcall_function.htm>
- `PERFORM`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abapperform.htm>
- `TABLES`: <https://help.sap.com/http.svc/rc/abapdocu_752_index_htm/7.52/en-US/abaptables.htm>

## 2. Current Supported Statement Families

Current built-in configs cover these object families:
- Declarations: `DATA`, `TYPES`, `PARAMETERS`, `SELECT-OPTIONS`, `RANGES`, `STATICS`, `CONSTANTS`, `FIELD-SYMBOLS`, `CLASS-DATA`
- Signatures and containers: `CLASS`, `METHOD`, `METHODS`, `CLASS-METHODS`, `FORM`
- Control flow: `IF`, `ELSEIF`, `ELSE`, `CASE`, `WHEN`, `DO`, `TRY`, `CATCH`, `CLEANUP`
- Calls and execution: `CALL_FUNCTION`, `CALL_METHOD`, `CALL_TRANSACTION`, `PERFORM`
- Assignments and transforms: `ASSIGNMENT`, `MOVE`, `MOVE-CORRESPONDING`, `CLEAR`
- Internal table and data access: `SELECT`, `READ_TABLE`, `LOOP_AT_ITAB`, `MODIFY_ITAB`, `DELETE_ITAB`, `INSERT_ITAB`, `APPEND`, `SORT_ITAB`

Important gap:
- Standalone `TABLES` is used in [`examples/deep_form_demo.abap`](/e:/Documents/MyGitProject/Abap/examples/deep_form_demo.abap), but there is no `configs/tables.json`, so the current parser does not emit a `TABLES` object for that statement family.

## 3. Current Conversion Pipeline

The current runtime has three layers even though they are not named explicitly.

### Layer A: Parse object

The base parser object is the `AbapObject` created in [`shared/abap-parser/01-context.js`](/e:/Documents/MyGitProject/Abap/shared/abap-parser/01-context.js).

Stable base fields:
- `id`
- `parent`
- `objectType`
- `file`
- `lineStart`
- `raw`
- `block`
- `extras`
- `comment`
- `keywords`
- `values`
- `children`

Meaning:
- `values` is generic capture output from config rules or statement-specific capture logic
- `extras` is statement-specific detail
- parser output is intentionally sparse and backward-compatible

### Layer B: Decl/condition enrichment

After statement parsing, declaration binding enriches the object model:
- `values.*.declRef`
- `values.*.decl`
- `extras.*` argument items with `valueDecl` and `originDecls`
- condition clauses with `leftOperandDecl` and `rightOperandDecl`
- unary `IS INITIAL` / `IS NOT INITIAL` / `IS ASSIGNED` / `IS BOUND` / `IS SUPPLIED` / `IS REQUESTED` use synthetic right-side `SYSTEM` decls

This layer is still close to parser semantics.

### Layer C: Viewer/export/template normalization

The viewer then adds more synthetic structure in several different places:
- Output tree rendering builds renderable objects and traced decl chains
- XML export normalizes entry objects through `normalizeEntryObjectForXml`
- Template preview clones objects through `buildTemplateContextObject`

These three consumers currently apply overlapping but different normalization rules. This is the main reason the schema feels patched together.

## 4. Where The Schema Becomes Confusing

The main ambiguity today is not `AbapObject` itself. The ambiguity comes from mixed semantics on top of it.

Examples:
- `finalDesc` exists for decl-like objects and for value-like objects
- template resolution has special handling so `values.expr.decl.finalDesc` may resolve to the value-level `finalDesc`, not the plain decl-level `finalDesc`
- Output, XML, and Template each synthesize decls for conditions and argument rows in slightly different places

Current rule for new template work:
- Prefer `values.<name>.finalDesc` when you want the value-level final text
- Prefer `values.<name>.decl.desc` when you want the display description of the bound decl
- Treat `values.<name>.decl.finalDesc` as a compatibility path for value entries, not as the canonical path for new templates
- For traced `PERFORM` params, inspect `originDecls[]` instead of assuming `decl` always points to the local `FORM_PARAM`

Statement-specific conditions remain statement-specific:
- `extras.ifCondition.conditions`
- `extras.performCall.ifConditions`
- `extras.select.whereConditions`
- `extras.select.havingConditions`
- `extras.readTable.conditions`
- `extras.loopAtItab.conditions`
- `extras.modifyItab.conditions`
- `extras.deleteItab.conditions`

That statement-specific detail is acceptable. The problem is that the conversion logic is duplicated across consumers.

## 5. Recommended Canonical Architecture

Do not try to force every ABAP statement into one identical flat shape. That will either lose information or create more special cases.

Instead keep three explicit schemas:

1. `ParseAbapObject`
- exactly the current parser contract
- minimal, backward-compatible, config-driven

2. `ResolvedAbapObject`
- one shared conversion result derived from `ParseAbapObject`
- used by Output, Template, and XML
- keeps canonical normalized fields in predictable places

3. Consumer projections
- `toOutputNode(resolved)`
- `toTemplateContext(resolved)`
- `toXmlNode(resolved)`

Recommended shared convert entry point:

```js
const resolved = resolveAbapObject(parseObject, context);
```

Recommended responsibilities of `ResolvedAbapObject`:
- preserve original statement object under `source`
- expose canonical `valuesByName`
- expose normalized `valueEntries[]`
- expose normalized `conditions[]`
- expose normalized `argumentSections`
- expose `decls` and `originDecls` in one place
- expose explicit desc metadata so value-level and decl-level `finalDesc` are never ambiguous

One workable target shape:

```js
{
  source: parseObject,
  objectType: parseObject.objectType,
  valuesByName: { ... },
  valueEntries: [ ... ],
  conditions: [ ... ],
  argumentSections: { ... },
  desc: {
    valueFinalDescByName: { ... },
    declDescByName: { ... },
    declFinalDescByName: { ... }
  }
}
```

If more explicit template paths are needed later, expose them from the resolved layer instead of teaching each consumer new aliases.

## 6. Recommended Refactor Order

Lowest-risk order:

1. Keep parser output stable.
2. Introduce one shared resolver at viewer/export layer first.
3. Migrate Template and XML to consume the same resolved structure.
4. Migrate Output tree helpers last.
5. Keep old template paths working as compatibility paths, but document only the canonical ones.

Do not start by rewriting `configs/*.json` or by changing the parse contract for all statements.

## 7. Immediate Changes Worth Doing

These are the changes that currently make the most sense:
- add `TABLES` support if DDIC work-area parsing is required
- update [`RULES.md`](/e:/Documents/MyGitProject/Abap/RULES.md) so it no longer looks like the full runtime schema
- keep this file as the source of truth for canonical path guidance
- before adding more viewer-side synthetic fields, route them through one shared resolver

## 8. Practical Rules For New Work

When adding a new statement:
- first decide whether it belongs only in `values`
- then decide whether it needs statement-specific detail in `extras`
- do not add consumer-only synthetic fields directly to parser output unless they are required for backward compatibility

When adding a new template path:
- prefer one canonical path
- if a compatibility alias is required, document it explicitly here
- do not rely on hidden special cases in the template resolver as the primary contract

When adding new condition support:
- keep parser detail in `extras.<kind>.conditions`
- feed Output, XML, and Template from the same resolved condition model
