(function (ns) {
  "use strict";

  class SourceRef {
    constructor(startLine, endLine) {
      this.startLine = startLine;
      this.endLine = endLine;
    }
  }

  class AbapParameter {
    constructor(kind, name, dataType, description, sourceRef) {
      this.kind = kind;
      this.name = name;
      this.dataType = dataType || "";
      this.description = description || "";
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapDataDeclaration {
    constructor(declKind, variableName, dataType, value, description, sourceRef) {
      this.declKind = declKind;
      this.variableName = variableName;
      this.dataType = dataType || "";
      this.value = value || "";
      this.description = description || "";
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapWrite {
    constructor(variableName, statement, sourceRef) {
      this.variableName = variableName;
      this.statement = statement;
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapAssignment {
    constructor(lhs, rhs, statement, sourceRef) {
      this.lhs = lhs;
      this.rhs = rhs;
      this.statement = statement || "";
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapIfStatement {
    constructor(kind, condition, sourceRef) {
      this.kind = String(kind || "").trim().toUpperCase() || "IF";
      this.condition = String(condition || "").trim();
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapMessageStatement {
    constructor(options) {
      this.kind = "MESSAGE";
      this.msgType = String(options?.msgType || "").trim();
      this.msgClass = String(options?.msgClass || "").trim();
      this.msgNo = String(options?.msgNo || "").trim();
      this.text = String(options?.text || "").trim();
      this.displayLike = String(options?.displayLike || "").trim();
      this.with = Array.isArray(options?.with) ? options.with.map((x) => String(x ?? "").trim()).filter(Boolean) : [];
      this.into = String(options?.into || "").trim();
      this.raising = String(options?.raising || "").trim();
      this.statement = String(options?.statement || "").trim();
      this.sourceRef = options?.sourceRef || null;
    }
  }

  class AbapItabOperation {
    constructor(kind, options, sourceRef) {
      this.kind = String(kind || "").trim().toUpperCase();
      this.table = String(options?.table || "").trim();
      this.target = String(options?.target || "").trim();
      this.conditionText = String(options?.conditionText || "").trim();
      this.conditionKind = String(options?.conditionKind || "").trim().toLowerCase(); // key|where|index|from|free
      this.binarySearch = Boolean(options?.binarySearch);
      this.statement = String(options?.statement || "").trim();
      this.sourceRef = sourceRef || null;
    }
  }

  class AbapCallEdge {
    constructor(fromKey, toKey, targetName, args, sourceRef) {
      this.fromKey = fromKey;
      this.toKey = toKey;
      this.targetName = targetName;
      this.args = args || { tables: [], using: [], changing: [] };
      this.sourceRef = sourceRef || null;
      this.isInCycle = false;
    }
  }

  class AbapRoutine {
    constructor(key, kind, name) {
      this.key = key;
      this.kind = kind;
      this.name = name;
      this.description = "";
      this.isDefined = false;
      this.sourceRef = null;

      this.params = [];
      this.localData = [];
      this.localConstants = [];
      this.assignments = [];
      this.writes = [];
      this.ifStatements = [];
      this.messages = [];
      this.itabOps = [];
      this.statementItems = [];

      this.calls = [];
      this.calledBy = [];

      this.depth = null;
      this.isInCycle = false;
    }

    get paramsCount() {
      return this.params.length;
    }

    get localDataCount() {
      return this.localData.length;
    }

    get localConstCount() {
      return this.localConstants.length;
    }

    get writesCount() {
      return this.writes.length;
    }

    get assignmentsCount() {
      return this.assignments.length;
    }
  }

  class ProgramModel {
    constructor(text, lines, lineStartOffsets) {
      this.text = text || "";
      this.lines = lines || [];
      this.lineStartOffsets = lineStartOffsets || [];

      this.nodes = new Map();
      this.edges = [];
      this.warnings = [];

      this.globalData = [];
      this.globalConstants = [];
    }

    static formKey(name) {
      return ProgramModel.routineKey("FORM", name);
    }

    static eventKey(name) {
      const n = String(name || "").replace(/\s+/g, " ").trim().toUpperCase();
      return `EVENT:${n}`;
    }

    static normalizeRoutineKind(kind) {
      return String(kind || "").trim().toUpperCase() || "FORM";
    }

    static routineKey(kind, name) {
      const k = ProgramModel.normalizeRoutineKind(kind);
      if (k === "EVENT") return ProgramModel.eventKey(name);
      return `${k}:${String(name || "").trim().toLowerCase()}`;
    }

    ensureRoutine(kind, name) {
      const k = ProgramModel.normalizeRoutineKind(kind);
      if (k === "EVENT") return this.ensureEvent(name);

      const key = ProgramModel.routineKey(k, name);
      let node = this.nodes.get(key);
      if (!node) {
        node = new AbapRoutine(key, k, String(name || "").trim());
        this.nodes.set(key, node);
      } else {
        node.kind = k;
        node.name = node.name || String(name || "").trim();
      }
      return node;
    }

    defineRoutine(kind, name, sourceRef, description, params) {
      const node = this.ensureRoutine(kind, name);
      node.kind = ProgramModel.normalizeRoutineKind(kind);
      node.name = String(name || "").trim();
      node.isDefined = true;
      node.sourceRef = sourceRef || node.sourceRef;
      node.description = description || node.description;
      node.params = params || node.params;
      return node;
    }

    ensureForm(name) {
      return this.ensureRoutine("FORM", name);
    }

    defineForm(name, sourceRef, description, params) {
      return this.defineRoutine("FORM", name, sourceRef, description, params);
    }

    ensureEvent(name, sourceRef) {
      const key = ProgramModel.eventKey(name);
      let node = this.nodes.get(key);
      if (!node) {
        node = new AbapRoutine(key, "EVENT", String(name || "").trim());
        node.isDefined = true;
        node.sourceRef = sourceRef || null;
        this.nodes.set(key, node);
      } else {
        node.kind = "EVENT";
        node.name = String(name || "").trim();
        node.isDefined = true;
        if (sourceRef && !node.sourceRef) node.sourceRef = sourceRef;
      }
      return node;
    }

    addEdge(edge) {
      this.edges.push(edge);
    }

    serialize() {
      const nodes = Array.from(this.nodes.values()).map((n) => ({
        key: n.key,
        kind: n.kind,
        name: n.name,
        description: n.description,
        isDefined: n.isDefined,
        depth: n.depth,
        isInCycle: n.isInCycle,
        sourceRef: n.sourceRef,
        params: n.params,
        localData: n.localData,
        localConstants: n.localConstants,
        assignments: n.assignments,
        writes: n.writes,
        ifStatements: n.ifStatements,
        messages: n.messages,
        itabOps: n.itabOps,
        statementItems: n.statementItems,
        calls: n.calls.map((e) => ({ toKey: e.toKey, sourceRef: e.sourceRef, args: e.args, isInCycle: e.isInCycle })),
        calledBy: n.calledBy.map((e) => ({ fromKey: e.fromKey, sourceRef: e.sourceRef, args: e.args, isInCycle: e.isInCycle })),
      }));

      return {
        version: ns.version,
        globals: { data: this.globalData, constants: this.globalConstants },
        nodes,
        edges: this.edges,
        warnings: this.warnings,
      };
    }
  }

  ns.model = {
    SourceRef,
    AbapRoutine,
    AbapParameter,
    AbapDataDeclaration,
    AbapWrite,
    AbapAssignment,
    AbapIfStatement,
    AbapMessageStatement,
    AbapItabOperation,
    AbapCallEdge,
    ProgramModel,
  };
})(window.AbapFlow);
