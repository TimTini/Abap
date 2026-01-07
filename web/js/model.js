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
      this.writes = [];

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
      return `FORM:${String(name || "").trim().toLowerCase()}`;
    }

    static eventKey(name) {
      const n = String(name || "").replace(/\s+/g, " ").trim().toUpperCase();
      return `EVENT:${n}`;
    }

    ensureForm(name) {
      const key = ProgramModel.formKey(name);
      let node = this.nodes.get(key);
      if (!node) {
        node = new AbapRoutine(key, "FORM", String(name || "").trim());
        this.nodes.set(key, node);
      }
      return node;
    }

    defineForm(name, sourceRef, description, params) {
      const node = this.ensureForm(name);
      node.kind = "FORM";
      node.name = String(name || "").trim();
      node.isDefined = true;
      node.sourceRef = sourceRef || node.sourceRef;
      node.description = description || node.description;
      node.params = params || node.params;
      return node;
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
        writes: n.writes,
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
    AbapCallEdge,
    ProgramModel,
  };
})(window.AbapFlow);

