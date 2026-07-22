"use strict";

(function initAbapViewerServiceRegistry(global) {
  const runtime = global.AbapViewerRuntime = global.AbapViewerRuntime || {};
  runtime.api = runtime.api || {};
  runtime.constants = runtime.constants || {};
  runtime.services = runtime.services || {};
  runtime.serviceOrder = Array.isArray(runtime.serviceOrder) ? runtime.serviceOrder : [];

  runtime.registerService = function registerService(name, service) {
    const key = String(name || "").trim();
    if (!key) {
      throw new Error("Viewer service name is required.");
    }
    runtime.services[key] = service && typeof service === "object" ? service : {};
    if (!runtime.serviceOrder.includes(key)) {
      runtime.serviceOrder.push(key);
    }
    return runtime.services[key];
  };

  runtime.getService = function getService(name) {
    return runtime.services[String(name || "").trim()] || null;
  };

  runtime.requireServiceMethod = function requireServiceMethod(serviceName, methodName) {
    const serviceKey = String(serviceName || "").trim();
    const methodKey = String(methodName || "").trim();
    return function delegatedServiceMethod() {
      const service = runtime.services[serviceKey];
      const method = service && service[methodKey];
      if (typeof method !== "function") {
        throw new Error("Viewer service method missing: " + serviceKey + "." + methodKey);
      }
      return method.apply(service, arguments);
    };
  };
})(window);
