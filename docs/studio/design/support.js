/*
 * Copyright 2026 Bradley Buchsbaum
 * Licensed under the Apache License, Version 2.0.
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Adapted from storyatlas4s mockups/v2/support.js at 90f682d1acddd759867098e90f6e3ef163244b0e.
 * Original SHA-256: 3d7ea4ea08db311c41bd2cdf1768ac5eae1398207ffe84175a7ed5d23e1723b1
 * Local design-board resolver only; not an Eyes Studio runtime dependency.
 */
(function () {
  "use strict";

  const exactExpression = /^\s*\{\{\s*([^}]+?)\s*\}\}\s*$/;
  const expression = /\{\{\s*([^}]+?)\s*\}\}/g;
  const directiveSelector = "sc-for, sc-if, template[data-dc-directive]";
  const directiveCandidateSelector = "sc-for, sc-if, template[data-dc-directive], *";

  function exactDirectiveExpression(directive, attributeName, directiveKind) {
    const raw = directive.getAttribute(attributeName);
    const match = raw && raw.match(exactExpression);
    if (!match) {
      throw new Error(
        `${directiveKind} ${attributeName} must be one exact design-canvas expression`
      );
    }
    return match[1];
  }

  function directiveKindOf(directive) {
    const localName = directive.localName.toLowerCase();
    return localName === "template"
      ? directive.getAttribute("data-dc-directive")
      : localName;
  }

  function assertKnownDirectiveCandidates(container) {
    for (const element of Array.from(container.querySelectorAll(directiveCandidateSelector))) {
      const localName = element.localName.toLowerCase();
      if (localName.startsWith("sc-") && localName !== "sc-for" && localName !== "sc-if") {
        throw new Error(`Unsupported design-canvas directive: ${localName}`);
      }
      if (localName === "template" && element.hasAttribute("data-dc-directive")) {
        const kind = directiveKindOf(element);
        if (kind !== "for" && kind !== "if" && kind !== "sc-for" && kind !== "sc-if") {
          throw new Error(
            `Unsupported design-canvas directive: ${kind == null ? "<missing>" : kind}`
          );
        }
      }
    }
  }

  function lookup(source, path) {
    const trimmed = path.trim();
    if (trimmed === "true") return true;
    if (trimmed === "false") return false;
    if (trimmed === "null") return null;
    if (/^-?(?:\d+\.?\d*|\.\d+)$/.test(trimmed)) return Number(trimmed);

    const parts = trimmed.split(".");
    let value = source;
    for (const part of parts) {
      if (value == null || !(part in Object(value))) {
        throw new Error(`Unresolved design-canvas expression: ${trimmed}`);
      }
      value = value[part];
    }
    return value;
  }

  function scopeOf(values, locals) {
    return Object.assign(Object.create(null), values, locals);
  }

  function stringify(value, name) {
    if (typeof value === "function") {
      throw new Error(`Function-valued expression ${name} cannot be interpolated as text`);
    }
    return value == null ? "" : String(value);
  }

  function interpolateString(raw, scope) {
    return raw.replace(expression, (_, name) => stringify(lookup(scope, name), name));
  }

  function interpolateTree(container, scope) {
    function visit(node) {
      if (node.nodeType === Node.TEXT_NODE) {
        if (node.nodeValue.includes("{{")) {
          node.nodeValue = interpolateString(node.nodeValue, scope);
        }
        return;
      }

      if (node.nodeType === Node.ELEMENT_NODE) {
        for (const attribute of Array.from(node.attributes)) {
          if (!attribute.value.includes("{{")) continue;
          const exact = attribute.value.match(exactExpression);
          const value = exact ? lookup(scope, exact[1]) : interpolateString(attribute.value, scope);
          const eventName = attribute.name.toLowerCase();
          if (eventName.startsWith("on") && typeof value === "function") {
            node.removeAttribute(attribute.name);
            node.addEventListener(eventName.slice(2), value);
          } else if (value == null || (value === false && ["disabled", "checked", "selected", "hidden"].includes(eventName))) {
            node.removeAttribute(attribute.name);
          } else {
            node.setAttribute(attribute.name, stringify(value, exact ? exact[1] : attribute.name));
          }
        }
      }

      for (const child of Array.from(node.childNodes || [])) visit(child);
    }

    for (const child of Array.from(container.childNodes || [])) visit(child);
  }

  function processDirectives(container, values, locals) {
    assertKnownDirectiveCandidates(container);
    let directive = container.querySelector(directiveSelector);
    while (directive) {
      const currentScope = scopeOf(values, locals);
      const replacement = document.createDocumentFragment();
      const directiveKind = directiveKindOf(directive);
      const children =
        directive.tagName.toLowerCase() === "template" && directive.content
          ? Array.from(directive.content.childNodes)
          : Array.from(directive.childNodes);

      if (directiveKind === "if" || directiveKind === "sc-if") {
        const condition = lookup(
          currentScope,
          exactDirectiveExpression(directive, "value", directiveKind)
        );
        if (condition) {
          for (const child of children) {
            replacement.appendChild(child.cloneNode(true));
          }
          processDirectives(replacement, values, locals);
          interpolateTree(replacement, currentScope);
        }
      } else if (directiveKind === "for" || directiveKind === "sc-for") {
        const listExpression = exactDirectiveExpression(directive, "list", directiveKind);
        const list = lookup(currentScope, listExpression);
        if (!Array.isArray(list)) {
          throw new Error(`${directiveKind} ${listExpression} did not resolve to an array`);
        }
        const localName = directive.getAttribute("as");
        if (!localName || !localName.trim()) {
          throw new Error(`${directiveKind} requires a non-empty as attribute`);
        }
        for (const item of list) {
          const itemFragment = document.createDocumentFragment();
          for (const child of children) {
            itemFragment.appendChild(child.cloneNode(true));
          }
          const itemLocals = Object.assign(Object.create(null), locals, { [localName]: item });
          processDirectives(itemFragment, values, itemLocals);
          interpolateTree(itemFragment, scopeOf(values, itemLocals));
          replacement.appendChild(itemFragment);
        }
      } else {
        throw new Error(
          `Unsupported design-canvas directive: ${directiveKind == null ? "<missing>" : directiveKind}`
        );
      }

      directive.replaceWith(replacement);
      directive = container.querySelector(directiveSelector);
    }
  }

  class DCLogic {
    constructor(props) {
      this.props = props || {};
      this.state = this.state || {};
      this.__dcRender = null;
    }

    setState(patch) {
      if (patch == null || typeof patch !== "object" || Array.isArray(patch)) {
        throw new Error("DCLogic.setState requires an object patch");
      }
      this.state = Object.assign({}, this.state, patch);
      if (this.__dcRender) this.__dcRender();
    }
  }

  window.DCLogic = DCLogic;

  function renderFailure(error) {
    document.documentElement.dataset.dcResolved = "false";
    document.documentElement.dataset.dcError = String(error && error.message ? error.message : error);
    const pre = document.createElement("pre");
    pre.id = "dc-resolution-error";
    pre.setAttribute("role", "alert");
    pre.style.cssText =
      "white-space:pre-wrap;margin:24px;padding:16px;border:2px solid #852A12;color:#852A12;background:#fff;font:14px/1.5 ui-monospace,monospace";
    pre.textContent = `Design-canvas resolution failed\n${error && error.stack ? error.stack : error}`;
    document.body.replaceChildren(pre);
  }

  function defaults(script) {
    const schema = JSON.parse(script.getAttribute("data-props") || "{}");
    return Object.fromEntries(Object.entries(schema)
      .filter(([name]) => !name.startsWith("$"))
      .map(([name, value]) => [name, value.default]));
  }

  async function resolveImports(container, baseUrl) {
    for (const imported of Array.from(container.querySelectorAll("dc-import"))) {
      const name = imported.getAttribute("name");
      if (!/^[A-Za-z]+$/.test(name || "")) throw new Error(`Invalid dc-import name: ${name}`);
      const url = new URL(`${name}.dc.html`, baseUrl);
      if (url.origin !== location.origin) throw new Error("Cross-origin dc-import refused");
      const response = await fetch(url);
      if (!response.ok) throw new Error(`dc-import ${name}: HTTP ${response.status}`);
      const doc = new DOMParser().parseFromString(await response.text(), "text/html");
      const props = {};
      for (const attr of Array.from(imported.attributes)) {
        if (!["name", "hint-size"].includes(attr.name)) {
          props[attr.name] = attr.value === "true" ? true : attr.value === "false" ? false : attr.value;
        }
      }
      const host = document.createElement("div");
      await mount(doc, host, props, url);
      imported.replaceWith(host);
    }
  }

  async function mount(doc, root, overrides, baseUrl) {
    const sourceRoot = doc.querySelector("x-dc");
    const script = doc.querySelector("script[data-dc-script]");
    if (!sourceRoot || !script) throw new Error("Board needs x-dc and its data-dc-script");
    // Figures uses only createElement(Fragment, null, text) to preserve literal
    // SVG label text. Support precisely that operation; refuse every other React API.
    const Fragment = Symbol("text fragment");
    const textFragments = {
      Fragment,
      createElement(type, props, ...children) {
        if (type !== Fragment || props != null || children.some(c => !["string", "number"].includes(typeof c))) {
          throw new Error("Board resolver supports only primitive text fragments");
        }
        return children.join("");
      }
    };
    const ComponentClass = new Function("DCLogic", "React", `${script.textContent}\nreturn Component;`)(DCLogic, textFragments);
    const component = new ComponentClass(Object.assign(defaults(script), overrides));
    for (const style of Array.from(sourceRoot.querySelectorAll("helmet style"))) {
      const copy = style.cloneNode(true);
      copy.dataset.dcResolvedStyle = "true";
      document.head.appendChild(copy);
    }
    const source = document.createElement("template");
    for (const child of Array.from(sourceRoot.childNodes)) {
      if (child.nodeType === Node.ELEMENT_NODE && child.localName === "helmet") continue;
      source.content.appendChild(child.cloneNode(true));
    }
    const render = async () => {
      const values = component.renderVals ? component.renderVals() : {};
      if (!values || typeof values !== "object" || Array.isArray(values)) {
        throw new Error("Component.renderVals must return an object");
      }
      const fragment = source.content.cloneNode(true);
      processDirectives(fragment, values, Object.create(null));
      interpolateTree(fragment, scopeOf(values, Object.create(null)));
      await resolveImports(fragment, baseUrl);
      root.replaceChildren(fragment);
      root.style.display = "block";
    };
    component.__dcRender = () => render().catch(renderFailure);
    await render();
    return component;
  }

  async function bootstrap() {
    try {
      const root = document.querySelector("x-dc");
      if (!root) throw new Error("Missing x-dc root");
      window.__dcComponent = await mount(document, root, {}, location.href);
      document.documentElement.dataset.dcResolved = "true";
      window.dispatchEvent(new CustomEvent("dc-resolved"));
    } catch (error) {
      renderFailure(error);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bootstrap, { once: true });
  } else {
    queueMicrotask(bootstrap);
  }
})();
