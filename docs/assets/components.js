class DocCallout extends HTMLElement {
  connectedCallback() {
    const tone = this.getAttribute("tone") || "";
    const title = this.getAttribute("title") || "";
    this.className = `callout callout-${tone}`;
    this.innerHTML = `<h3>${title}</h3><p>${this.innerHTML}</p>`;
  }
}

class SchemaCard extends HTMLElement {
  connectedCallback() {
    const title = this.getAttribute("title") || "";
    const subtitle = this.getAttribute("subtitle") || "";
    this.className = "schema-card";
    this.innerHTML = `<h3>${title}</h3><p>${subtitle}</p>`;
  }
}

class CodeSample extends HTMLElement {
  connectedCallback() {
    const language = this.getAttribute("language") || "text";
    const copyText = this.dataset.copy || this.textContent || "";
    const code = this.textContent.trim().replace(/</g, "&lt;").replace(/>/g, "&gt;");
    this.className = "code-block";
    this.innerHTML = `
      <div class="code-head">
        <strong>${language}</strong>
        <button class="copy-button" type="button" data-copy-text="${escapeAttribute(copyText)}">Copy</button>
      </div>
      <pre><code>${code}</code></pre>
    `;
  }
}

function escapeAttribute(value) {
  return value
    .replace(/&/g, "&amp;")
    .replace(/"/g, "&quot;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

customElements.define("doc-callout", DocCallout);
customElements.define("schema-card", SchemaCard);
customElements.define("code-sample", CodeSample);
