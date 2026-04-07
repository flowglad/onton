document.addEventListener("DOMContentLoaded", () => {
  /* ── Copy buttons ── */
  document.querySelectorAll(".copy-button").forEach((button) => {
    const defaultLabel = button.textContent || "Copy";
    let resetTimer = null;

    const resetLabelSoon = () => {
      if (resetTimer) {
        window.clearTimeout(resetTimer);
      }
      resetTimer = window.setTimeout(() => {
        button.textContent = defaultLabel;
      }, 1200);
    };

    button.addEventListener("click", async () => {
      const text = button.getAttribute("data-copy-text") || "";
      try {
        await navigator.clipboard.writeText(text);
        button.textContent = "Copied";
        resetLabelSoon();
      } catch {
        button.textContent = "Copy failed";
        resetLabelSoon();
      }
    });
  });
});
