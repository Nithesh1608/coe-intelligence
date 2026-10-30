(() => {
  const boot = () => {
    if (!document.body) return;

    const glow = document.createElement("div");
    glow.className = "pointer-glow";
    document.body.appendChild(glow);
    window.addEventListener("pointermove", (event) => {
      glow.style.left = `${event.clientX}px`;
      glow.style.top = `${event.clientY}px`;
    }, { passive: true });

    const resizePlots = () => {
      window.dispatchEvent(new Event("resize"));
      if (!window.echarts) return;
      document.querySelectorAll("[_echarts_instance_]").forEach((el) => {
        try {
          const inst = window.echarts.getInstanceByDom(el);
          if (inst) inst.resize();
        } catch (_) { /* ignore */ }
      });
    };

    // bslib page_navbar + Posit often paint echarts at 0x0 until a resize.
    document.addEventListener("shown.bs.tab", () => window.setTimeout(resizePlots, 60));
    document.addEventListener("shown.bs.pill", () => window.setTimeout(resizePlots, 60));
    document.addEventListener("shiny:value", () => window.setTimeout(resizePlots, 80));
    document.addEventListener("shiny:sessioninitialized", () => {
      window.setTimeout(resizePlots, 120);
      window.setTimeout(resizePlots, 500);
      window.setTimeout(resizePlots, 1200);
    });
    window.addEventListener("load", () => window.setTimeout(resizePlots, 200));
    window.setTimeout(resizePlots, 300);
    window.setTimeout(resizePlots, 900);
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot, { once: true });
  } else {
    boot();
  }
})();
