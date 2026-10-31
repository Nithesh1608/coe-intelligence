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
      // echarts4r often mounts the instance on an inner div without _echarts_instance_
      // on the Shiny output node — walk widgets and children.
      document.querySelectorAll(".echarts4r, [_echarts_instance_]").forEach((el) => {
        try {
          let inst = window.echarts.getInstanceByDom(el);
          if (!inst) {
            for (const child of el.querySelectorAll("div")) {
              inst = window.echarts.getInstanceByDom(child);
              if (inst) break;
            }
          }
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

    // Below-fold Home charts can stay blank until they enter the viewport.
    if ("IntersectionObserver" in window) {
      const io = new IntersectionObserver((entries) => {
        if (entries.some((e) => e.isIntersecting)) {
          window.setTimeout(resizePlots, 40);
        }
      }, { root: null, threshold: 0.05 });
      const watch = () => {
        document.querySelectorAll(".chart-frame, .echarts4r").forEach((el) => io.observe(el));
      };
      watch();
      document.addEventListener("shiny:value", () => window.setTimeout(watch, 100));
    }
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot, { once: true });
  } else {
    boot();
  }
})();
