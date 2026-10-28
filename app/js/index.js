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
        const inst = window.echarts.getInstanceByDom(el);
        if (inst) inst.resize();
      });
    };

    document.addEventListener("shown.bs.tab", () => window.setTimeout(resizePlots, 60));
    document.addEventListener("shiny:value", () => window.setTimeout(resizePlots, 60));
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot, { once: true });
  } else {
    boot();
  }
})();
