(() => {
  const glow = document.createElement("div");
  glow.className = "pointer-glow";
  document.body.appendChild(glow);
  window.addEventListener("pointermove", (event) => {
    glow.style.left = `${event.clientX}px`;
    glow.style.top = `${event.clientY}px`;
  }, { passive: true });
})();
