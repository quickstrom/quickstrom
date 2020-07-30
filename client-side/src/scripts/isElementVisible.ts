function isElementVisible(el: HTMLElement): boolean {
  const cs = window.getComputedStyle(el);
  return (
    cs.getPropertyValue("display") !== "none" &&
    cs.getPropertyValue("visibility") !== "hidden" &&
    cs.getPropertyValue("opacity") !== "0" &&
    el.offsetParent !== null
  );
}

// @ts-ignore
const [selector, done] = args;

done({ Right: isElementVisible(selector) });
