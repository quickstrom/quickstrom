export function isElementVisible(el: HTMLElement): boolean {
    const cs = window.getComputedStyle(el);
    return (
        cs.getPropertyValue("display") !== "none" &&
        cs.getPropertyValue("visibility") !== "hidden" &&
        cs.getPropertyValue("opacity") !== "0" &&
        // Fixed elements and HTMLOptionElements (in Chrome, at least) have no offsetParent
        (cs.position === "fixed"
            || (el instanceof HTMLOptionElement && el.parentElement === document.activeElement)
            || el.offsetParent !== null)
    );
}
