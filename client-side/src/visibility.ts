export function isElementVisible(el: HTMLElement): boolean {
    const cs = window.getComputedStyle(el);
    return (
        cs.getPropertyValue("display") !== "none" &&
        cs.getPropertyValue("visibility") !== "hidden" &&
        cs.getPropertyValue("opacity") !== "0" &&
        el.offsetParent !== null
    );
}