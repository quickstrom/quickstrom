export type Ctor<T> = { new(...args: any[]): T; };

function findParentOfType(el: HTMLElement, typ: Ctor<HTMLElement>): HTMLElement | null {
    if (!!el.parentElement) {
        if (el.parentElement instanceof typ) {
            return el.parentElement;
        } else {
            return findParentOfType(el.parentElement, typ);
        }
    } else {
        return null;
    }
}

function isVisibleOption(el: HTMLElement) {
    if (!(el instanceof HTMLOptionElement)) {
        return false;
    }
    const select = findParentOfType(el, HTMLSelectElement);
    return !!select && isElementVisible(select);
}

export function isElementVisible(el: HTMLElement): boolean {
    const cs = window.getComputedStyle(el);

    return (
        cs.getPropertyValue("display") !== "none" &&
        cs.getPropertyValue("visibility") !== "hidden" &&
        cs.getPropertyValue("opacity") !== "0" &&
        // Fixed elements and HTMLOptionElements (in Chrome, at least) have no offsetParent
        (cs.position === "fixed"
            || isVisibleOption(el)
            || el.offsetParent !== null)
    );
}

type Point = [number, number];

export function isElementInViewport(element: HTMLElement): boolean {
    const windowHeight = window.innerHeight || document.documentElement.clientHeight;
    const windowWidth = window.innerWidth || document.documentElement.clientWidth;

    function contained([y, x]: Point): boolean {
        return (
            y >= 0
            && x >= 0
            && y < windowHeight
            && x < windowWidth
        );
    }

    const rects: DOMRect[] = element.getClientRects() as any as DOMRect[];

    for (const rect of rects) {
        const points: Point[] = [[rect.top, rect.left], [rect.top, rect.right], [rect.bottom, rect.left], [rect.bottom, rect.right]];
        for (const point of points) {
            if (contained(point)) {
                return true;
            }
        }
    }
    return false;
}
