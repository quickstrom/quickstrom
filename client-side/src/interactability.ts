import {isElementVisible} from "./visibility";

function isChildOf(el: HTMLElement, parent: HTMLElement): boolean {
    if (el === parent) {
        return true;
    } else if (!!el.parentElement) {
        return isChildOf(el.parentElement, parent);
    } else {
        return false;
    }
}

function wouldReceiveClick(element: HTMLElement): boolean {
    // check if clicking at the center of the element would hit it
    const rect = element.getBoundingClientRect();
    const x = rect.left + rect.width / 2;
    const y = rect.top + rect.height / 2;
    const hitElement = document.elementFromPoint(x, y);
    return !!hitElement && isChildOf(hitElement as HTMLElement, element);
}

function isElementInFront(element: HTMLElement): boolean {
    if (element instanceof HTMLOptionElement) {
        // Options are not interacted with directly, but through their parent select element
        return isElementInFront(element.parentElement as HTMLElement);
    } else if (element instanceof HTMLOptGroupElement) {
        // Option groups are not interacted with directly, but through their parent select element
        return isElementInFront(element.parentElement as HTMLElement);
    } else {
        return wouldReceiveClick(element);
    }
}

export function isElementInteractable(element: HTMLElement) {
    return isElementVisible(element) && window.getComputedStyle(element).pointerEvents !== "none" && isElementInFront(element);
}
