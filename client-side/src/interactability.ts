import { isElementVisible } from "./visibility";

export function isElementInteractable(element: HTMLElement) {
    return isElementVisible(element) && window.getComputedStyle(element).pointerEvents !== "none";
}
