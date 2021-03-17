import { isElementVisible } from "./visibility";

export type Position = { x: number; y: number; width: number; height: number; }

export function getPosition(element: Element): Position | undefined {
    if (isElementVisible(element as HTMLElement)) {
        const rect = element.getBoundingClientRect();
        return {
            x: Math.round(rect.left),
            y: Math.round(rect.top),
            width: Math.round(rect.right - rect.left),
            height: Math.round(rect.bottom - rect.top),
        }
    } else {
        return undefined;
    }
}