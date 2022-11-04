import { toArray } from "./arrays";
import { isElementInteractable } from "./interactability";
import { getPosition, Position } from "./position";
import { isElementVisible } from "./visibility";

export type Selector = string;

export interface Schema {
    [name: string]: Schema,
};

export interface Dependencies {
    [selector: string]: Schema,
};

interface ElementState {
    ref?: Element,
    position?: Position,
    [x: string]: any,
}

export interface QueriedState {
    [selector: string]: Array<ElementState>;
}

export function runQuery(selector: Selector, schema: Schema): ElementState[] {
    function queryCssValues(element: HTMLElement, subSchema: Schema): any {
        const css: ElementState = {};
        Object.entries(subSchema).forEach(([name, subSchema]) => {
            if (Object.keys(subSchema).length > 0) {
                throw Error("Schema for CSS value cannot contain sub-schemas: " + JSON.stringify(subSchema));
            } else {
                css[name] = window
                    .getComputedStyle(element)
                    .getPropertyValue(name);
            }
        });
        return css;
    }
    function queryAttributeValues(element: HTMLElement, subSchema: Schema): any {
        const attrs: ElementState = {};
        Object.entries(subSchema).forEach(([name, _]) => {
            attrs[name] = element.getAttribute(name);
        });
        return attrs;
    }
    function queryRecursive(value: any, schema: Schema): any {
        const entries = Object.entries(schema);
        if (entries.length === 0) {
            return value;
        } else if (typeof value !== 'object') {
            throw Error(`Can't recursively query ${value} using schema: ${schema}`);
        } else {
            const result: ElementState = {};
            entries.forEach(([name, subSchema]) => {
                result[name] = queryRecursive(value[name], subSchema);
            });
            return result;
        }
    }

    const elements: HTMLElement[] = toArray(document.querySelectorAll(selector) as NodeListOf<HTMLElement>);
    return elements.map((element) => {
        var m: ElementState = {};
        Object.entries(schema).forEach(([key, subSchema]) => {
            switch (key) {
                case "enabled":
                    // @ts-ignore
                    m[key] = !element.disabled;
                    break;
                case "visible":
                    m[key] = isElementVisible(element as HTMLElement);
                    break;
                case "interactable":
                    m[key] = isElementInteractable(element);
                    break;
                case "active":
                    m[key] = document.activeElement == element;
                    break;
                case "classList":
                    // @ts-ignore
                    m[key] = Array(...element.classList);
                    break;
                case "css":
                    m[key] = queryCssValues(element, subSchema);
                    break;
                case "attributes":
                    m[key] = queryAttributeValues(element, subSchema);
                    break;
                default:
                    // @ts-ignore
                    m[key] = queryRecursive(element[key], subSchema);
                    break;
            }
        });
        m.ref = element;
        m.position = getPosition(element);
        return m;
    });
}

export function queryState(deps: Dependencies): QueriedState {
    var r: QueriedState = {};
    Object.entries(deps).forEach(([selector, schema]) => {
        r[selector] = runQuery(selector, schema).filter((e: ElementState) => e.ref?.isConnected ?? true);
    });
    return r;
}
