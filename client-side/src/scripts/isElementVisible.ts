import { isElementVisible } from "../visibility";

// @ts-ignore
const [selector, done] = args;

done({ Right: isElementVisible(selector) });
