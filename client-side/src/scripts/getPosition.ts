import { getPosition } from "../position";

// @ts-ignore
const [element, done] = args;

done({ Right: getPosition(element) });
