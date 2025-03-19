import type { ReactNode } from "react";

/**
 * Displays the root of the transformations documentation section.
 * @returns A documentation section.
 */
export function TransformationsRoot(): ReactNode {
    return (
        <div>
            To modify a shape, you can use any of the functions in this section. They all take some argument, the shape
                to be transformed, and return the transformed shape.
            <br />
            By default, shapes are drawn at the top left corner of the canvas, with no rotation or scaling, have a
                stroke color of black, a stroke weight of 1, and no fill color.
        </div>
    );
}
