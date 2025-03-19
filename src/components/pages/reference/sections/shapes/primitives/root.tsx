import type { ReactNode } from "react";

/**
 * Displays the root of the 2D primitives documentation section.
 * @returns A documentation section.
 */
export function PrimitivesRoot(): ReactNode {
    return (
        <div>
            To draw 2D shapes, you can use any of the functions in this section. They all return
                the <code>Shape</code> data type. By default, all shapes are drawn at the top left corner of the canvas,
                have a stroke color of black, a stroke weight of 1, and no fill color.
        </div>
    );
}
