import type { ReactNode } from "react";

/**
 * Displays the combining shapes documentation section.
 * @returns A documentation section.
 */
export function CombiningShapes(): ReactNode {
    return (
        <div>
            Shapes can be combined using the <code>(&amp;) :: Shape -&gt; Shape -&gt; Shape</code> operator.
            The <code>empty</code> is the identity function for the <code>&amp;</code> operator.
            <br />
            <br />
            The resulting <code>Shape</code> is a list of <code>Shape</code>s that are drawn on top of each other, in
                the order they are combined.
            Applying transformations to a group of shapes applies the transformation to each shape individually.
            Given that the shapes may have different relative origin points, applying transformations such as a rotation
                or scaling may result in unexpected behavior.
        </div>
    );
}
