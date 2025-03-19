import type { ReactNode } from "react";

/**
 * Displays the chaining transformations documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function ChainingTransformations({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    return (
        <div>
            Transformations can be applied in two ways. The first is to simply apply the transformation function to the
                shape directly (e.g. <code>fill Red (circle 50)</code>). <br />
            The second is to use the <code>(&gt;&gt;&gt;) :: Shape -&gt; (Shape -&gt; Shape) -&gt; Shape</code> operator
                to chain transformations together (e.g. <code>circle 50 &gt;&gt;&gt; fill Red</code>).
            The <code>id</code> function is the identity function for the <code>&gt;&gt;&gt;</code> operator.
            <br />
            <br />
            The following examples all produce the same result:
            {list([
                <><pre>fill Red (translate (Vector 50 50) (rotate (radians 45) (circle 50)))</pre></>,
                <><pre>circle 50 &gt;&gt;&gt; fill Red &gt;&gt;&gt; translate (Vector 50 50) &gt;&gt;&gt; rotate
                    (radians 45)</pre></>,
                <><pre>circle 50 &gt;&gt;&gt; (fill Red . translate (Vector 50 50) . rotate (radians 45))</pre></>,
                <><pre>circle 50 &gt;&gt;&gt; (foldr (.) id [fill Red, translate (Vector 50 50), rotate (radians
                    45)])</pre></>,
                <><pre>foldl (&gt;&gt;&gt;) (circle 50) [fill Red, translate (Vector 50 50), rotate (radians 45)]</pre>
                </>,
            ])}
        </div>
    );
}
