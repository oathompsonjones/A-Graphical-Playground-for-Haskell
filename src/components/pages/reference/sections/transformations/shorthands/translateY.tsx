import type { ReactNode } from "react";

/**
 * Displays the translateY documentation section.
 * @returns A documentation section.
 */
export function TranslateY(): ReactNode {
    return (
        <div>
            The <code>translateY :: Float -&gt; Shape -&gt; Shape</code> function is shorthand for <code>translate
                (Vector 0 y)</code>.
        </div>
    );
}
