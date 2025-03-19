import type { ReactNode } from "react";

/**
 * Displays the translateX documentation section.
 * @returns A documentation section.
 */
export function TranslateX(): ReactNode {
    return (
        <div>
            The <code>translateX :: Float -&gt; Shape -&gt; Shape</code> function is shorthand for <code>translate
                (Vector x 0)</code>.
        </div>
    );
}
