import type { ReactNode } from "react";

/**
 * Displays the root of the Haskell documentation section.
 * @returns The Haskell documentation section.
 */
export function HaskellRoot(): ReactNode {
    return (
        <div>
            Haskell has a built in library called <code>Prelude</code> which contains many useful functions and types.
            This library is automatically imported into every Haskell project, including your sketches on this site.
            You can find the documentation for the <code>Prelude</code> library <a
                href="https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html">here</a>.
        </div>
    );
}
