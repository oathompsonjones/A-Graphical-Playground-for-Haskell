import type { ReactNode } from "react";

/**
 * Displays the root of the Haskell notation documentation section.
 * @returns A documentation section.
 */
export function NotationRoot(): ReactNode {
    return (
        <div>
            Please note that the following documentation makes use of Haskell type signatures to describe functions.
            <br />
            Haskell type signatures look like this: <code>identifier :: Type1 -&gt; Type2 -&gt; ... -&gt; TypeN</code>.
            <br />
            The <code>::</code> symbol is read as "has type", and the <code>-&gt;</code> symbol is read as "to".
            <br />
            The <code>identifier</code> is the name of the function, and the <code>TypeX</code> values are the types of
                the arguments and the return value. The last <code>TypeN</code> is always the return value.
            <br />
            If an argument's type looks like this: <code>(Type1 -&gt; Type2)</code>, it means that that argument is a
                function that takes a <code>Type1</code> and returns a <code>Type2</code>.
            <br />
            Infix operators in Haskell are defined the same way as functions, but with the operator name in parentheses.
        </div>
    );
}
