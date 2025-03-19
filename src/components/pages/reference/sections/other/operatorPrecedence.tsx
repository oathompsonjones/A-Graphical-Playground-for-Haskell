import type { ReactNode } from "react";

/**
 * Displays the operator precedence documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function OperatorPrecedence({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    return (
        <div>
            Precedence for the <code>(&lt;&lt;&lt;)</code>, <code>(&amp;)</code>, and <code>(&gt;&gt;&gt;)
            </code> operators is defined as follows:
            {list([
                <><code>infixl 7 &lt;&lt;&lt;:</code></>,
                <><code>infixl 7 &lt;&lt;&lt;</code></>,
                <><code>infixr 8 &amp;</code></>,
                <><code>infixl 9 &gt;&gt;&gt;</code></>,
            ])}
            This means that expressions such as the following do not require parentheses.
            <pre>{[
                "render $ createCanvas 500 500 <<< circle 100 >>> translate (Vector 250 250)",
                "                                & square 200 >>> translate (Vector 250 250)",
            ].join("\n")}</pre>
        </div>
    );
}
