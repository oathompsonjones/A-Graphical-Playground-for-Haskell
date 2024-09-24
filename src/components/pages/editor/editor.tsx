"use client";

import "styles/components/codeTheme.css";
import type { FormEvent, ReactNode, UIEvent } from "react";
import { useEffect, useState } from "react";
import { Paper } from "@mui/material";
import haskell from "highlight.js/lib/languages/haskell";
import hljs from "highlight.js/lib/core";
import styles from "styles/components/editor.module.css";
import typescript from "highlight.js/lib/languages/typescript";

/**
 * This is the text editor.
 * @returns The editor element.
 */
export function Editor(): ReactNode {
    const [displayCode, setDisplayCode] = useState<ReactNode>(null);

    const handleChange = (event: FormEvent<HTMLTextAreaElement>): void => {
        setDisplayCode(
            event.currentTarget.value
                .split("\n")
                .map((line, i) => (
                    <code
                        key={i}
                        className="language-haskell"
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        dangerouslySetInnerHTML={{ __html: hljs.highlight(line, { language: "haskell" }).value }}
                    />
                )),
        );

        hljs.highlightAll();
    };

    const handleScroll = (event: UIEvent<HTMLTextAreaElement>): void => {
        const pre = document.getElementById("code-editor-pre");

        if (pre !== null) {
            pre.scrollTop = event.currentTarget.scrollTop;
            pre.scrollLeft = event.currentTarget.scrollLeft;
        }
    };

    useEffect(() => {
        if (hljs.getLanguage("haskell") === undefined)
            hljs.registerLanguage("haskell", haskell);

        if (hljs.getLanguage("typescript") === undefined)
            hljs.registerLanguage("typescript", typescript);

        if (displayCode === null)
            handleChange({ currentTarget: { value: "" } } as FormEvent<HTMLTextAreaElement>);
    }, []);

    return (
        <Paper className={styles.editor!}>
            <pre id="code-editor-pre">{displayCode}</pre>
            <textarea onChange={handleChange} onScroll={handleScroll} />
        </Paper>
    );
}
