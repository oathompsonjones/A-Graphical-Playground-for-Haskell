"use client";

import "styles/components/codeTheme.css";
import type { FormEvent, ReactNode, UIEvent } from "react";
import { useEffect, useState } from "react";
import { Paper } from "@mui/material";
import haskell from "highlight.js/lib/languages/haskell";
import hljs from "highlight.js/lib/core";
import styles from "styles/components/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";

// FIXME: This page breaks on production build.
// NotFoundError: The object can not be found here.
// HierarchyRequestError: The operation would yield an incorrect node tree.

/**
 * This is the text editor.
 * @returns The editor element.
 */
export function Editor(): ReactNode {
    const [rawCode, setRawCode] = useLocalStorage("code", "test");
    const [displayCode, setDisplayCode] = useState<ReactNode>(null);

    const handleChange = (event: FormEvent<HTMLTextAreaElement>): void => {
        setRawCode(event.currentTarget.value);
        setDisplayCode(event.currentTarget.value.split("\n").map((line, i) => (
            <code
                key={i} className="language-haskell"
                // eslint-disable-next-line @typescript-eslint/naming-convention
                dangerouslySetInnerHTML={{ __html: /* hljs.highlight( */line/* , { language: "haskell" }).value */ }}
            />
        )));

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

        if (displayCode === null)
            handleChange({ currentTarget: { value: rawCode } } as FormEvent<HTMLTextAreaElement>);
    }, []);

    return (
        <Paper className={styles.editor!}>
            <pre id="code-editor-pre">{displayCode}</pre>
            <textarea
                onChange={handleChange}
                onScroll={handleScroll}
                placeholder="Start writing code..."
                value={rawCode}
            />
        </Paper>
    );
}
