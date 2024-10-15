"use client";

import "styles/components/codeTheme.css";
import type { FormEvent, ReactNode, UIEvent } from "react";
import { useEffect, useState } from "react";
import type { HLJSApi } from "highlight.js";
import { Paper } from "@mui/material";
import styles from "styles/components/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";

/*
TODO:

* Remove things like macOS double space inserting a dot.
* Allow use of the tab key.
* Support keyboard shortcuts, like Ctrl+S to save, Ctrl+Enter to run, etc.
* Allow the Haskell code to be executed, displaying text output in the console.
* Add accounts to allow users to save their code.
* Add the ability to share code via a URL.
* Add autocomplete for Haskell code.
* Implement graphics and animations.
*/

/*
! Some of this code looks cumbersome and unnecessary.
It's a workaround for a bug in the next.js build process.
? What caused the bug?
The next.js build process doesn't like the regex used for Haskell's octal literals in highlight.js.
? How to fix it?
Add `transpilePackages: ["highlight.js"]` to the next.config.js file.
Use dynamic imports to load highlight.js.
That's why we are storing the highlight.js instance in a state variable,
    because otherwise there would be way too many async funcitons.
*/

/**
 * This is the text editor.
 * @returns The editor element.
 */
export function Editor(): ReactNode {
    // Setup state variables.
    const [hljs, setHljs] = useState<HLJSApi>(null!);
    const [rawCode, setRawCode] = useLocalStorage("code", "-- Start writing your code here.\n\n");
    const [displayCode, setDisplayCode] = useState<ReactNode>(null);

    /**
     * Highlights the code.
     * @param code - The code to highlight.
     * @param _hljs - The highlight.js instance. If not provided, the state variable will be used.
     * @returns The highlighted code.
     */
    function highlightCode(code: string, _hljs: HLJSApi = hljs): ReactNode {
        return code.split("\n").map((line, i) => (
            <code
                key={i}
                className="language-haskell"
                // eslint-disable-next-line @typescript-eslint/naming-convention
                dangerouslySetInnerHTML={{ __html: _hljs.highlight(line, { language: "haskell" }).value }}
            />
        ));
    }

    /**
     * Updates the state variables when the textarea changes, keeping the highlighted code in sync.
     * @param event - The form event.
     */
    function handleChange(event: FormEvent<HTMLTextAreaElement>): void {
        if (event.currentTarget.value === rawCode)
            return;

        setRawCode(event.currentTarget.value);
        setDisplayCode(highlightCode(event.currentTarget.value));
    }

    /**
     * Makes sure the pre element scrolls with the textarea.
     * @param event - The UI event.
     */
    function handleScroll(event: UIEvent<HTMLTextAreaElement>): void {
        const pre = document.getElementById("code-editor-pre");

        if (pre !== null) {
            pre.scrollTop = event.currentTarget.scrollTop;
            pre.scrollLeft = event.currentTarget.scrollLeft;
        }
    }

    // Initialize
    useEffect(() => {
        // Set the textarea value to the code stored in local storage.
        (document.getElementById("code-editor-textarea") as HTMLTextAreaElement).value = rawCode;

        // Load highlight.js and highlight the initial code.
        import("highlight.js/lib/core").then(({ default: _hljs }) => {
            import("highlight.js/lib/languages/haskell").then(({ default: haskell }) => {
                if (_hljs.getLanguage("haskell") === undefined)
                    _hljs.registerLanguage("haskell", haskell);

                setHljs(_hljs);
                setDisplayCode(highlightCode(rawCode, _hljs));
            }).catch(() => undefined);
        }).catch(() => undefined);
    }, []);

    // Render the editor.
    return (
        <Paper className={styles.editor!}>
            <pre id="code-editor-pre">{displayCode}</pre>
            <textarea id="code-editor-textarea" onChange={handleChange} onScroll={handleScroll} />
        </Paper>
    );
}
