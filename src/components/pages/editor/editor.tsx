"use client";

import "styles/codeTheme.css";
import type { FormEvent, KeyboardEvent, ReactNode, UIEvent } from "react";
import { useEffect, useState } from "react";
import type { HLJSApi } from "highlight.js";
import { PlainPaper } from "./plainPaper";
import styles from "styles/components/pages/editor/editor.module.css";

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
 * @param props - The properties of the editor.
 * @param props.code - The initial code to display.
 * @param props.updateCode - The function to call when the code changes.
 * @param props.save - The function to call when the user saves the code.
 * @param props.open - The function to call when the user opens a file.
 * @param props.new - The function to call when the user creates a new file.
 * @param props.run - The function to call when the user runs the code.
 * @returns The editor element.
 */
// eslint-disable-next-line max-lines-per-function
export function Editor({ code, updateCode, save, open, new: new_, run }: {
    code: string;
    updateCode: (rawCode: string) => void;
    save: () => void;
    open: () => void;
    new: () => void;
    run: () => void;
}): ReactNode {
    // Setup state variables.
    const [hljs, setHljs] = useState<HLJSApi>(null!);
    const [displayCode, setDisplayCode] = useState<ReactNode>(null);

    const highlightCode = (_code: string, _hljs: HLJSApi = hljs): ReactNode => _code.split("\n").map((line, i) => (
        <code
            key={i}
            className="language-haskell"
            // eslint-disable-next-line @typescript-eslint/naming-convention
            dangerouslySetInnerHTML={{ __html: _hljs.highlight(line, { language: "haskell" }).value }}
        />
    ));

    const handleChange = (event: FormEvent<HTMLTextAreaElement>): void => {
        // Ignore the event if the code hasn't changed.
        if (event.currentTarget.value === code)
            return;

        // Make sure the library is imported.
        if (!code.startsWith("import HaskellGraphics\n")) {
            // TODO: Automatically add the import statement.
        }

        // Prevent macOS double space inserting a dot.
        if ("inputType" in event.nativeEvent && event.nativeEvent.inputType === "insertReplacementText" &&
            "data" in event.nativeEvent && typeof event.nativeEvent.data === "string" && event.nativeEvent.data === ". "
        ) {
            const textarea = event.currentTarget;
            const { selectionEnd: end, selectionStart: start } = textarea;

            textarea.value = `${textarea.value.substring(0, start - 2)}  ${textarea.value.substring(end)}`;
            textarea.selectionStart = start;
            textarea.selectionEnd = textarea.selectionStart;
        }

        // Update the code and display.
        updateCode(event.currentTarget.value);
        setDisplayCode(highlightCode(event.currentTarget.value));
    };

    const handleScroll = (event: UIEvent<HTMLTextAreaElement>): void => {
        const pre = document.getElementById("code-editor-pre");

        if (pre !== null) {
            pre.scrollTop = event.currentTarget.scrollTop;
            pre.scrollLeft = event.currentTarget.scrollLeft;
        }
    };

    // eslint-disable-next-line max-statements
    const handleKey = (event: KeyboardEvent<HTMLTextAreaElement>): void => {
        const isMacOS = navigator.platform.includes("Mac");
        const controlKey = isMacOS ? event.metaKey : event.ctrlKey;

        const textarea = event.currentTarget;
        const { selectionEnd: end, selectionStart: start } = textarea;

        if (controlKey) {
            switch (event.key) {
                case "/": {
                    // TODO: Put the comment syntax at the correct indent level.
                    // TODO: Comment multiple lines at once.
                    event.preventDefault();

                    const beforeText = textarea.value.substring(0, start);
                    const lineStart = beforeText.lastIndexOf("\n") + 1;
                    const lineIndex = beforeText.split("").filter((char) => char === "\n").length;
                    const line = textarea.value.split("\n")[lineIndex];

                    if (line === undefined)
                        break;

                    const beforeLines = textarea.value.substring(0, lineStart);
                    const afterLines = textarea.value.substring(lineStart + line.length);

                    if (line.startsWith("--")) {
                        const len = line.startsWith("-- ") ? 3 : 2;

                        textarea.value = `${beforeLines}${line.substring(len)}${afterLines}`;
                        textarea.selectionStart = lineStart + line.length - len;
                        textarea.selectionEnd = textarea.selectionStart;
                    } else {
                        textarea.value = `${beforeLines}-- ${line}${afterLines}`;
                        textarea.selectionStart = lineStart + line.length + 3;
                        textarea.selectionEnd = textarea.selectionStart;
                    }

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
                case "s":
                    event.preventDefault();
                    save();
                    break;
                case "o":
                    event.preventDefault();
                    open();
                    break;
                case "n":
                    event.preventDefault();
                    new_();
                    break;
                case "Enter":
                    event.preventDefault();
                    run();
                    break;
            }
        } else {
            switch (event.key) {
                case "Tab": {
                    // TODO: Indent multiple lines at once.
                    // TODO: Align the cursor with the correct indent level.
                    event.preventDefault();
                    textarea.value = `${textarea.value.substring(0, start)}  ${textarea.value.substring(end)}`;
                    textarea.selectionStart = start + 2;
                    textarea.selectionEnd = start + 2;

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
                case "(": case "[": case "{": case "<": {
                    if (event.key === "<" && start === end)
                        break;

                    event.preventDefault();

                    textarea.value = [
                        textarea.value.substring(0, start),
                        event.key,
                        textarea.value.slice(start, end),
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        { "(": ")", "<": ">", "[": "]", "{": "}" }[event.key],
                        textarea.value.substring(end),
                    ].join("");

                    textarea.selectionStart = start + 1;
                    textarea.selectionEnd = end + 1;

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
                case ")": case "]": case "}": case ">": {
                    if (textarea.value[end] === event.key) {
                        event.preventDefault();
                        textarea.selectionStart = end + 1;
                        textarea.selectionEnd = end + 1;

                        handleChange(event as FormEvent<HTMLTextAreaElement>);
                    }

                    break;
                }
                case "\"": case "'": case "`": {
                    event.preventDefault();

                    if (textarea.value[end] === event.key) {
                        textarea.selectionStart = end + 1;
                        textarea.selectionEnd = end + 1;
                    } else {
                        textarea.value = [
                            textarea.value.substring(0, start),
                            event.key,
                            textarea.value.slice(start, end),
                            event.key,
                            textarea.value.substring(end),
                        ].join("");

                        textarea.selectionStart = start + 1;
                        textarea.selectionEnd = end + 1;
                    }

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
            }
        }
    };

    const setValue = (textarea: HTMLTextAreaElement | null): void => {
        if (textarea !== null)
            textarea.value = code;
    };

    useEffect(() => {
        // Load highlight.js and highlight the initial code.
        import("highlight.js/lib/core").then(({ default: _hljs }) => {
            import("highlight.js/lib/languages/haskell").then(({ default: haskell }) => {
                if (_hljs.getLanguage("haskell") === undefined)
                    _hljs.registerLanguage("haskell", haskell);

                setHljs(_hljs);
                setDisplayCode(highlightCode(code, _hljs));
            }).catch(() => undefined);
        }).catch(() => undefined);
    }, [code]);

    return (
        <PlainPaper className={styles.editor!}>
            <pre id="code-editor-pre">{displayCode}</pre>
            <textarea onKeyDown={handleKey} onChange={handleChange} onScroll={handleScroll} ref={setValue} />
        </PlainPaper>
    );
}
