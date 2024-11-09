"use client";

import "styles/components/codeTheme.css";
import type { Dispatch, FormEvent, KeyboardEvent, ReactNode, SetStateAction, UIEvent } from "react";
import { useEffect, useState } from "react";
import type { HLJSApi } from "highlight.js";
import { PlainPaper } from "./plainPaper";
import styles from "styles/components/editor.module.css";

/*
TODO:

* Keyboard controls
    * Support shortcuts like Ctrl+S to save, Ctrl+Enter to run, etc.
* Livelits
    * Number sliders
    * Colour pickers
* Allow the Haskell code to be executed, displaying text output in the console.
* Add accounts to allow users to save their code.
    * Set up database
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
 * @param props - The properties of the editor.
 * @param props.code - The initial code to display.
 * @param props.updateCode - The function to call when the code changes.
 * @param props.run - The function to call when the code is run.
 * @param props.save - The function to call when the code is saved.
 * @returns The editor element.
 */
export function Editor({ code, updateCode, run, save }: {
    code: string;
    updateCode: Dispatch<SetStateAction<string>>;
    run: () => void;
    save: () => void;
}): ReactNode {
    // Setup state variables.
    const [hljs, setHljs] = useState<HLJSApi>(null!);
    const [displayCode, setDisplayCode] = useState<ReactNode>(null);

    /**
     * Highlights the code.
     * @param _code - The code to highlight.
     * @param _hljs - The highlight.js instance. If not provided, the state variable will be used.
     * @returns The highlighted code.
     */
    function highlightCode(_code: string, _hljs: HLJSApi = hljs): ReactNode {
        return _code.split("\n").map((line, i) => (
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
        // Ignore the event if the code hasn't changed.
        if (event.currentTarget.value === code)
            return;

        // Prevent macOS double space inserting a dot.
        if (event.nativeEvent.type === "input" && "data" in event.nativeEvent &&
            typeof event.nativeEvent.data === "string" && event.nativeEvent.data.length > 1
        ) {
            const textarea = event.currentTarget;
            const { selectionEnd: end, selectionStart: start } = textarea;
            const inputSize = event.nativeEvent.data.length;

            textarea.value = `${textarea.value.substring(0, start - inputSize)}  ${textarea.value.substring(end)}`;
            textarea.selectionStart = start + inputSize;
            textarea.selectionEnd = textarea.selectionStart;
        }

        // Update the code and display.
        updateCode(event.currentTarget.value);
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

    /**
     * Inserts 2 spaces when the tab key is pressed.
     * @param event - The keyboard event.
     */
    function handleTab(event: KeyboardEvent<HTMLTextAreaElement>): void {
        event.preventDefault();

        const tabSize = 2;
        const textarea = event.currentTarget;
        const { selectionEnd: end, selectionStart: start } = textarea;

        textarea.value = textarea.value.substring(0, start) +
                    " ".repeat(tabSize) + textarea.value.substring(end);
        textarea.selectionStart = start + tabSize;
        textarea.selectionEnd = textarea.selectionStart;

        handleChange(event as FormEvent<HTMLTextAreaElement>);
    }

    /**
     * Comments out the current line when the slash key is pressed with the command/control key.
     * @param event - The keyboard event.
     */
    function handleSlash(event: KeyboardEvent<HTMLTextAreaElement>): void {
        event.preventDefault();

        const textarea = event.currentTarget;
        const beforeText = textarea.value.substring(0, textarea.selectionStart);
        const lineStart = beforeText.lastIndexOf("\n") + 1;
        const lineIndex = beforeText.split("").filter((char) => char === "\n").length;
        const line = textarea.value.split("\n")[lineIndex];

        if (line === undefined)
            return;

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
    }

    /**
     * Saves the code when the s key is pressed with the command/control key.
     * @param event - The keyboard event.
     */
    function handleS(event: KeyboardEvent<HTMLTextAreaElement>): void {
        event.preventDefault();
        save();
    }

    /**
     * Runs the code when the enter key is pressed with the command/control key.
     * @param event - The form event.
     */
    function handleEnter(event: KeyboardEvent<HTMLTextAreaElement>): void {
        event.preventDefault();
        run();
    }

    /**
     * Handles key presses, allowing for keyboard shortcuts and use of the tab key.
     * @param event - The keyboard event.
     */
    function handleKey(event: KeyboardEvent<HTMLTextAreaElement>): void {
        const isMacOS = navigator.platform.includes("Mac");
        const controlKey = isMacOS ? event.metaKey : event.ctrlKey;

        switch (event.key) {
            case "Tab":
                handleTab(event);
                break;
            case "/":
                if (controlKey)
                    handleSlash(event);

                break;
            case "s":
                if (controlKey)
                    handleS(event);

                break;
            case "Enter":
                if (controlKey)
                    handleEnter(event);

                break;
        }
    }

    // Initialize
    useEffect(() => {
        // Set the textarea value to the code stored in local storage.
        (document.getElementById("code-editor-textarea") as HTMLTextAreaElement).value = code;

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

    // Render the editor.
    return (
        <PlainPaper className={styles.editor!}>
            <pre id="code-editor-pre">{displayCode}</pre>
            <textarea id="code-editor-textarea" onKeyDown={handleKey} onChange={handleChange} onScroll={handleScroll} />
        </PlainPaper>
    );
}
