import "styles/codeTheme.css";
import type { FormEvent, KeyboardEvent, ReactNode, UIEvent } from "react";
import { memo, useContext } from "react";
import { PlainPaper } from "./plainPaper";
import { SketchContext } from "contexts/sketch";
import { decompressFromEncodedURIComponent } from "lz-string";
import { renderToString } from "react-dom/server";
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
 * @param props.save - The function to call when the user saves the code.
 * @param props.open - The function to call when the user opens a file.
 * @param props.new - The function to call when the user creates a new file.
 * @param props.run - The function to call when the user runs the code.
 * @returns The editor element.
 */
// eslint-disable-next-line max-lines-per-function
function EditorComponent({ save, open, new: new_, run }: {
    save: () => void;
    open: () => void;
    new: () => void;
    run: () => void;
}): ReactNode {
    const { code: compressedCode, setSaved, updateCode } = useContext(SketchContext);
    const code = decompressFromEncodedURIComponent(compressedCode);

    /**
     * Highlight the code and update the display.
     * We use separate <code> elements for each line to render line numbers.
     * This runs automatically on each render. As the code changes, it updates the parent component's state, triggering
     * a rerender of this component too, thus updating the display.
     * @param pre - The pre element to update.
     */
    const updateDisplayCode = (pre: HTMLPreElement | null): void => {
        if (pre === null)
            return;

        import("highlight.js/lib/core").then(({ default: _hljs }) => {
            const setDisplayCode = (): void => {
                pre.innerHTML = code.split("\n").map((line) => renderToString(
                    <code
                        className="language-haskell"
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        dangerouslySetInnerHTML={{ __html: _hljs.highlight(line, { language: "haskell" }).value }}
                    />,
                )).join("");
            };

            // This only runs when the component is first rendered.
            if (_hljs.getLanguage("haskell") === undefined) {
                import("highlight.js/lib/languages/haskell").then(({ default: haskell }) => {
                    _hljs.registerLanguage("haskell", haskell);

                    setDisplayCode();
                }).catch(() => undefined);
            } else {
                setDisplayCode();
            }
        }).catch(() => undefined);
    };

    /**
     * Handle changes to the code.
     * @param event - The form event.
     */
    const handleChange = (event: FormEvent<HTMLTextAreaElement>): void => {
        // Ignore the event if the code hasn't changed.
        if (event.currentTarget.value === code)
            return;

        // Prevent macOS double space inserting a dot.
        if ("inputType" in event.nativeEvent && event.nativeEvent.inputType === "insertReplacementText" &&
            "data" in event.nativeEvent && typeof event.nativeEvent.data === "string" && event.nativeEvent.data === ". "
        ) {
            const textarea = event.currentTarget;
            const { selectionEnd: end, selectionStart: start } = textarea;

            textarea.value = `${textarea.value.substring(0, start - 2)}  ${textarea.value.substring(end)}`;
            textarea.selectionStart = start;
            textarea.selectionEnd = start;
        }

        // Update the code and display.
        updateCode(event.currentTarget.value);

        // The code has changed, so it's no longer saved.
        setSaved(false);
    };

    /**
     * Synchronise scrolling between the textarea and pre elements.
     * @param event - The UI scroll event.
     * @param event.currentTarget - The textarea element.
     * @param event.currentTarget.scrollLeft - The horizontal scroll position of the textarea.
     * @param event.currentTarget.scrollTop - The vertical scroll position of the textarea.
     */
    const handleScroll = ({ currentTarget: { scrollLeft, scrollTop } }: UIEvent<HTMLTextAreaElement>): void => {
        const pre = document.getElementById("pre");

        if (pre !== null) {
            pre.scrollTop = scrollTop;
            pre.scrollLeft = scrollLeft;
        }
    };

    /**
     * Handle keyboard shortcuts and special character behaviours.
     * @param event - The keyboard event.
     */
    // eslint-disable-next-line max-statements
    const handleKey = (event: KeyboardEvent<HTMLTextAreaElement>): void => {
        const isMacOS = navigator.platform.includes("Mac");
        const controlKey = isMacOS ? event.metaKey : event.ctrlKey;

        const textarea = event.currentTarget;
        const { selectionEnd: end, selectionStart: start } = textarea;

        if (controlKey) {
            switch (event.key) {
                // Comment out the selected lines/current line.
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
                // Save the sketch.
                case "s":
                    event.preventDefault();
                    save();
                    break;
                // Open a sketch.
                case "o":
                    event.preventDefault();
                    open();
                    break;
                // Create a new sketch.
                case "n":
                    event.preventDefault();
                    new_();
                    break;
                // Run the sketch.
                case "Enter":
                    event.preventDefault();
                    run();
                    break;
            }
        } else {
            switch (event.key) {
                // Apply correct indentation to new lines.
                case "Enter": {
                    event.preventDefault();
                    const line = textarea.value.substring(0, start).split("\n").pop()!;
                    const [indent] = (/^\s*/).exec(line)!;

                    textarea.value = `${textarea.value.substring(0, start)}\n${indent}${textarea.value.substring(end)}`;
                    textarea.selectionStart = start + 1 + indent.length;
                    textarea.selectionEnd = start + 1 + indent.length;

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
                // Indent the selected lines/current line.
                case "Tab": {
                    // TODO: Indent multiple lines at once.
                    event.preventDefault();
                    const cursorColumn = textarea.value.substring(0, start).split("\n").pop()!.length;
                    const spaceCount = cursorColumn % 2 === 0 ? 2 : 1;

                    textarea.value = textarea.value.substring(0, start) +
                        " ".repeat(spaceCount) +
                        textarea.value.substring(end);

                    textarea.selectionStart = start + spaceCount;
                    textarea.selectionEnd = start + spaceCount;

                    handleChange(event as FormEvent<HTMLTextAreaElement>);
                    break;
                }
                // Automatically close angle brackets only when wrapping text.
                case "<":
                    if (start === end)
                        break;
                // Automatically close brackets.
                case "(": case "[": case "{": {
                    // TODO: Don't close brackets if the cursor is directly before a non-whitespace character.
                    // TODO: When deleting opening brackets, delete closing pair if immediately after opening.
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
                // Allow typing over closing brackets.
                case ")": case "]": case "}": case ">": {
                    if (textarea.value[end] === event.key) {
                        event.preventDefault();
                        textarea.selectionStart = end + 1;
                        textarea.selectionEnd = end + 1;

                        handleChange(event as FormEvent<HTMLTextAreaElement>);
                    }

                    break;
                }
                // Automatically close quotes and allow typing over closing quotes.
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

    /**
     * Set the initial value of the textarea.
     * @param textarea - The textarea element.
     */
    const setCode = (textarea: HTMLTextAreaElement | null): void => {
        if (textarea !== null)
            textarea.value = code;
    };

    return (
        <PlainPaper className={styles.editor!}>
            <pre id="pre" ref={updateDisplayCode} />
            <textarea
                spellCheck={false}
                onKeyDown={handleKey} onChange={handleChange} onScroll={handleScroll} ref={setCode} />
        </PlainPaper>
    );
}

export const Editor = memo(EditorComponent);
