"use client";

import type { Dispatch, ReactNode, SetStateAction } from "react";
import { createContext, useContext, useEffect } from "react";
import { UserContext } from "./user";
import { compressToEncodedURIComponent } from "lz-string";
import { useLocalStorage } from "hooks/useLocalStorage";

export const SketchContext = createContext<{
    open: boolean;
    saved: boolean;
    setSaved: Dispatch<SetStateAction<boolean>>;
    id: string | null;
    setId: Dispatch<SetStateAction<string | null>>;
    title: string;
    setTitle: Dispatch<SetStateAction<string>>;
    code: string;
    setCode: Dispatch<SetStateAction<string>>;
    updateCode: (rawCode: string) => void;
    author: string | null;
    setAuthor: Dispatch<SetStateAction<string | null>>;
    resetSketch: () => void;
}>(null!);

/**
 * The user context provider, which wraps the default provider, and sets the initial state.
 * @param props - The props to pass to the component.
 * @param props.children - The children to render.
 * @returns The wrapped user context provider.
 */
export function SketchContextProvider({ children }: { children: ReactNode; }): ReactNode {
    const { user } = useContext(UserContext);

    const defaults = {
        author: user === null ? null : user.username ?? user.email.split("@")[0]!,
        code: compressToEncodedURIComponent([
            "import Lib",
            "",
            "-- Start writing your code here.",
            "main :: IO ()",
            "main = render $ background LightGrey (createCanvas 800 600)",
            "",
        ].join("\n")),
        id: null,
        open: false,
        saved: true,
        title: "untitled",
    };

    const [open, setOpen, resetOpen] = useLocalStorage("open", defaults.open);
    const [saved, setSaved, resetSaved] = useLocalStorage("saved", defaults.saved);
    const [id, setId, resetId] = useLocalStorage<string | null>("id", defaults.id);
    const [title, setTitle, resetTitle] = useLocalStorage("title", defaults.title);
    const [code, setCode, resetCode] = useLocalStorage("code", defaults.code);
    const updateCode = (rawCode: string): void => setCode(compressToEncodedURIComponent(rawCode));
    const [author, setAuthor] = useLocalStorage<string | null>("author", defaults.author);

    const resetSketch = (): void => {
        resetTitle();
        resetCode();
        resetSaved();
        resetOpen();
        resetId();
        setAuthor(user?.username ?? user?.email.split("@")[0] ?? null);
    };

    useEffect(() => {
        if (code !== defaults.code) {
            setOpen(true);
            setSaved(false);
        }
    }, [code]);

    return (
        <SketchContext.Provider value={{
            author,
            code,
            id,
            open,
            resetSketch,
            saved,
            setAuthor,
            setCode,
            setId,
            setSaved,
            setTitle,
            title,
            updateCode,
        }}>
            {children}
        </SketchContext.Provider>
    );
}
