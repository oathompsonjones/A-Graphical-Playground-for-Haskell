"use client";

import type { Dispatch, ReactNode, SetStateAction } from "react";
import { createContext, useContext } from "react";
import { UserContext } from "./user";
import { compressToEncodedURIComponent } from "lz-string";
import { useLocalStorage } from "hooks/useLocalStorage";

export const SketchContext = createContext<{
    saved: boolean;
    setSaved: Dispatch<SetStateAction<boolean>>;
    resetSaved: () => void;
    id: string | null;
    setId: Dispatch<SetStateAction<string | null>>;
    resetId: () => void;
    title: string;
    setTitle: Dispatch<SetStateAction<string>>;
    resetTitle: () => void;
    code: string;
    setCode: Dispatch<SetStateAction<string>>;
    resetCode: () => void;
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

    const [saved, setSaved, resetSaved] = useLocalStorage("saved", false);
    const [id, setId, resetId] = useLocalStorage<string | null>("id", null);
    const [title, setTitle, resetTitle] = useLocalStorage("title", "untitled");
    const [code, setCode, resetCode] = useLocalStorage("code", compressToEncodedURIComponent([
        "import Lib",
        "",
        "-- Start writing your code here.",
        "main :: IO ()",
        "main = render $ background LightGrey (createCanvas 800 600)",
        "",
    ].join("\n")));
    const updateCode = (rawCode: string): void => setCode(compressToEncodedURIComponent(rawCode));
    const [author, setAuthor] = useLocalStorage<string | null>("author", user === null
        ? null
        : user.username ?? user.email.split("@")[0]!);

    const resetSketch = (): void => {
        resetTitle();
        resetCode();
        resetSaved();
        resetId();
        setAuthor(user?.username ?? user?.email.split("@")[0] ?? null);
    };

    return (
        <SketchContext.Provider value={{
            author,
            code,
            id,
            resetCode,
            resetId,
            resetSaved,
            resetSketch,
            resetTitle,
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
