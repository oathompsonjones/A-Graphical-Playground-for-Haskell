"use client";

import { useLayoutEffect, useState } from "react";

/**
 * Returns the current window size.
 * @returns An object containing the current window size.
 */
export function useWindowSize(): { height: number; width: number; } {
    const [windowSize, setWindowSize] = useState<{ height: number; width: number; }>(() => ({
        height: typeof window === "undefined" ? 0 : window.innerHeight,
        width: typeof window === "undefined" ? 0 : window.innerWidth,
    }));

    useLayoutEffect(() => {
        const onResize = (): void => setWindowSize({
            height: window.innerHeight,
            width: window.innerWidth,
        });

        window.addEventListener("resize", onResize);
        onResize();

        return (): void => window.removeEventListener("resize", onResize);
    }, []);

    return windowSize;
}
