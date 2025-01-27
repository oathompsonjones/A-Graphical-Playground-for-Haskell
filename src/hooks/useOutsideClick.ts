"use client";

import { useEffect, useRef } from "react";
import type { RefObject } from "react";

/**
 * Handles clicks outside of a component.
 * @template T - The type of the ref object.
 * @param callback - Function to execute when the user clicks outside of the component.
 * @returns The ref object to pass to the component.
 */
export function useOutsideClick<T extends HTMLElement>(callback: () => unknown): RefObject<T> {
    const ref = useRef<T>(null);

    useEffect(() => {
        const handleClickOutside = (event: MouseEvent | TouchEvent): void => {
            if (ref.current !== null && !ref.current.contains(event.target as Node))
                callback();
        };

        document.addEventListener("mousedown", handleClickOutside);
        document.addEventListener("touchstart", handleClickOutside);

        return (): void => {
            document.removeEventListener("mousedown", handleClickOutside);
            document.removeEventListener("touchstart", handleClickOutside);
        };
    }, [ref]);

    return ref as RefObject<T>;
}
