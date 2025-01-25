"use client";

import type { Dispatch, SetStateAction } from "react";
import { useEffect, useState } from "react";

/**
 * Provides the same behaviour as `useState`, but also stores data using localStorage.
 * @template T - The type of the value to store.
 * @param key - The name of the variable to store in localStorage.
 * @param initialValue - The initial value to store for that variable.
 * @returns The value stored in localStorage, a function to update that value, and a function to remove that value.
 */
export function useLocalStorage<T>(key: string, initialValue: T): [T, Dispatch<SetStateAction<T>>] {
    const parse = (value: string): T => {
        if (value === "undefined")
            return undefined as unknown as T;

        let parsed: unknown;

        try {
            parsed = JSON.parse(value);
        } catch (error) {
            return initialValue;
        }

        return parsed as T;
    };

    const readValue = (): T => {
        if (typeof window === "undefined")
            return initialValue;

        try {
            const raw = localStorage.getItem(key);

            return raw === null ? initialValue : parse(raw);
        } catch (error) {
            return initialValue;
        }
    };

    const [storedValue, setStoredValue] = useState(readValue);

    const setValue: Dispatch<SetStateAction<T>> = (value) => {
        if (typeof window !== "undefined") {
            // Allow value to be a function so we have the same API as useState
            const newValue = value instanceof Function ? value(readValue()) : value;

            // Save to local storage and state
            localStorage.setItem(key, JSON.stringify(newValue));
            setStoredValue(newValue);
        }
    };

    useEffect(() => setStoredValue(readValue), [key]);

    return [storedValue, setValue];
}
