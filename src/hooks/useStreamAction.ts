"use client";

import { useState } from "react";

/**
 * Generator function that streams the response body from a fetch request.
 * @template T - The type of the response body.
 * @param reader - The stream to read from.
 * @returns An async generator of strings.
 * @yields A string.
 */
async function* readStream<T>(reader: ReadableStreamDefaultReader<T>): AsyncGenerator<T, void, unknown> {
    for (;;) {
        // eslint-disable-next-line no-await-in-loop
        const { done, value } = await reader.read();

        if (done)
            break;

        yield value;
    }
}

/**
 * Custom hook that streams data from an external resource.
 * @template T - The type of the data.
 * @template U - The type of the arguments.
 * @param action - The action which returns the data stream.
 * @param action.onExecute - The function to execute the stream.
 * @param action.onCancel - The function to cancel the stream.
 * @returns A 4-tuple containing the streamed data, the function to execute the stream,
 * the function to terminate the stream, and the function to clear the streamed data.
 */
export function useStreamAction<T, U extends unknown[]>(
    action: (...args: U) => Promise<ReadableStream<T>>,
): [T[], (...args: Parameters<typeof action>) => void, () => void, () => void] {
    const [data, setData] = useState<T[]>([]);
    const [streamReader, setStreamReader] = useState<ReadableStreamDefaultReader<T> | null>(null);

    /** Terminates the stream. */
    function terminateStream(): void {
        streamReader?.releaseLock();
    }

    /** Clears the streamed data. */
    function clearStream(): void {
        setData([]);
    }

    /**
     * Runs the action and streams the data.
     * @param args - The arguments to pass to the action.
     */
    function executeStream(...args: Parameters<typeof action>): void {
        terminateStream();
        clearStream();

        void (async (): Promise<void> => {
            const stream = await action(...args);
            const _streamReader = stream.getReader();

            setStreamReader(_streamReader);

            for await (const value of readStream(_streamReader))
                setData((prev) => [...prev, value]);
        })();
    }

    return [data, executeStream, terminateStream, clearStream];
}
