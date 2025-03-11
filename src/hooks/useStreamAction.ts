"use client";

import { useRef, useState } from "react";

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
 * @template T - The type of the arguments.
 * @template U - The type of the streamed data.
 * @param action - The action which returns the data stream.
 * @param action.onExecute - The function to execute the stream.
 * @param action.onCancel - The function to cancel the stream.
 * @returns A 4-tuple containing the streamed data, the function to execute the stream,
 * the function to terminate the stream, and the function to clear the streamed data.
 */
export function useStreamAction<T extends unknown[], U>(
    action: (...args: T) => Promise<ReadableStream<U>>,
): [U[], (...args: T) => void, () => void, () => void] {
    const [data, setData] = useState<U[]>([]);
    const streamReader = useRef<ReadableStreamDefaultReader<U>>(null);

    /** Terminates the stream. */
    function terminateStream(): void {
        void streamReader.current?.cancel();
    }

    /** Clears the streamed data. */
    function clearStream(): void {
        setData([]);
    }

    /**
     * Runs the action and streams the data.
     * @param args - The arguments to pass to the action.
     */
    function executeStream(...args: T): void {
        terminateStream();
        clearStream();

        void (async (): Promise<void> => {
            streamReader.current = (await action(...args)).getReader();

            for await (const value of readStream(streamReader.current))
                setData((prev) => [...prev, value]);
        })();
    }

    return [data, executeStream, terminateStream, clearStream];
}
