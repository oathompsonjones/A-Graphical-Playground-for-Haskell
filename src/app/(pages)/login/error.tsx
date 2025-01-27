"use client";

import LogIn from "./page";
import type { ReactNode } from "react";

/**
 * Handles errors for the page.
 * @param props - The component properties.
 * @param props.error - The error that occurred.
 * @param props.reset - The function to reset the application.
 * @returns An error element.
 */
export default function ErrorPage({ error }: { error: unknown; reset: () => void; }): ReactNode {
    if (!(error instanceof Error))
        return <LogIn error={new Error("An unexpected error occured.")} />;

    return (
        <LogIn error={
            error.message.startsWith("authenticate-") || error.message.startsWith("register-")
                ? new Error(error.message.split("-")[1], { cause: error.message.split("-")[0] })
                : new Error(error.message)
        } />
    );
}
