import Link from "next/link";
import type { ReactNode } from "react";

/**
 * Handles any 404 errors.
 * @returns An error page.
 */
export default function Error(): ReactNode {
    return (
        <>
            <h2>Error 404 - Page not found</h2>
            <p>Click <Link href="/">here</Link> to go to the homepage.</p>
        </>
    );
}
