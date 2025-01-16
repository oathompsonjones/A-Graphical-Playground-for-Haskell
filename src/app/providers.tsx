"use client";

import { useEffect, useState } from "react";
import type { AlertColor } from "@mui/material";
import Link from "next/link";
import { NotificationsContext } from "contexts/notifications";
import type { ReactNode } from "react";
import { ThemeContextProvider } from "contexts/theme";
import type { User } from "database/schemas/user";
import { UserContext } from "contexts/user";
import { getUser } from "database/database";

/**
 * Contains any context providers.
 * @param props - The props to pass to the layout.
 * @param props.children - The children to render.
 * @returns The context providers.
 */
export function Providers({ children }: { children: ReactNode; }): ReactNode {
    // Notifications context
    const [message, setMessage] = useState<ReactNode>(<>
        This website uses cookies. <Link href="/privacy">Learn more</Link>
    </>);
    const [type, setType] = useState<AlertColor>("info");

    // User context
    const [user, setUser] = useState<User | null>(null);

    useEffect(() => {
        const cookies = Object.fromEntries(document.cookie
            .split(";").map((c) => c.trim().split("=") as [string, string]));

        if ("user" in cookies) {
            getUser(decodeURIComponent(cookies.user))
                .then(setUser)
                .catch(() => undefined);
        }
    }, []);

    return (
        <ThemeContextProvider>
            <UserContext.Provider value={{ setUser, user }}>
                <NotificationsContext.Provider value={{ message, setMessage, setType, type }}>
                    {children}
                </NotificationsContext.Provider>
            </UserContext.Provider>
        </ThemeContextProvider>
    );
}
