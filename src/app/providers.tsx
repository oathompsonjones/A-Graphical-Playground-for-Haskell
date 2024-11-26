"use client";

import { type ReactNode, useState } from "react";
import { ThemeContextProvider } from "contexts/theme";
import type { User } from "database/schemas/user";
import { UserContext } from "contexts/user";

/**
 * Contains any context providers.
 * @param props - The props to pass to the layout.
 * @param props.children - The children to render.
 * @returns The context providers.
 */
export function Providers({ children }: { children: ReactNode; }): ReactNode {
    const [user, setUser] = useState<User | null>(null);

    return (
        <ThemeContextProvider>
            <UserContext.Provider value={{ setUser, user }}>
                {children}
            </UserContext.Provider>
        </ThemeContextProvider>
    );
}
