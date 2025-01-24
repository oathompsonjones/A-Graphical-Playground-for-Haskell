"use client";

import { createContext, useEffect, useState } from "react";
import type { ReactNode } from "react";
import type { User } from "schemas/database";
import { getUser } from "database/index";

export const UserContext = createContext<{ user: User | null; setUser: (user: User | null) => void; }>(null!);

/**
 * The user context provider, which wraps the default provider, and sets the initial state.
 * @param props - The props to pass to the component.
 * @param props.children - The children to render.
 * @returns The wrapped user context provider.
 */
export function UserContextProvider({ children }: { children: ReactNode; }): ReactNode {
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
        <UserContext.Provider value={{ setUser, user }}>
            {children}
        </UserContext.Provider>
    );
}
