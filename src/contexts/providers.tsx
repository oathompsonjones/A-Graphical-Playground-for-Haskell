"use client";

import { NotificationsContextProvider } from "contexts/notifications";
import type { ReactNode } from "react";
import { SketchContextProvider } from "./sketch";
import { ThemeContextProvider } from "contexts/theme";
import { UserContextProvider } from "./user";

/**
 * Contains any context providers.
 * @param props - The props to pass to the layout.
 * @param props.children - The children to render.
 * @returns The context providers.
 */
export function Providers({ children }: { children: ReactNode; }): ReactNode {
    return (
        <ThemeContextProvider>
            <NotificationsContextProvider>
                <UserContextProvider>
                    <SketchContextProvider>
                        {children}
                    </SketchContextProvider>
                </UserContextProvider>
            </NotificationsContextProvider>
        </ThemeContextProvider>
    );
}
