"use client";

import { createContext, useState } from "react";
import type { AlertColor } from "@mui/material";
import Link from "next/link";
import { NotificationsBanner } from "components/contexts/notificationBanner";
import type { ReactNode } from "react";

export const NotificationsContext = createContext<{
    message: ReactNode;
    setMessage: (message: ReactNode) => void;
    setType: (type: AlertColor) => void;
    type: AlertColor;
}>(null!);

/**
 * The notifications context provider, which wraps the default provider, and sets the initial state.
 * @param props - The props to pass to the component.
 * @param props.children - The children to render.
 * @returns The wrapped notifications context provider.
 */
export function NotificationsContextProvider({ children }: { children: ReactNode; }): ReactNode {
    const [message, setMessage] = useState<ReactNode>(<>
        This website uses cookies. <Link href="/privacy">Learn more</Link>
    </>);
    const [type, setType] = useState<AlertColor>("info");

    return (
        <NotificationsContext.Provider value={{ message, setMessage, setType, type }}>
            {children}
            {message !== null && <NotificationsBanner message={message} setMessage={setMessage} type={type} />}
        </NotificationsContext.Provider>
    );
}
