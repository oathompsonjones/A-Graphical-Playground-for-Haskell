"use client";

import { Alert, IconButton, Stack, Typography } from "@mui/material";
import { createContext, useState } from "react";
import type { AlertColor } from "@mui/material";
import { Close } from "@mui/icons-material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/contexts/notifications.module.css";

export const NotificationsContext = createContext<{
    setNotification: (message: ReactNode, type?: AlertColor) => void;
}>(null!);

/**
 * The notifications context provider, which wraps the default provider, and sets the initial state.
 * @param props - The props to pass to the component.
 * @param props.children - The children to render.
 * @returns The wrapped notifications context provider.
 */
export function NotificationsContextProvider({ children }: { children: ReactNode; }): ReactNode {
    const [state, setState] = useState<{ message: ReactNode; type: AlertColor; } | null>({
        message: <>This website uses cookies. <Link href="/privacy">Learn more</Link></>,
        type: "info",
    });

    const setNotification = (message: ReactNode, type: AlertColor = "info"): void => {
        setState({ message, type });
    };

    const onClick = (): void => setState(null);

    return (
        <NotificationsContext.Provider value={{ setNotification }}>
            {children}
            {state !== null && (
                <Alert className={styles.notifications!} severity={state.type}>
                    <Stack direction="row" spacing={1} alignItems="center">
                        <Typography>{state.message}</Typography>
                        <IconButton className={styles.close!} onClick={onClick}>
                            <Close />
                        </IconButton>
                    </Stack>
                </Alert>
            )}
        </NotificationsContext.Provider>
    );
}
