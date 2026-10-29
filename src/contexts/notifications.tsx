"use client";

import { Alert, IconButton, Stack, Typography } from "@mui/material";
import { createContext, useCallback, useEffect, useState } from "react";
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
export function NotificationsContextProvider({ children }: { readonly children: ReactNode; }): ReactNode {
    const [state, setState] = useState<{ message: ReactNode; type: AlertColor; } | null>({
        message: <>This website uses cookies. <Link href="/privacy">Learn more</Link></>,
        type: "info",
    });
    const [opaque, setOpaque] = useState(true);
    const [hover, setHover] = useState(false);

    const setNotification = (message: ReactNode, type: AlertColor = "info"): void => setState({ message, type });
    const onClick = useCallback((): void => setState(null), []);

    useEffect(() => {
        const timeout = setTimeout(() => setOpaque(false), 5_000);

        setOpaque(true);
        setHover(false);

        return (): void => clearTimeout(timeout);
    }, [state]);

    const enableHover = useCallback((): void => setHover(true), []);
    const disableHover = useCallback((): void => setHover(false), []);

    return (
        <NotificationsContext.Provider value={{ setNotification }}>
            {children}
            {state !== null && (
                <Alert
                    className={`${styles.notifications} ${opaque || hover ? "" : styles.translucent}`}
                    onMouseEnter={enableHover} onMouseLeave={disableHover} severity={state.type}
                >
                    <Stack direction="row" spacing={1} sx={{ alignItems: "center" }}>
                        <Typography>{state.message}</Typography>
                        <IconButton className={styles.close} onClick={onClick}>
                            <Close />
                        </IconButton>
                    </Stack>
                </Alert>
            )}
        </NotificationsContext.Provider>
    );
}
