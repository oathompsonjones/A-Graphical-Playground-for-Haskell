"use client";

import { Alert, Icon, IconButton, Stack, Typography } from "@mui/material";
import { createContext, useEffect, useState } from "react";
import type { AlertColor } from "@mui/material";
import { Close } from "@mui/icons-material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/contexts/notifications.module.css";

export const NotificationsContext = createContext<{
    setNotification: (type: AlertColor, message: ReactNode) => void;
}>(null!);

/**
 * The notifications context provider, which wraps the default provider, and sets the initial state.
 * @param props - The props to pass to the component.
 * @param props.children - The children to render.
 * @returns The wrapped notifications context provider.
 */
export function NotificationsContextProvider({ children }: { children: ReactNode; }): ReactNode {
    const defaultMessage = <>This website uses cookies. <Link href="/privacy">Learn more</Link></>;
    const defaultCloseIcon = <Close />;
    const defaultType: AlertColor = "info";

    const [state, setState] = useState<{
        closeIcon: ReactNode;
        message: ReactNode;
        type: AlertColor;
        timeouts: NodeJS.Timeout[];
    } | null>({
        closeIcon: defaultCloseIcon,
        message: defaultMessage,
        timeouts: Array<NodeJS.Timeout>(4).fill(null!),
        type: defaultType,
    });

    const setCloseIcon = (closeIcon: ReactNode): void => setState((prev) => ({
        closeIcon,
        message: prev?.message ?? defaultMessage,
        timeouts: prev?.timeouts ?? [],
        type: prev?.type ?? defaultType,
    }));

    const setStateTimeout = (time: number, callback: () => void): void => {
        const timeout = setTimeout(callback, time);

        setState((prev) => ({
            closeIcon: prev?.closeIcon ?? defaultCloseIcon,
            message: prev?.message ?? defaultMessage,
            timeouts: (prev?.timeouts ?? []).concat(timeout),
            type: prev?.type ?? defaultType,
        }));
    };

    const clearTimeouts = (): void => state?.timeouts.forEach(clearTimeout);

    const setNotification = (type: AlertColor, message: ReactNode): void => {
        clearTimeouts();
        setState({ closeIcon: defaultCloseIcon, message, timeouts: [], type });
    };

    const onClick = (): void => setState(null);

    useEffect(() => {
        clearTimeouts();

        if (state?.message === null)
            return;

        setStateTimeout(7000, () => {
            setCloseIcon(<Icon>3</Icon>);
            setStateTimeout(1000, () => {
                setCloseIcon(<Icon>2</Icon>);
                setStateTimeout(1000, () => {
                    setCloseIcon(<Icon>1</Icon>);
                    setStateTimeout(1000, () => {
                        setState(null);
                    });
                });
            });
        });
    }, [state?.message]);

    return (
        <NotificationsContext.Provider value={{ setNotification }}>
            {children}
            {state !== null && (
                <Alert className={styles.notifications!} severity={state.type}>
                    <Stack direction="row" spacing={1} alignItems="center">
                        <Typography>{state.message}</Typography>
                        <IconButton className={styles.close!} onClick={onClick}>{state.closeIcon}</IconButton>
                    </Stack>
                </Alert>
            )}
        </NotificationsContext.Provider>
    );
}
