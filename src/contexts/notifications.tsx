"use client";

import { Alert, Icon, IconButton } from "@mui/material";
import { createContext, useEffect, useState } from "react";
import type { AlertColor } from "@mui/material";
import { Close } from "@mui/icons-material";
import Link from "next/link";
import type { ReactNode } from "react";
import styles from "styles/contexts/notifications.module.css";

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
    const [closeIcon, setCloseIcon] = useState(<Close />);
    const [timeouts, setTimeouts] = useState<NodeJS.Timeout[]>(Array(4).fill(null));

    const setStateTimeout = (index: number, time: number, callback: () => void): NodeJS.Timeout => {
        const timeout = setTimeout(callback, time);

        setTimeouts((prevTimeouts) => {
            prevTimeouts[index] = timeout;

            return prevTimeouts;
        });

        return timeout;
    };

    const setCloseTimeout = setStateTimeout.bind(null, 0, 1000);
    const setOneSecondTimeout = setStateTimeout.bind(null, 1, 1000);
    const setTwoSecondTimeout = setStateTimeout.bind(null, 2, 1000);
    const setThreeSecondTimeout = setStateTimeout.bind(null, 3, 7000);

    useEffect(() => {
        for (const timeout of timeouts)
            clearTimeout(timeout);

        if (message === null)
            return;

        setThreeSecondTimeout(() => {
            setCloseIcon(<Icon>3</Icon>);
            setTwoSecondTimeout(() => {
                setCloseIcon(<Icon>2</Icon>);
                setOneSecondTimeout(() => {
                    setCloseIcon(<Icon>1</Icon>);
                    setCloseTimeout(() => {
                        setMessage(null);
                        setCloseIcon(<Close />);
                    });
                });
            });
        });
    }, [message]);

    return (
        <NotificationsContext.Provider value={{ message, setMessage, setType, type }}>
            {children}
            {message !== null && (
                <Alert className={styles.notifications!} severity={type}>
                    {message}
                    <IconButton className={styles.close!} onClick={() => setMessage(null)}>
                        {closeIcon}
                    </IconButton>
                </Alert>
            )}
        </NotificationsContext.Provider>
    );
}
