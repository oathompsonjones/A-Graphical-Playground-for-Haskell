"use client";

import { Alert, Icon, IconButton, Stack, Typography } from "@mui/material";
import type { Dispatch, ReactNode, SetStateAction } from "react";
import { useEffect, useState } from "react";
import type { AlertColor } from "@mui/material";
import { Close } from "@mui/icons-material";
import styles from "styles/components/contexts/notifications.module.css";

/**
 * The notifications banner component, which displays a message to the user and automatically closes after 10 seconds.
 * @param props - The props to pass to the component.
 * @param props.message - The message to display.
 * @param props.type - The type of alert to display.
 * @param props.setMessage - The function to set the message.
 * @returns The notifications banner component.
 */
export function NotificationsBanner({ message, setMessage, type }: {
    message: ReactNode;
    setMessage: Dispatch<SetStateAction<ReactNode>>;
    type: AlertColor;
}): ReactNode {
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

    const onClick = (): void => setMessage(null);

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
        <Alert className={styles.notifications!} severity={type}>
            <Stack direction="row" spacing={1} alignItems="center">
                <Typography>{message}</Typography>
                <IconButton className={styles.close!} onClick={onClick}>{closeIcon}</IconButton>
            </Stack>
        </Alert>
    );
}
