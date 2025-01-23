"use client";

import { Alert, Icon, IconButton } from "@mui/material";
import { useContext, useEffect, useState } from "react";
import { Close } from "@mui/icons-material";
import { NotificationsContext } from "contexts/notifications";
import type { ReactNode } from "react";
import styles from "styles/components/notifications.module.css";

/**
 * Contains an alert for notifications.
 * @returns Any notifications to display.
 */
export function Notifications(): ReactNode {
    const { message, setMessage, type } = useContext(NotificationsContext);
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

    return message === null
        ? <></>
        : (
            <Alert className={styles.notifications!} severity={type}>
                {message}
                <IconButton className={styles.close!} onClick={() => setMessage(null)}>
                    {closeIcon}
                </IconButton>
            </Alert>
        );
}
