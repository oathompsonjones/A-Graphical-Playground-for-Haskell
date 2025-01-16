"use client";

import { Alert, IconButton } from "@mui/material";
import { Close } from "@mui/icons-material";
import { NotificationsContext } from "contexts/notifications";
import type { ReactNode } from "react";
import styles from "styles/components/notifications.module.css";
import { useContext } from "react";

/**
 * Contains an alert for notifications.
 * @returns Any notifications to display.
 */
export function Notifications(): ReactNode {
    const { message, setMessage, type } = useContext(NotificationsContext);

    return message === null
        ? <></>
        : (
            <Alert className={styles.notifications!} severity={type}>
                {message}
                <IconButton className={styles.close!} onClick={() => setMessage(null)}>
                    <Close />
                </IconButton>
            </Alert>
        );
}
