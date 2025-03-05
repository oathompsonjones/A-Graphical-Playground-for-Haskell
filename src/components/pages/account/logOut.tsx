import { Button, FormControl } from "@mui/material";
import type { ReactNode } from "react";
import styles from "styles/components/pages/account/logOut.module.css";

/**
 * Displays a button to log out of the account.
 * @param props - The properties of the component.
 * @param props.action - The action to call when the button is clicked.
 * @param props.pending - Whether the action is pending.
 * @returns The button element wrapped in a form.
 */
export function LogOut({ action, pending }: { action: (payload: FormData) => void; pending: boolean; }): ReactNode {
    return (
        <FormControl component="form" action={action} className={styles.button!}>
            <Button type="submit" disabled={pending}>Sign Out</Button>
        </FormControl>
    );
}
