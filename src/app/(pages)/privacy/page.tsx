import type { ReactNode } from "react";
import { Typography } from "@mui/material";

/**
 * This details my privacy policy.
 * @returns The privacy policy page.
 */
export default function Privacy(): ReactNode {
    return (
        <div>
            <Typography variant="h2">Privacy Policy</Typography>
            <br />
            <Typography variant="h4">Personal Data</Typography>
            <Typography component="ul">
                <li>This website uses your email address to identify you.</li>
                <li>Your email address is stored securely, and will never be shared with a third party.</li>
                <li>Your email address is only used to allow you to log in and reset your password.</li>
                <li>You can use this website without an account, but you will not be able to save your work.</li>
            </Typography>
            <br />
            <Typography variant="h4">Cookies</Typography>
            <Typography component="ul">
                <li>This website uses cookies to allow you to log in and to remember your preferences.</li>
                <li>This website does not use cookies for tracking or advertising.</li>
                <li>This website does not use any third party cookies.</li>
            </Typography>
            <br />
            <Typography variant="caption">
                If you have any queries regarding this privacy policy,
                please contact me <a href="mailto:oathompsonjones@gmail.com">here</a>.
            </Typography>
        </div>
    );
}
