"use client";

import { Alert, Button, FormControl, Paper, TextField, Typography } from "@mui/material";
// Import { Apple, GitHub, Google, StackOverflow } from "@mui/icons-material";
import { useActionState, useEffect } from "react";
import type { AuthResponse } from "actions/auth/authenticate";
import Link from "next/link";
import { PasswordField } from "components/pages/auth/passwordField";
import type { ReactNode } from "react";
import { authenticate } from "actions/auth/authenticate";
import styles from "styles/pages/auth.module.css";

// TODO: Add ability to reset password via email.

/**
 * Displays the sign-in page.
 * @returns The page element.
 */
export default function LogIn(): ReactNode {
    const [state, action, isPending] = useActionState<AuthResponse, FormData>(authenticate, {
        error: null,
        success: false,
    });

    useEffect(() => {
        if (state.success)
            window.location.href = `${window.location.origin}/account`;
    }, [state.success]);

    return (
        <div>
            <Typography align="center" variant="h2">Sign in</Typography>
            <br />
            <Paper className={styles.container}>
                {!state.success && state.error !== null && <Alert severity="error">{state.error}</Alert>}
                <FormControl action={action} className={styles.form} component="form">
                    <TextField label="Email" name="email" type="email" />
                    <PasswordField label="Password" name="password" />
                    <Button className={styles.button} disabled={isPending} type="submit">Sign in</Button>
                    {/* <ButtonGroup>
                        <Button className={styles.button!} color="secondary" startIcon={<Apple />} href="">
                            Sign in with Apple
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<Google />} href="">
                            Sign in with Google
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<GitHub />} href="">
                            Sign in with GitHub
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<StackOverflow />} href="">
                            Sign in with StackOverflow
                        </Button>
                    </ButtonGroup> */}
                    <Typography align="center" variant="caption">
                        Don't have an account? <Link href="/auth/register">Register</Link>
                    </Typography>
                    {/* <Typography variant="caption" textAlign="center">
                        <Link href="">Forgot Password?</Link>
                    </Typography> */}
                </FormControl>
            </Paper>
        </div>
    );
}
