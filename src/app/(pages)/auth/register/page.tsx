"use client";

import { Alert, Button, FormControl, Paper, TextField, Typography } from "@mui/material";
// Import { Apple, GitHub, Google, StackOverflow } from "@mui/icons-material";
import { useActionState, useEffect } from "react";
import type { AuthResponse } from "actions/auth/authenticate";
import Link from "next/link";
import { PasswordField } from "components/pages/auth/passwordField";
import type { ReactNode } from "react";
import { register } from "actions/auth/register";
import styles from "styles/pages/auth.module.css";

/**
 * Displays the sign-in page.
 * @returns The page element.
 */
export default function Register(): ReactNode {
    const [state, action, isPending] = useActionState<AuthResponse, FormData>(register, {
        error: null,
        success: false,
    });

    useEffect(() => {
        if (state.success)
            window.location.href = `${window.location.origin}/account`;
    }, [state.success]);

    return (
        <div>
            <Typography align="center" variant="h2">Register</Typography>
            <br />
            <Paper className={styles.container}>
                {!state.success && state.error !== null && <Alert severity="error">{state.error}</Alert>}
                <FormControl action={action} className={styles.form} component="form">
                    <TextField label="Email" name="email" type="email" />
                    <PasswordField label="Password" name="password" />
                    <PasswordField label="Confirm Password" name="confirmPassword" />
                    <Button className={styles.button} disabled={isPending} type="submit">Register</Button>
                    {/* <ButtonGroup>
                        <Button className={styles.button!} color="secondary" startIcon={<Apple />} href="">
                            Register with Apple
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<Google />} href="">
                            Register with Google
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<GitHub />} href="">
                            Register with GitHub
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<StackOverflow />} href="">
                            Register with StackOverflow
                        </Button>
                    </ButtonGroup> */}
                    <Typography align="center" variant="caption">
                        Already have an account? <Link href="/auth/login">Sign in</Link>
                    </Typography>
                </FormControl>
            </Paper>
        </div>
    );
}
