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
            <Typography variant="h2" textAlign="center">Register</Typography>
            <br />
            <Paper className={styles.container!}>
                {!state.success && state.error !== null && <Alert severity="error">{state.error}</Alert>}
                <FormControl component="form" action={action} className={styles.form!}>
                    <TextField label="Email" name="email" type="email" />
                    <PasswordField label="Password" name="password" />
                    <PasswordField label="Confirm Password" name="confirmPassword" />
                    <Button className={styles.button!} type="submit" disabled={isPending}>Register</Button>
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
                    <Typography variant="caption" textAlign="center">
                        Already have an account? <Link href="/auth/login">Sign in</Link>
                    </Typography>
                </FormControl>
            </Paper>
        </div>
    );
}
