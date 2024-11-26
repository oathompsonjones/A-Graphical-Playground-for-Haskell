"use client";

import { Alert, Button, ButtonGroup, FormControl, Paper, TextField, Typography } from "@mui/material";
import { Apple, GitHub, Google } from "@mui/icons-material";
import Link from "next/link";
import type { ReactNode } from "react";
import { authenticate } from "actions/auth/authenticate";
import { register } from "actions/auth/register";
import styles from "styles/pages/logIn.module.css";
import { useState } from "react";

/**
 * Displays the sign-in page.
 * @param props - The component properties.
 * @param props.error - The error to display.
 * @returns The page element.
 */
export default function LogIn({ error }: { error: Error | undefined; }): ReactNode {
    const [err, setErr] = useState<Error | undefined>(error);
    // Checks if the error is from the register or authenticate function, and sets the form type accordingly.
    const [formType, setFormType] = useState<"authenticate" | "register">(
        err?.cause === "register" || err?.cause === "authenticate" ? err.cause : "authenticate",
    );
    const switchFormType = (): void => {
        setErr(undefined);
        setFormType(formType === "authenticate" ? "register" : "authenticate");
    };
    const title = { authenticate: "Sign in", register: "Register" }[formType];
    const action = { authenticate, register }[formType];

    return (
        <div>
            <Typography variant="h2" textAlign="center">{title}</Typography>
            <br />
            <Paper className={styles.container!}>
                {err !== undefined && <Alert severity="error">{err.message}</Alert>}
                <FormControl component="form" action={action} className={styles.form!}>
                    <TextField label="Email" name="email" type="email" />
                    <TextField label="Password" name="password" type="password" />
                    {
                        formType === "register" &&
                        <TextField label="Confirm Password" name="confirmPassword" type="password" />
                    }
                    <Button className={styles.button!} type="submit">
                        {title}
                    </Button>
                    <ButtonGroup>
                        <Button className={styles.button!} color="secondary" startIcon={<Apple />} href="">
                            {title} with Apple
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<Google />} href="">
                            {title} with Google
                        </Button>
                        <Button className={styles.button!} color="secondary" startIcon={<GitHub />} href="">
                            {title} with GitHub
                        </Button>
                    </ButtonGroup>
                    <Typography variant="caption" textAlign="center">
                        {{ authenticate: "Don't have an account?", register: "Already have an account?" }[formType]}
                        <Link onClick={switchFormType} href="">
                            {{ authenticate: " Register", register: " Sign in" }[formType]}
                        </Link>
                    </Typography>
                </FormControl>
            </Paper>
        </div>
    );
}
