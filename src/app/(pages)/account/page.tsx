/* eslint-disable max-lines-per-function, react/jsx-max-depth */

"use client";

import {
    Avatar, Button, Card, CardContent, FormControl,
    Grid2 as Grid, Paper, TextField, Typography,
} from "@mui/material";
import { deleteSketch, getSketches } from "database/index";
import { useActionState, useContext, useEffect, useState } from "react";
import type { AuthResponse } from "actions/auth/authenticate";
import Link from "next/link";
import { PasswordField } from "components/pages/auth/passwordField";
import type { ReactNode } from "react";
import type { Sketch } from "schemas/database";
import { UserContext } from "contexts/user";
import { logout } from "actions/auth/logout";
import styles from "styles/pages/account.module.css";
import { updateProfile } from "actions/auth/updateProfile";

// TODO: Warn user before deleting sketch.
// TODO: Warn user before opening a sketch if they have unsaved changes to their current sketch.

/**
 * This is the account page.
 * @returns The page element.
 */
export default function Account(): ReactNode {
    const [logOutState, logOutAction, logOutIsPending] = useActionState<AuthResponse, FormData>(logout, {
        error: null,
        success: false,
    });
    const [updateState, updateAction, updateIsPending] = useActionState<AuthResponse, FormData>(updateProfile, {
        error: null,
        success: false,
    });
    const { user } = useContext(UserContext);
    const [sketches, setSketches] = useState<Sketch[] | null>(null);
    const [avatarUpload, setAvatarUpload] = useState<string | null>(null);

    const fetchSketches = (): void => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                .catch(() => undefined);
        }
    };

    const onDeleteClick = (sketch: Sketch): void => {
        if (user) {
            deleteSketch(sketch._id.toString())
                .then(async () => getSketches(user._id.toString())
                    .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                    .catch(() => undefined))
                .catch(() => undefined);
        }
    };

    useEffect(fetchSketches, [user]);

    useEffect(() => {
        if (logOutState.success)
            window.location.href = `${window.location.origin}/auth/login`;

        if (updateState.success)
            window.location.reload();
    }, [logOutState.success, updateState.success]);

    return user && (
        <div>
            <Typography variant="h2">Welcome back, <b>{user.username ?? user.email.split("@")[0]}</b>!</Typography>
            <br />
            <Paper className={styles.formWrapper!}>
                <Typography variant="h4">Update Profile</Typography>
                <br />
                <FormControl component="form" action={updateAction} className={styles.form!}>
                    <Grid container spacing={2} className={styles.form}>
                        <Grid size={{ sm: 4, xs: 12 }} alignContent="center">
                            <div className={styles.avatarWrapper}>
                                <Avatar className={styles.avatar!} src={avatarUpload ?? user.avatar ?? ""} />
                                <div className={styles.avatarOverlay}>
                                    <input type="hidden" name="avatar" value={avatarUpload ?? ""} />
                                    <input
                                        className={styles.avatarInput}
                                        id="avatarUpload"
                                        type="file"
                                        accept="image/*"
                                        onChange={(e) => {
                                            const file = e.target.files?.item(0);

                                            if (file) {
                                                const reader = new FileReader();

                                                reader.onload = (): void => {
                                                    // Compress the image.
                                                    const canvas = document.createElement("canvas");
                                                    const context = canvas.getContext("2d")!;
                                                    const image = new Image();
                                                    const maxSize = 512;

                                                    canvas.width = maxSize;
                                                    canvas.height = maxSize;
                                                    image.src = reader.result as string;

                                                    const { height: maxHeight, width: maxWidth } = canvas;
                                                    let { height, width } = image;

                                                    if (width > height) {
                                                        if (width > maxWidth) {
                                                            height *= maxWidth / width;
                                                            width = maxWidth;
                                                        }
                                                    } else if (height > maxHeight) {
                                                        width *= maxHeight / height;
                                                        height = maxHeight;
                                                    }

                                                    context.fillStyle = "white";
                                                    context.fillRect(0, 0, canvas.width, canvas.height);
                                                    context.drawImage(image, 0, 0, canvas.width, canvas.height);
                                                    setAvatarUpload(canvas.toDataURL("image/jpeg"));
                                                };
                                                reader.readAsDataURL(file);
                                            }
                                        }}
                                    />
                                    <label htmlFor="avatarUpload" className={styles.avatarLabel}>
                                        { }
                                        <Typography variant="h6">Change Avatar</Typography>
                                    </label>
                                </div>
                            </div>
                        </Grid>
                        <Grid size={{ sm: 8, xs: 12 }} alignContent="center">
                            <div className={styles.fields}>
                                <input type="hidden" name="id" value={user._id.toString()} />
                                <TextField
                                    label="Email" name="email" type="email" autoComplete="username"
                                    defaultValue={user.email} />
                                <TextField
                                    label="Username" name="username"
                                    defaultValue={user.username ?? user.email.split("@")[0]} />
                                <PasswordField label="Password" name="password" />
                                <Button className={styles.button!} type="submit" disabled={updateIsPending}>
                            Save Changes
                                </Button>
                                {/* <Typography variant="caption" textAlign="center">
                            <Link href="">Reset Password</Link>
                        </Typography> */}
                            </div>
                        </Grid>
                    </Grid>
                </FormControl>
            </Paper>
            <br />
            <Typography variant="h4">Your Saved sketches</Typography>
            {sketches === null || sketches.length === 0
                ? (
                    <div className={styles.noSketches}>
                        <Typography variant="h6">You have no saved sketches.</Typography>
                        <Typography variant="caption">
                            <Link href="/editor">Create a new sketch</Link>
                        </Typography>
                    </div>
                )
                : (
                    <div className={styles.sketches}>
                        {sketches.map((sketch, i) => (
                            <Card key={i}>
                                <CardContent className={styles.sketchMain!}>
                                    <Typography variant="h5">
                                        <Link href={`/editor?id=${encodeURIComponent(sketch._id.toString())}`}>
                                            {sketch.name}
                                        </Link>
                                    </Typography>
                                    <Button color="error" onClick={() => onDeleteClick(sketch)} size="small">
                                        Delete</Button>
                                </CardContent>
                                <CardContent>
                                    <Typography variant="caption" color="textDisabled">
                                        <b>Created At:</b>
                                        {new Date(parseInt(sketch.createdAt, 10)).toLocaleString()}
                                    </Typography>
                                    <br />
                                    <Typography variant="caption" color="textDisabled">
                                        <b>Last Edited At:</b>
                                        {new Date(parseInt(sketch.modifiedAt, 10)).toLocaleString()}
                                    </Typography>
                                </CardContent>
                            </Card>
                        ))}
                    </div>
                )}
            <br />
            <FormControl component="form" action={logOutAction} className={styles.signOut!}>
                <Button type="submit" disabled={logOutIsPending}>Sign Out</Button>
            </FormControl>
        </div>
    );
}
