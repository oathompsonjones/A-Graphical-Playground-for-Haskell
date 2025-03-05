/* eslint-disable react/jsx-max-depth */

"use client";

import { Avatar, Button, FormControl, Grid2 as Grid, Paper, TextField, Typography } from "@mui/material";
import { useContext, useState } from "react";
import { PasswordField } from "../auth/passwordField";
import type { ReactNode } from "react";
import { UserContext } from "contexts/user";
import styles from "styles/components/pages/account/editProfile.module.css";

/**
 * Displays a form to edit the user's profile.
 * @param props - The properties of the component.
 * @param props.action - The function to call to update the user's profile.
 * @param props.pending - Whether the action is pending.
 * @returns The form element.
 */
export function EditProfile({ action, pending }: {
    action: (payload: FormData) => void;
    pending: boolean;
}): ReactNode {
    const { user } = useContext(UserContext);
    const [avatarUpload, setAvatarUpload] = useState<string | null>(null);

    return (
        <Paper className={styles.formWrapper!}>
            <Typography variant="h4">Update Profile</Typography>
            <br />
            <FormControl component="form" action={action} className={styles.form!}>
                <Grid container spacing={2} className={styles.form}>
                    <Grid size={{ sm: 4, xs: 12 }} alignContent="center">
                        <div className={styles.avatarWrapper}>
                            <Avatar className={styles.avatar!} src={avatarUpload ?? user?.avatar ?? ""} />
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
                                <label htmlFor="avatarUpload" className={styles.avatarLabel} />
                                <Typography variant="h6">
                                    Change Avatar
                                </Typography>
                            </div>
                        </div>
                    </Grid>
                    <Grid size={{ sm: 8, xs: 12 }} alignContent="center">
                        <div className={styles.fields}>
                            <input type="hidden" name="id" value={user?._id.toString()} />
                            <TextField
                                label="Email" name="email" type="email" autoComplete="username"
                                defaultValue={user?.email} />
                            <TextField
                                label="Username" name="username"
                                defaultValue={user?.username ?? user?.email.split("@")[0]} />
                            <PasswordField label="Password" name="password" />
                            <Button type="submit" disabled={pending}>
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
    );
}
