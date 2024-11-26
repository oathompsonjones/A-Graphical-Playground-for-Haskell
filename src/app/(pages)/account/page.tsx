"use client";

import Image from "next/image";
import type { ReactNode } from "react";
import { Typography } from "@mui/material";
import { UserContext } from "contexts/user";
import { useContext } from "react";

/**
 * This is the account page.
 * @returns The page element.
 * @throws Error if the user is not logged in.
 */
export default function Account(): ReactNode {
    const { user } = useContext(UserContext);

    // This should never happen, middleware should prevent it.
    if (user === null)
        return <></>;

    return (
        <div>
            <Typography variant="h2">Account</Typography>
            <br />
            <Typography variant="h3" sx={{ color: "#dd2222", fontWeight: "bold", textAlign: "center" }}>
                This is a placeholder page!
            </Typography>
            <br />
            <div style={{ display: "flex", gap: "1rem" }}>
                <div>
                    <Typography>Welcome back, <b>{user.username ?? user.email.split("@")[0]}</b>!</Typography>
                    {user.avatar !== null && <Image
                        src={user.avatar}
                        alt="User avatar"
                        height={500}
                        width={500}
                        style={{ height: "auto", width: "500px" }}
                    />}
                </div>
                <div>
                    <Typography>Your saved code:</Typography>
                    <ul>
                        {user.sketches.map((sketch, i) => (
                            <li key={i}>
                                <Typography>{sketch.name}</Typography>
                                <Typography variant="caption">
                                    Created At: {new Date(sketch.createdAt).toUTCString()}
                                </Typography>
                                <Typography variant="caption">
                                    Modified At: {new Date(sketch.modifiedAt).toUTCString()}
                                </Typography>
                                <pre>{sketch.content}</pre>
                            </li>
                        ))}
                    </ul>
                </div>
            </div>
        </div>
    );
}
