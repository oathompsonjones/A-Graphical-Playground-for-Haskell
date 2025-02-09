import { Button, TextField } from "@mui/material";
import { Visibility, VisibilityOff } from "@mui/icons-material";
import type { ReactNode } from "react";
import { useState } from "react";

/**
 * Displays a text field with a password that can be shown or hidden.
 * @param props - The component properties.
 * @param props.label - The label of the text field.
 * @param props.name - The name of the text field.
 * @returns The text field element.
 */
export function PasswordField({ label, name }: { label: string; name: string; }): ReactNode {
    const [showPassword, setShowPassword] = useState(false);
    const onClick = (): void => setShowPassword((prevShowPassword) => !prevShowPassword);

    return (
        <TextField
            label={label}
            name={name}
            type={showPassword ? "text" : "password"}
            slotProps={{
                input: {
                    endAdornment: <Button onClick={onClick} variant="text" sx={{ color: "inherit" }}>
                        {showPassword ? <Visibility /> : <VisibilityOff />}
                    </Button>,
                },
            }}
        />
    );
}
