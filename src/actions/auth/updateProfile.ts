"use server";

import type { AuthResponse } from "./authenticate";
import type { User } from "schemas/database";
import { authenticate } from "./authenticate";
import { updateProfileSchema } from "schemas/forms";
import { updateUser } from "database/index";

/**
 * Update the user's profile in the database.
 * @param _state - The previous state.
 * @param formData - The form data to send.
 * @returns Whether the user's profile was updated.
 */
export async function updateProfile(_state: AuthResponse, formData: FormData): Promise<AuthResponse> {
    try {
        const { avatar, email, username } = updateProfileSchema.parse({
            avatar: formData.get("avatar"),
            email: formData.get("email"),
            password: formData.get("password"),
            username: formData.get("username"),
        });

        const authenticated = await authenticate(_state, formData);

        if (!authenticated.success)
            return { error: "Invalid credentials.", success: false };

        const updated: Partial<User> = {};

        if (avatar !== undefined && avatar !== "")
            updated.avatar = avatar;

        if (email !== "")
            updated.email = email;

        if (username !== "")
            updated.username = username;

        await updateUser(authenticated.data!, updated);
    } catch (err) {
        return {
            error: err instanceof Error ? err.message : String(err),
            success: false,
        };
    }

    return { data: null, success: true };
}
