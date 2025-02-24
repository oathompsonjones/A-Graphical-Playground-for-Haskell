"use server";

import { getUserFromId, updateUser } from "database/index";
import type { AuthResponse } from "./authenticate";
import type { User } from "schemas/database";
import { compare } from "bcrypt";
import { updateProfileSchema } from "schemas/forms";

/**
 * Update the user's profile in the database.
 * @param _state - The previous state.
 * @param formData - The form data to send.
 * @returns Whether the user's profile was updated.
 */
export async function updateProfile(_state: AuthResponse, formData: FormData): Promise<AuthResponse> {
    try {
        const { avatar, email, id, username, password } = updateProfileSchema.parse({
            avatar: formData.get("avatar"),
            email: formData.get("email"),
            id: formData.get("id"),
            password: formData.get("password"),
            username: formData.get("username"),
        });

        const user = JSON.parse(await getUserFromId(id)) as User;
        const isCorrectPassword = await compare(password, user.passwordHash);

        if (!isCorrectPassword)
            throw new Error("Invalid credentials.");

        const updated: Partial<User> = {};

        if (avatar !== undefined && avatar !== "")
            updated.avatar = avatar;

        if (email !== "")
            updated.email = email;

        if (username !== "")
            updated.username = username;

        await updateUser(id, updated);
    } catch (err) {
        return {
            error: err instanceof Error ? err.message : String(err),
            success: false,
        };
    }

    return { data: null, success: true };
}
