"use server";

import type { AuthResponse } from "./authenticate";
import type { User } from "schemas/database";
import { cookies } from "next/headers";
import { createUser } from "database/index";
import { hash } from "bcrypt";
import { registrationSchema } from "schemas/forms";

/**
 * Register the user with the server.
 * @param _state - The previous state.
 * @param formData - The form data to send.
 * @returns Whether the user was registered.
 */
export async function register(_state: AuthResponse, formData: FormData): Promise<AuthResponse> {
    let id: string | null = null;

    try {
        const { email, password, confirmPassword } = registrationSchema.parse({
            confirmPassword: formData.get("confirmPassword"),
            email: formData.get("email"),
            password: formData.get("password"),
        });

        if (password !== confirmPassword)
            throw new Error("Passwords do not match.");

        const passwordHash = await hash(password, 11);
        const user = JSON.parse(await createUser(email, passwordHash)) as User;

        id = user._id.toString();
        (await cookies()).set("user", id);
    } catch (err) {
        return {
            error: err instanceof Error ? err.message : String(err),
            success: false,
        };
    }

    return { data: id, success: true };
}
