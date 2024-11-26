"use server";

import { cookies } from "next/headers";
import { createUser } from "database/database";
import { hash } from "bcrypt";
import { registrationSchema } from "database/schemas/forms";

/**
 * Register the user with the server.
 * @param formData - The form data to send.
 */
export async function register(formData: FormData): Promise<void> {
    try {
        const parsedData = registrationSchema.parse({
            confirmPassword: formData.get("confirmPassword"),
            email: formData.get("email"),
            password: formData.get("password"),
        });

        if (parsedData.password !== parsedData.confirmPassword)
            throw new Error("Passwords do not match.");

        const passwordHash = await hash(parsedData.password, 11);
        const user = await createUser(parsedData.email, passwordHash);

        cookies().set("user", user.email);
    } catch (err) {
        throw new Error(`register-${err instanceof Error ? err.message : String(err)}`);
    }
}
