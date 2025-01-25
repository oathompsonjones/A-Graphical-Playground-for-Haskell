"use server";

import { RedirectType, redirect } from "next/navigation";
import type { User } from "schemas/database";
import { cookies } from "next/headers";
import { createUser } from "database/index";
import { hash } from "bcrypt";
import { registrationSchema } from "schemas/forms";

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
        const user = JSON.parse(await createUser(parsedData.email, passwordHash)) as User;

        (await cookies()).set("user", user._id.toString());
    } catch (err) {
        throw new Error(`register-${err instanceof Error ? err.message : String(err)}`);
    } finally {
        redirect("/account", RedirectType.replace);
    }
}
