import z from "zod";

export const registrationSchema = z.object({
    confirmPassword: z.string(),
    email: z.string(),
    password: z.string(),
});

export const authenticationSchema = z.object({
    email: z.string(),
    password: z.string(),
});
