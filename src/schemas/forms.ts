import z from "zod";

// Register
export const registrationSchema = z.object({
    confirmPassword: z.string(),
    email: z.string(),
    password: z.string(),
});

// Login
export const authenticationSchema = z.object({
    email: z.string(),
    password: z.string(),
});

// Save
export const saveSketchSchema = z.object({
    authorId: z.string(),
    content: z.string(),
    name: z.string(),
});
