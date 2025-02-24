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

// Update Profile
export const updateProfileSchema = z.object({
    avatar: z.string().optional(),
    email: z.string(),
    id: z.string(),
    password: z.string(),
    username: z.string(),
});

// Save
export const saveSketchSchema = z.object({
    content: z.string(),
    id: z.string(),
});

export const saveAsSketchSchema = z.object({
    authorId: z.string(),
    content: z.string(),
    name: z.string(),
});
