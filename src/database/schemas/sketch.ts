import z from "zod";

export const sketchSchema = z.object({
    content: z.string(),
    createdAt: z.string().datetime(),
    modifiedAt: z.string().datetime(),
    name: z.string(),
});

export type Sketch = z.infer<typeof sketchSchema>;
