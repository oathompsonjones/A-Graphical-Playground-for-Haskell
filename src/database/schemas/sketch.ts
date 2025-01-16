import type { WithId } from "mongodb";
import z from "zod";

export const sketchSchema = z.object({
    authorId: z.string(),
    content: z.string(),
    createdAt: z.string().datetime(),
    modifiedAt: z.string().datetime(),
    name: z.string(),
    public: z.boolean(),
});

export type SketchWithoutId = z.infer<typeof sketchSchema>;
export type Sketch = WithId<SketchWithoutId>;
