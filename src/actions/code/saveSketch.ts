import { createSketch, updateSketch } from "database/index";
import { saveAsSketchSchema, saveSketchSchema } from "schemas/forms";
import type { Sketch } from "schemas/database";

/**
 * Saves the sketch.
 * @param formData - The form data containing the sketch name.
 * @returns The ID of the saved sketch.
 */
export async function saveSketch(formData: FormData): Promise<string> {
    try {
        let sketch: string;

        if (formData.has("id")) {
            const parsedData = saveSketchSchema.parse({
                content: formData.get("content"),
                id: formData.get("id"),
            });

            sketch = await updateSketch(parsedData.id, { content: parsedData.content });
        } else {
            const parsedData = saveAsSketchSchema.parse({
                authorId: formData.get("authorId"),
                content: formData.get("content"),
                name: formData.get("name"),
            });

            sketch = await createSketch(parsedData.authorId, parsedData.name, parsedData.content);
        }

        return (JSON.parse(sketch) as Sketch)._id.toString();
    } catch (err) {
        throw new Error(err instanceof Error ? err.message : String(err));
    }
}
