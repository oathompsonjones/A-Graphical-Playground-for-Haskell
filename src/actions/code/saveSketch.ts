import { createSketch } from "database/index";
import { saveSketchSchema } from "schemas/forms";

/**
 * Saves the sketch.
 * @param formData - The form data containing the sketch name.
 */
export async function saveSketch(formData: FormData): Promise<void> {
    try {
        const parsedData = saveSketchSchema.parse({
            authorId: formData.get("authorId"),
            content: formData.get("content"),
            name: formData.get("name"),
        });

        await createSketch(parsedData.authorId, parsedData.name, parsedData.content);
    } catch (err) {
        throw new Error(err instanceof Error ? err.message : String(err));
    }
}
