import z from "zod";

// Points
const vectorSchema = z.object({
    x: z.number(),
    y: z.number(),
});

// Shapes
const lineSchema = z.object({
    length: z.number(),
    type: z.literal("line"),
});

const ellipseSchema = z.object({
    horizontalAxis: z.number(),
    type: z.literal("ellipse"),
    verticalAxis: z.number(),
});

const rectSchema = z.object({
    height: z.number(),
    type: z.literal("rect"),
    width: z.number(),
});

const polygonSchema = z.object({
    points: z.array(vectorSchema),
    type: z.literal("polygon"),
});

const baseShapeSchema = z.object({
    angle: z.number(),
    fill: z.string(),
    position: vectorSchema,
    stroke: z.string(),
    strokeWeight: z.number(),
}).and(z.union([lineSchema, ellipseSchema, rectSchema, polygonSchema]));

const shapeSchema = baseShapeSchema.or(z.array(baseShapeSchema));

export type Shape = z.infer<typeof shapeSchema>;

// Canvas
export const canvasSchema = z.object({
    backgroundColor: z.string(),
    height: z.number().int(),
    shapes: z.array(shapeSchema),
    width: z.number().int(),
});
export type CanvasSchema = z.infer<typeof canvasSchema>;
