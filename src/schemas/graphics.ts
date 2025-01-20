import z from "zod";

// Points
export const pointSchema = z.object({
    x: z.number(),
    y: z.number(),
});
export type Point = z.infer<typeof pointSchema>;

// Shapes
export const ellipseSchema = z.object({
    horizontalAxis: z.number(),
    type: z.literal("ellipse"),
    verticalAxis: z.number(),
});
export type Ellipse = z.infer<typeof ellipseSchema>;

export const polygonSchema = z.object({
    points: z.array(pointSchema),
    type: z.literal("polygon"),
});
export type Polygon = z.infer<typeof polygonSchema>;

const _shapeSchema = z.object({
    angle: z.number(),
    fill: z.string(),
    position: pointSchema,
    stroke: z.string(),
    strokeWeight: z.number(),
}).and(z.union([ellipseSchema, polygonSchema]));

export const shapeSchema = _shapeSchema.or(z.array(_shapeSchema));
export type Shape = z.infer<typeof shapeSchema>;

// Canvas
export const frameSchema = z.object({
    backgroundColor: z.string(),
    height: z.number().int(),
    shapes: z.array(shapeSchema),
    width: z.number().int(),
});
export type Frame = z.infer<typeof frameSchema>;
