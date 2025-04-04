import z from "zod";

export const enum ShapeType {
    Line = 0,
    Ellipse = 1,
    Rect = 2,
    Polygon = 3,
    Curve = 4,
    Arc = 5,
}

export const enum Connection {
    Open = 0,
    Chord = 1,
    Pie = 2,
}

export const colours = [
    "transparent",
    "aliceblue",
    "antiquewhite",
    "aqua",
    "aquamarine",
    "azure",
    "beige",
    "bisque",
    "black",
    "blanchedalmond",
    "blue",
    "blueviolet",
    "brown",
    "burlywood",
    "cadetblue",
    "chartreuse",
    "chocolate",
    "coral",
    "cornflowerblue",
    "cornsilk",
    "crimson",
    "cyan",
    "darkblue",
    "darkcyan",
    "darkgoldenrod",
    "darkgray",
    "darkgreen",
    "darkgrey",
    "darkkhaki",
    "darkmagenta",
    "darkolivegreen",
    "darkorange",
    "darkorchid",
    "darkred",
    "darksalmon",
    "darkseagreen",
    "darkslateblue",
    "darkslategray",
    "darkslategrey",
    "darkturquoise",
    "darkviolet",
    "deeppink",
    "deepskyblue",
    "dimgray",
    "dimgrey",
    "dodgerblue",
    "firebrick",
    "floralwhite",
    "forestgreen",
    "fuchsia",
    "gainsboro",
    "ghostwhite",
    "gold",
    "goldenrod",
    "gray",
    "green",
    "greenyellow",
    "grey",
    "honeydew",
    "hotpink",
    "indianred",
    "indigo",
    "ivory",
    "khaki",
    "lavender",
    "lavenderblush",
    "lawngreen",
    "lemonchiffon",
    "lightblue",
    "lightcoral",
    "lightcyan",
    "lightgoldenrodyellow",
    "lightgray",
    "lightgreen",
    "lightgrey",
    "lightpink",
    "lightsalmon",
    "lightseagreen",
    "lightskyblue",
    "lightslategray",
    "lightslategrey",
    "lightsteelblue",
    "lightyellow",
    "lime",
    "limegreen",
    "linen",
    "magenta",
    "maroon",
    "mediumaquamarine",
    "mediumblue",
    "mediumorchid",
    "mediumpurple",
    "mediumseagreen",
    "mediumslateblue",
    "mediumspringgreen",
    "mediumturquoise",
    "mediumvioletred",
    "midnightblue",
    "mintcream",
    "mistyrose",
    "moccasin",
    "navajowhite",
    "navy",
    "oldlace",
    "olive",
    "olivedrab",
    "orange",
    "orangered",
    "orchid",
    "palegoldenrod",
    "palegreen",
    "paleturquoise",
    "palevioletred",
    "papayawhip",
    "peachpuff",
    "peru",
    "pink",
    "plum",
    "powderblue",
    "purple",
    "red",
    "rosybrown",
    "royalblue",
    "saddlebrown",
    "salmon",
    "sandybrown",
    "seagreen",
    "seashell",
    "sienna",
    "silver",
    "skyblue",
    "slateblue",
    "slategray",
    "slategrey",
    "snow",
    "springgreen",
    "steelblue",
    "tan",
    "teal",
    "thistle",
    "tomato",
    "turquoise",
    "violet",
    "wheat",
    "white",
    "whitesmoke",
    "yellow",
    "yellowgreen",
];

const defaults = {
    angle: 0,
    fill: colours.indexOf("transparent"),
    position: [0, 0] as [number, number],
    stroke: colours.indexOf("black"),
    strokeWeight: 1,
};

// Vectors
const vectorSchema = z.tuple([z.number(), z.number()]);

// Shapes
const emptySchema = z.object({});

const lineSchema = z.object({
    l: z.number(),
    t: z.literal(ShapeType.Line),
});

const ellipseSchema = z.object({
    h: z.number(),
    t: z.literal(ShapeType.Ellipse),
    v: z.number(),
});

const rectSchema = z.object({
    h: z.number(),
    t: z.literal(ShapeType.Rect),
    w: z.number(),
});

const polygonSchema = z.object({
    t: z.literal(ShapeType.Polygon),
    v: z.array(vectorSchema),
});

const curveSchema = z.object({
    t: z.literal(ShapeType.Curve),
    v: z.array(vectorSchema),
});

const arcSchema = z.object({
    b: z.number(),
    c: z.union([z.literal(Connection.Open), z.literal(Connection.Chord), z.literal(Connection.Pie)]),
    e: z.number(),
    h: z.number(),
    t: z.literal(ShapeType.Arc),
    v: z.number(),
});

const baseShapeSchema = z.object({
    a: z.number().optional().default(defaults.angle),
    f: z.union([z.string(), z.number()]).optional().default(defaults.fill),
    p: vectorSchema.optional().default(defaults.position),
    s: z.union([z.string(), z.number()]).optional().default(defaults.stroke),
    sw: z.number().optional().default(defaults.strokeWeight),
}).and(z.union([lineSchema, ellipseSchema, rectSchema, polygonSchema, curveSchema, arcSchema, emptySchema]));

export const shapeSchema = baseShapeSchema.or(z.array(baseShapeSchema));
export type Shape = z.infer<typeof shapeSchema>;

// Canvas
export const canvasSchema = z.object({
    b: z.string().or(z.number()),
    h: z.number().int(),
    r: z.number(),
    w: z.number().int(),
});
export type CanvasSchema = z.infer<typeof canvasSchema>;
