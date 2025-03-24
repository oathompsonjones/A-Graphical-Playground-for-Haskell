import { MenuItem, Select, Tooltip } from "@mui/material";
import { useEffect, useState } from "react";
import type { ReactNode } from "react";
import styles from "styles/components/pages/reference/section.module.css";

const cssNamedColours = [
    "AliceBlue",
    "AntiqueWhite",
    "Aqua",
    "AquaMarine",
    "Azure",
    "Beige",
    "Bisque",
    "Black",
    "BlanchedAlmond",
    "Blue",
    "BlueViolet",
    "Brown",
    "BurlyWood",
    "CadetBlue",
    "Chartreuse",
    "Chocolate",
    "Coral",
    "CornflowerBlue",
    "Cornsilk",
    "Crimson",
    "Cyan",
    "DarkBlue",
    "DarkCyan",
    "DarkGoldenRod",
    "DarkGray",
    "DarkGreen",
    "DarkGrey",
    "DarkKhaki",
    "DarkMagenta",
    "DarkOliveGreen",
    "DarkOrange",
    "DarkOrchid",
    "DarkRed",
    "DarkSalmon",
    "DarkSeaGreen",
    "DarkSlateBlue",
    "DarkSlateGray",
    "DarkSlateGrey",
    "DarkTurquoise",
    "DarkViolet",
    "DeepPink",
    "DeepSkyBlue",
    "DimGray",
    "DimGrey",
    "DodgerBlue",
    "FireBrick",
    "FloralWhite",
    "ForestGreen",
    "Fuchsia",
    "Gainsboro",
    "GhostWhite",
    "Gold",
    "GoldenRod",
    "Gray",
    "Green",
    "GreenYellow",
    "Grey",
    "HoneyDew",
    "HotPink",
    "IndianRed",
    "Indigo",
    "Ivory",
    "Khaki",
    "Lavender",
    "LavenderBlush",
    "LawnGreen",
    "LemonChiffon",
    "LightBlue",
    "LightCoral",
    "LightCyan",
    "LightGoldenRodYellow",
    "LightGray",
    "LightGreen",
    "LightGrey",
    "LightPink",
    "LightSalmon",
    "LightSeaGreen",
    "LightSkyBlue",
    "LightSlateGray",
    "LightSlateGrey",
    "LightSteelBlue",
    "LightYellow",
    "Lime",
    "LimeGreen",
    "Linen",
    "Magenta",
    "Maroon",
    "MediumAquaMarine",
    "MediumBlue",
    "MediumOrchid",
    "MediumPurple",
    "MediumSeaGreen",
    "MediumSlateBlue",
    "MediumSpringGreen",
    "MediumTurquoise",
    "MediumVioletRed",
    "MidnightBlue",
    "MintCream",
    "MistyRose",
    "Moccasin",
    "NavajoWhite",
    "Navy",
    "Oldlace",
    "Olive",
    "OliveDrab",
    "Orange",
    "OrangeRed",
    "Orchid",
    "PaleGoldenRod",
    "PaleGreen",
    "PaleTurquoise",
    "PaleVioletRed",
    "PapayaWhip",
    "PeachPuff",
    "Peru",
    "Pink",
    "Plum",
    "PowderBlue",
    "Purple",
    "Red",
    "RosyBrown",
    "RoyalBlue",
    "SaddleBrown",
    "Salmon",
    "SandyBrown",
    "SeaGreen",
    "SeaShell",
    "Sienna",
    "Silver",
    "SkyBlue",
    "SlateBlue",
    "SlateGray",
    "SlateGrey",
    "Snow",
    "SpringGreen",
    "SteelBlue",
    "Tan",
    "Teal",
    "Thistle",
    "Tomato",
    "Turquoise",
    "Violet",
    "Wheat",
    "White",
    "WhiteSmoke",
    "Yellow",
    "YellowGreen",
];

const namedToRGB = (color: string, doc: Document | null): [number, number, number] => {
    if (doc === null)
        return [255, 255, 255];

    const element = doc.createElement("div");

    element.style.color = color;
    doc.body.appendChild(element);
    const { color: rgb } = getComputedStyle(element);

    doc.body.removeChild(element);

    const [r, g, b] = rgb.match(/\d+/g)!.map(Number);

    return [r!, g!, b!];
};

const rgbToHex = (r: number, g: number, b: number): string => `#${[r, g, b]
    .map((c) => c.toString(16).padStart(2, "0")).join("")}`;

const rgbToHsl = (r: number, g: number, b: number): [number, number, number] => {
    const [R, G, B] = [r / 255, g / 255, b / 255];
    const cMax = Math.max(R, G, B);
    const cMin = Math.min(R, G, B);
    const d = cMax - cMin;
    let [h, s, l] = [0, 0, 0];

    // Calculate hue
    switch (cMax) {
        case cMin:
            break;
        case R:
            h = (G - B) / d + (G < B ? 6 : 0);
            break;
        case G:
            h = (B - R) / d + 2;
            break;
        default:
            h = (R - G) / d + 4;
            break;
    }

    // Calculate lightness and saturation
    l = (cMax + cMin) / 2;
    s = d === 0 ? 0 : d / (1 - Math.abs(2 * l - 1));

    // Adjust values
    h = Number((h * 60).toFixed(1));
    l = Number((l * 100).toFixed(1));
    s = Number((s * 100).toFixed(1));

    return [h, s, l];
};

/**
 * Displays the root of the colors documentation section.
 * @param props - The properties of the component.
 * @param props.list - A function to use to create a list element.
 * @returns A documentation section.
 */
export function ColorsRoot({ list }: { list: (elems: ReactNode[]) => ReactNode; }): ReactNode {
    const [sortByColour, setSortByColour] = useState(true);
    const [doc, setDoc] = useState<Document | null>(null);

    useEffect(() => {
        if (typeof window.document !== "undefined")
            setDoc(window.document);
    }, [doc]);

    return (
        <div>
            Colors are represented using the <code>Color</code> data type, which has the following constructors:
            {list([
                <><code>RGB Float Float Float</code> — Represents a color with red, green, and blue values.</>,
                <><code>RGBA Float Float Float Float</code> — Represents a color with red, green, blue, and alpha
                    values.</>,
                <><code>Hex String</code> — Represents a color using a hexadecimal string. You can prefix the string
                    with a hash (e.g. <code>"#ff0000"</code>) or you can leave it out (e.g. <code>"ff0000"</code>).
                </>,
                <><code>HSL Float Float Float</code> — Represents a color with hue, saturation,
                    and lightness values.</>,
                <><code>HSLA Float Float Float Float</code> — Represents a color with hue, saturation, lightness,
                    and alpha values.</>,
                <><code>Transparent</code> — Represents a transparent color.</>,
            ])}

            You can also use the following CSS named colors (sort by <Select
                value={Number(sortByColour)} variant="standard" size="small"
                onChange={(e) => setSortByColour(Boolean(e.target.value))}>
                <MenuItem value={0}>Alphabetical</MenuItem>
                <MenuItem value={1}>Colour</MenuItem>
            </Select>):
            <br />
            {cssNamedColours.map((name) => {
                const rgb = namedToRGB(name, doc);
                const hex = rgbToHex(...rgb);
                const hsl = rgbToHsl(...rgb);
                const contrast = rgb[0] * 0.299 + rgb[1] * 0.587 + rgb[0] * 0.114 > 140 ? "black" : "white";

                return { contrast, hex, hsl, name, rgb };
            })
                .sort((a, b) => (sortByColour
                    ? a.hsl[0] - b.hsl[0] || a.hsl[1] - b.hsl[1] || a.hsl[2] - b.hsl[2]
                    : a.name.localeCompare(b.name)))
                .map(({ name, rgb, hsl, hex, contrast }, i) => (
                    <Tooltip
                        key={i} placement="top"
                        componentsProps={{
                            popper: { className: styles.tooltip },
                            tooltip: {
                                sx: {
                                    backgroundColor: name,
                                    border: `3px solid ${contrast}`,
                                    borderRadius: "2vmin",
                                    color: contrast,
                                },
                            },
                        }}
                        title={<>
                            <b>{name}</b>
                            <br />
                            rgb({rgb.join(", ")})
                            <br />
                            hsl({hsl[0]}, {hsl[1]}%, {hsl[2]}%)
                            <br />
                            {hex.toUpperCase()}
                        </>}>
                        <span style={{ lineHeight: "2em" }}>
                            <code style={{ background: name, color: contrast }}>{name}</code>
                            {" "}
                        </span>
                    </Tooltip>
                ))}
        </div>
    );
}
