"use client";

import { Icon, Stack, Typography } from "@mui/material";
import { Angles } from "components/pages/reference/sections/other/angles";
import { Arc } from "components/pages/reference/sections/shapes/primitives/arc";
import { Bezier2 } from "components/pages/reference/sections/shapes/primitives/bezier2";
import { Bezier3 } from "components/pages/reference/sections/shapes/primitives/bezier3";
import { Canvas } from "components/pages/reference/sections/canvas";
import { Center } from "components/pages/reference/sections/transformations/shorthands/center";
import { ChainingTransformations } from "components/pages/reference/sections/transformations/chainingTransformations";
import { Circle } from "components/pages/reference/sections/shapes/nonPrimitives/circle";
import { ColorsRoot } from "components/pages/reference/sections/colors/root";
import { CombiningShapes } from "components/pages/reference/sections/shapes/combiningShapes";
import { Contents } from "components/pages/reference/contents";
import { Controls } from "components/pages/reference/controls";
import { Ellipse } from "components/pages/reference/sections/shapes/primitives/ellipse";
import { Empty } from "components/pages/reference/sections/shapes/primitives/empty";
import { Fill } from "components/pages/reference/sections/transformations/fill";
import { HaskellRoot } from "components/pages/reference/sections/haskell/root";
import { ImagesAndAnimations } from "components/pages/reference/sections/imagesAndAnimations";
import { KeyboardReturn } from "@mui/icons-material";
import { Line } from "components/pages/reference/sections/shapes/primitives/line";
import { NoFill } from "components/pages/reference/sections/transformations/shorthands/noFill";
import { NoStroke } from "components/pages/reference/sections/transformations/shorthands/noStroke";
import { NonPrimitivesRoot } from "components/pages/reference/sections/shapes/nonPrimitives/root";
import { NotationExamples } from "components/pages/reference/sections/haskell/notation/examples";
import { NotationRoot } from "components/pages/reference/sections/haskell/notation/root";
import { OperatorPrecedence } from "components/pages/reference/sections/other/operatorPrecedence";
import { Pie } from "components/pages/reference/sections/shapes/primitives/pie";
import { Polygon } from "components/pages/reference/sections/shapes/primitives/polygon";
import { PrimitivesRoot } from "components/pages/reference/sections/shapes/primitives/root";
import { RandomNumbers } from "components/pages/reference/sections/other/randomNumbers";
import type { ReactNode } from "react";
import { Rect } from "components/pages/reference/sections/shapes/primitives/rect";
import { Regular } from "components/pages/reference/sections/shapes/nonPrimitives/regular";
import { Rotate } from "components/pages/reference/sections/transformations/rotate";
import { Scale } from "components/pages/reference/sections/transformations/scale";
import { Section } from "components/pages/reference/section";
import { Segment } from "components/pages/reference/sections/shapes/primitives/segment";
import { Square } from "components/pages/reference/sections/shapes/nonPrimitives/square";
import { Stroke } from "components/pages/reference/sections/transformations/stroke";
import { StrokeWeight } from "components/pages/reference/sections/transformations/strokeWeight";
import { TransformationsRoot } from "components/pages/reference/sections/transformations/root";
import { Translate } from "components/pages/reference/sections/transformations/translate";
import { TranslateX } from "components/pages/reference/sections/transformations/shorthands/translateX";
import { TranslateY } from "components/pages/reference/sections/transformations/shorthands/translateY";
import { Vectors } from "components/pages/reference/sections/vectors";

export type SectionType = ((list: (elems: ReactNode[]) => ReactNode) => ReactNode) | {
    [key: string]: SectionType;
    root?: (list: (elems: ReactNode[]) => ReactNode) => ReactNode;
};

const docs: Record<string, SectionType> = {
    /* eslint-disable @typescript-eslint/naming-convention, sort-keys */
    Haskell: {
        root: () => <HaskellRoot />,
        Notation: {
            root: () => <NotationRoot />,
            Examples: () => <NotationExamples />,
        },
    },
    Canvas: () => <Canvas />,
    "Images and Animations": () => <ImagesAndAnimations />,
    Vectors: (list) => <Vectors list={list} />,
    Shapes: {
        "2D Primitives": {
            root: () => <PrimitivesRoot />,
            line: () => <Line />,
            ellipse: () => <Ellipse />,
            rect: () => <Rect />,
            polygon: () => <Polygon />,
            bezier2: () => <Bezier2 />,
            bezier3: () => <Bezier3 />,
            arc: () => <Arc />,
            pie: () => <Pie />,
            segment: () => <Segment />,
            empty: () => <Empty />,
        },
        "Non-Primatives": {
            root: () => <NonPrimitivesRoot />,
            circle: () => <Circle />,
            square: () => <Square />,
            regular: () => <Regular />,
        },
        "Combining Shapes": () => <CombiningShapes />,
    },
    Transformations: {
        root: () => <TransformationsRoot />,
        fill: () => <Fill />,
        stroke: () => <Stroke />,
        strokeWeight: () => <StrokeWeight />,
        translate: () => <Translate />,
        rotate: () => <Rotate />,
        scale: () => <Scale />,
        Shorthands: {
            noFill: () => <NoFill />,
            noStroke: () => <NoStroke />,
            translateX: () => <TranslateX />,
            translateY: () => <TranslateY />,
            center: () => <Center />,
        },
        "Chaining Transformations": (list) => <ChainingTransformations list={list} />,
    },
    Colors: (list) => <ColorsRoot list={list} />,
    Other: {
        Angles: (list) => <Angles list={list} />,
        "Random Numbers": (list) => <RandomNumbers list={list} />,
        "Operator Precedence": (list) => <OperatorPrecedence list={list} />,
    },
    /* eslint-enable @typescript-eslint/naming-convention, sort-keys */
};

/**
 * This is the reference page.
 * @param props - The properties of the page.
 * @param props.hideControls - Whether to hide the controls.
 * @returns The reference page.
 */
export default function Reference({ hideControls }: { hideControls?: boolean; }): ReactNode {
    return (
        <>
            <div className="wrapper">
                <Typography variant="h2">Reference</Typography>
                <Stack direction={{ md: "row" }} alignItems="top" gap={5}>
                    <Contents docs={docs} />
                    {!(hideControls ?? false) && <Controls controls={[
                        [<KeyboardReturn />, "Run your sketch."],
                        [<Icon>S</Icon>, "Save your sketch."],
                        [<Icon>O</Icon>, "Open one of your saved sketches."],
                        [<Icon>N</Icon>, "Create a new sketch."],
                        [<Icon>/</Icon>, "Comment/uncomment the current line."],
                    ]} />}
                </Stack>
            </div>
            {Object.entries(docs).map(([title, content], i) => (
                <Section title={title} content={content} depth={0} colored={i % 2 === 0} key={i} />
            ))}
        </>
    );
}
