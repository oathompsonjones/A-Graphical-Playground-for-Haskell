import type { ReactNode } from "react";
import { Typography } from "@mui/material";

/**
 * This is the reference page.
 * @returns The home reference.
 */
export default function Reference(): ReactNode {
    return (
        <div>
            <Typography variant="h2">Reference</Typography>
            <br />
            <Typography>
                Lorem, ipsum dolor sit amet consectetur adipisicing elit.
                Hic, cupiditate natus eaque voluptatem eum magni voluptates,
                doloribus at illum animi voluptate saepe praesentium perspiciatis
                fugit esse! Pariatur veniam recusandae repellat.
            </Typography>
            <br />
            <Typography>
                Lorem ipsum dolor sit amet consectetur adipisicing elit.
                Quo, esse voluptates nam nisi molestias a amet reprehenderit
                quaerat dolorem doloribus ab veritatis modi debitis exercitationem,
                obcaecati similique. Officia, consequatur id?
            </Typography>
            <br />
            <Typography>
                Lorem ipsum dolor sit amet consectetur, adipisicing elit.
                Laborum pariatur hic inventore neque quam impedit nemo cumque
                maxime magnam veritatis fuga obcaecati dolor natus ipsum ratione
                corporis, voluptas praesentium consectetur?
            </Typography>
        </div>
    );
}
