import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/console.module.css";

/**
 * This is the console to display code outputs.
 * @param props - The properties of the component.
 * @param props.content - The content to display on the console.
 * @returns The console element.
 */
export function Console({ content }: { content?: string; }): ReactNode {
    return (
        <PlainPaper className={styles.console!}>
            <pre>
                {content}
                Lorem ipsum dolor sit amet consectetur, adipisicing elit.
                Dolor fugiat cupiditate sint autem, inventore consequatur.
                Error aut iste quidem, voluptates maiores assumenda cumque,
                sed vero optio pariatur perferendis, exercitationem quaerat?
                <br /><br />
                Lorem ipsum dolor sit amet consectetur adipisicing elit.
                Non voluptas illo sunt rerum ea ad consequatur doloremque ipsa
                provident, consectetur excepturi? Odio placeat recusandae pariatur.
                Maxime voluptatibus perspiciatis alias voluptates.
                <br /><br />
                Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                Aperiam distinctio cumque qui alias sequi neque asperiores suscipit,
                nesciunt est fugiat hic dicta. Saepe enim, facilis porro ab
                a placeat assumenda!
                <br /><br />
                Lorem ipsum dolor sit amet consectetur adipisicing elit.
                Non voluptas illo sunt rerum ea ad consequatur doloremque ipsa
                provident, consectetur excepturi? Odio placeat recusandae pariatur.
                Maxime voluptatibus perspiciatis alias voluptates.
                <br /><br />
                Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                Aperiam distinctio cumque qui alias sequi neque asperiores suscipit,
                nesciunt est fugiat hic dicta. Saepe enim, facilis porro ab
                a placeat assumenda!
            </pre>
        </PlainPaper>
    );
}
