import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import { Typography } from "@mui/material";
import americanFlag from "assets/images/examples/americanFlag.png";
import fractalTree from "assets/images/examples/fractalTree.png";
import styles from "styles/pages/home.module.css";
import unionFlag from "assets/images/examples/unionFlag.png";

const examples: Array<{ name: string; code: string; image: StaticImageData; }> = [
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlOhYuIQkZJRkLBQJYHhsnADkunA1JOnkEHCMLHioLLQwdEk8%2BnBZuQdwAGRwxLUIeE-E6ahwHCoun%2BAMCVxuxB4WzgCXuumo8DaVRqdWifVUxVUkzUFgAfPi4EQ6NE7A4yPjce8IABlGBQCAAa3JBPpiUc7VhfQyWWmoIe3zyv3ulwg11u0NUcAp5BycRujgAmu0MCYpiCAR9iF8fodGuDxcqsVKCXSCvKyG0AGp4BHQQ2qTTYibTVDAXQQu6iR6C9KSt7eg7qV5UrUCg5%2BkPan3qEF2PDAvluvAe6Gw0TwxHI2r1OAAenM2Ms8mlROiuju0o%2BtPpTONlNZCXZbU5MN%2BPPV-J1ktESZTag7Ad%2BQZ77oNkulprljYt1ttUDgmgAHPIimq%2BZqo%2BH7r2DW0MEaJ7KWOalRMOxuw0Pt6PIdC9weTUfze1ZzA7ZpulpLC68G3bvcADd4xgYBqHqCMKGgYAAC9akSOgIyA2BQPqIM3kgqAYLg1DYyBc4%2BSQkCwIQuEbXgYx5AmOtCWAYlATJaiqzpRlmUpSdj2nAANKwug7DCsOScDSIRfpP26EtaJJex2UrGlmNrQ8zWnJUKI7QiULoYc4AzHFKPEglS20vRZOrFjqPY81uMsIs%2BKg2DBM04T4E6boNGLAzJKMisCSYmtWJlJTFXMGyfDgABZBISDgX86QATzgSASHgCACHIKgEvpdIchAVAQEi4hSMheM4AAEhiJIAISNgAB5arBUUPTgWrqrgHdISaurf0yW5cPjfDQTA4hKrYURkASagGSyph2oEIR2moXRpwAYQqqrRL0tUgA",
        image: unionFlag,
        name: "Union Jack",
    },
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwAEngAecAEQAGOQCYAjABYA7DNRRecUROkzkyZQGZTeLewrAYI8VNkFnLrZmz4ipclTgsKAIZgeGycAOQ6cADmECRR5BBwjCx4qCwwUMDBbKIE9DxQcAAUAGQAlHB44DAAngDKgcFwANrpmU1FwHAAVHCKPcXKCnJwAPRwZmUVAD5wXQA82M1yAHQryooAupuocBxUOrt7fhlZZDV6cDrU8MoAnAqDw2MTphUAfJ9wRHR0V7qfd5wYgQOoZCAAazIgPIUACxBYdACdgAmnAauh0gFWJc8nQeH9ShVkPFWjBsXBpBdZtJFi1Vus5JsADTouB05ZrAAcLMqADc8MRilAmMQeJSKiUSvzBcLReKajNZhAeOKiiLGGKJXApXAVWqNVrFTs9tY8IdjnASZdrvB1AA2EZFIpDR7jSYDdQfL4-P4CIRwGEgsEiqFHPZYwpUy6RsKBr4ZeGI5FkIoANTwN2gxWkvXtAFY4ABqOD5uQVIoXXr5wslstTcMnbFx0SxuNB0HgqHxoG%2BuAAdRsdkbbcukDoNRiQuaGazhSKphGvSKtDY8KidDwU2Ki4GRRYJDg68328blvPF72s2PZDpRRAQSuAR4wCTxQfYGKddUFfvj6KvRqD%2B9JrHcmxTFMOzuHAACyASHoKGQXJAJDwBABA%2BGQYAilEcIgKgD6HqIOhiuacAACRwNQ8J8gEbDzAxJztCE7KMZGLBHGaFrHNRxC0TkVoBNQEK4fK%2By2Km1A6CmADCNF0RMDwjK65aoEAA",
        image: americanFlag,
        name: "American Flag",
    },
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoCYEMZbgFSgFMi48BeRIrAMzLgB84A5CDUvACkJLIEo43YhwHsowAG7AAdgHNBAZQAWEAO59MOPEmmlKO0gDEANhBxwTZ%2BJfM3rp82Mkz5nZWo2oi0jJBnwALgDEGVIAWgA%2BOAA1IgBjGGgvHz9peE4DOAAPOABPMih5Y284VWAMGCUBSliE6EEcgGo4Yuk4ACo4OIgAZwLZAU585taOuB6Zfs844hwiACEoLGk4pSI%2BoIsHeEiQ3ThdjNCAGj2iDRnqGAWllbW%2B5dliwUyc-KxClpKyiqq4Sk4xRoMFO4lkShgGjgpTWxFQ0OhtUSUGyAHI8ujKN5fBB-C9Qtk8v0vm0fpUoQigfB-mc0RjBB95M1HsVBqNOgAmOAAejgAGZBmSlGMubyBfCEWCITTXujcujOIyDmQ5KzBOy4KL%2BYLypURTztehLnMeKRNgBJNIHKJ2a203ams6oY3XR3sMB6lmkYwEpjuvXkSgABhl1DoPt0EoR0ZjsbjjDgEEqRCgZR6pBprHYLQJVLgUpgEtUsKIUfVRGBBlBwHBMAM1S6s2ui2Wq3WKqe3tCZbzNJdREdnH9wrCcAAjAIvV9K93owW%2B02B8JBMPlROO88CwZ0BglqpMptMrtlFgwKXd1h9wSjvs3sTRkKGxHvd4y-H3x%2BYxFv3AYLcesYcyCEi9RvBSn4QZ%2B35RFASZAYyb6QUh0bQeMf4QAA1qQADqSjANciHIchqE9OhWHYUQNbSkKO57o6myOkeUQnmemB0cuGRhjmugCJ%2BlAXle%2BzPmxl6DlmXZCRW8AFg2AmZMJRFEQAZHAAmOnmhGKfGKlqcuBboCAWCTBaADyggaIZkyUMQPgpnAAAkXTLBIWB9AAPB5qnsbwf4kEWJZlnEzmuTSyBYHEGGyLBACuPiCAASgA4vMcAAKwAGxpZlGWDP2ADCwV9AAnEGIYlUG4G-su0KUP2jqZZwYDAAa6UCH%2BsUYWW7XSBhNW0gALKlZWlYIYRNQaHICHyI2pagQA",
        image: fractalTree,
        name: "Fractal Tree",
    },
];

/**
 * This is the home page.
 * @returns The home page.
 */
export default function Home(): ReactNode {
    return (
        <div>
            <Typography variant="h2">Home Page</Typography>
            <br />
            <Typography variant="h4">Examples</Typography>
            <div className={styles.examples}>
                {examples.map(({ name, code, image }, i) => (
                    <div className={styles.example} key={i}>
                        <Image src={image} alt={name} width={1000} height={600} />
                        <Link href={`/editor?code=${code}&title=${encodeURIComponent(name)}&author=examples`}>
                            {name}
                        </Link>
                    </div>
                ))}
            </div>
        </div>
    );
}
