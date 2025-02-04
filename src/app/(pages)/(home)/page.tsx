import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import { Typography } from "@mui/material";
import americanFlag from "assets/images/examples/americanFlag.png";
import fractalTree from "assets/images/examples/fractalTree.png";
import styles from "styles/pages/home.module.css";
import unionFlag from "assets/images/examples/unionFlag.png";

// TODO: Add more examples.
// TODO: Add more information to the home page.

const examples: Array<{ name: string; code: string; image: StaticImageData; }> = [
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlOhYuIQkZJRkLBQJYHhsnADkunA1JOnkEHCMLHioLLQwdEk8%2BnBZuQdwAGRwxLUIeE-E6ahwHCoun%2BAMCVxuxB4WzgCXuumo8DaVRqdWifVUxVUkzUFgAfPi4EQ6NE7A4yPjce8IABlGBQCAAa3JBPpiUc7VhfQyWWmoIe3zyv3ulwg11u0NUcAp5BycRujgAmu0MCYpiCAR9iF8fodGuDxcqsVKCXSCvKyG0AGp4BHQQ2qTTYibTVDAXQQu6iR6C9KSt7eg7qV5UrUCg5%2BkPan3qEF2PDAvluvAe6Gw0TwxHI2r1OAAenM2Ms8mlROiuju0o%2BtPpTONlNZCXZbU5MN%2BPPV-J1ktESZTag7Ad%2BQZ77oNkulprljYt1ttUDgmgAHPIimq%2BZqo%2BH7r2DW0MEaJ7KWOalRMOxuw0Pt6PIdC9weTUfze1ZzA7ZpulpLC68G3bvcADd4xgYBqHqCMKGgYAAC9akSOgIyA2BQPqIM3kgqAYLg1DYyBc4%2BSQkCwIQuEbXgYx5AmOtCWAYlATJaiqzpRlmUpSdj2nAANKwug7DCsOScDSIRfpP26EtaJJex2UrGlmNrQ8zWnJUKI7QiULoYc4AzHFKPEglS20vRZOrFjqPY81uMsIs%2BKg2DBM04T4E6boNGLAzJKMisCSYmtWJlJTFXMGyfDgABZBISDgX86QATzgSASHgCACHIKgEvpdIchAVAQEi4hSMheM4AAEhiJIAISNgAB5arBUUPWDHdIWDX9MluXD43w0EwOISq2FEZAEmoBksqYFqBCEdpqF0acAGEKqq0S9LVIA",
        image: unionFlag,
        name: "Union Jack",
    },
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwAEngAecAEQAGOQCYAjABYA7DNRRecUROkzkyZQGZTeLewrAYI8VNkFnLrZmz4ipclTgsKAIZgeGycAOQ6cADmECRR5BBwjCx4qCwwUMDBbKIE9DxQcAAUAGQAlHB44DAAngDKgcFwANrpmU1FwHAAVHCKPcXKCnJwAPRwZmUVAD5wXQA82M1yAHQryooAupuocBxUOrt7fhlZZDV6cDrU8MoAnAqDw2MTphUAfJ9wRHR0V7qfd5wYgQOoZCAAazIgPIUACxBYdACdgAmnAauh0gFWJc8nQeH9ShVkPFWjBsXBpBdZtJFi1Vus5JsADTouB05ZrAAcLMqADc8MRilAmMQeJSKiUSvzBcLReKajNZhAeOKiiLGGKJXApXAVWqNVrFTs9tY8IdjnASZdrvB1AA2EZFIpDR7jSYDdQfL4-P4CIRwGEgsEiqFHPZYwpUy6RsKBr4ZeGI5FkIoANTwN2gxWkvXtAFY4ABqOD5uQVIoXXr5wslstTcMnbFx0SxuNB0HgqHxoG%2BuAAdRsdkbbcukDoNRiQuaGazhSKphGvSKtDY8KidDwU2Ki4GRRYJDg68328blvPF72s2PZDpRRAQSuAR4wCTxQfYGKddUFfvj6KvRqD%2B9JrHcmxTFMOzuHAACyASHoKGQXJAJDwBABA%2BGQYAilEcIgKgD6HqIOhiuacAACRwNQ8J8gEbDzAxJztCEOpNqwRxmhaxzUcQtE5FaATUBCuHyvstiptQOgpgAwjRdETA8IyuuWqBAA",
        image: americanFlag,
        name: "American Flag",
    },
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYDGANgIZR5wlwAmJMVAzjFAK4EwvlwwRzljkGeAHbwqyYMLIBPbuTypa9OABV5lOAF5EeEgDMNAHzgA5CNQpUAFGryWAlHBvqSji1GAA3SQHMnAZQALCAB3e0U6KiRhCm1oigAxIgg6OCSU%2BHTUrMzk1PcvXwDgsPQsXEJSLhgQ3j0WYXZgCGEGOBDA4AJAyiJkkLgWNp44ZBZgImoNCSkoWWY7VAJyOjwAISgSRsC8NoAuPbS8%2BAwAPkRJCjOneIAaC5jw5d0Ydc3t3cphHyIKK3i4AAPOCyMh%2BX7CdrAagwQKObRWX56GD3Dw%2BQIwcJwdo7ciobHYgBqhB4UCBAHIQZTtMT2NAnMCANRwCFwABUcAIEDaYMcVlkzNZHIYkkoUB8WIJSPgWgeFEBlOklKsYLgzK2PzwfKFcAATHAAPRwADMfJC0Nh7L1hpNkuxaIxsoBCqpTlV2A1v21Iit%2BqNpqc5phPQ5ftt6Geq1sFAOcAAkqI4NccknzgDrtG5UsVq9MxYwJbPRQiJc4MZ85bNNoAAxO3QGEsxfEEluttvt7HGCCwvBQc1CbHaMwWFml6V8YDomDNjq9hQtxF4ZF3CdT%2BLwzk5t5bbqfIujpst8eyyO59RWCs9bAARkc%2B%2Bl8Wb9snju0p7wmYveALV7gt6%2Bmqrhij6YNg%2BDEGQFA1HUDRNC0bQdF0PQkH0oRtEM3C8NQmwDOIkgyHIizYSQIQArG6bnEEJBgAoxGkaW-ylsCoLiiyPpBrCG6NsWIhPh2-ECW2pzCXIWwMKQrxOLSpJAiCdqCQpAnCecUDdqsYo%2BHxinaa2ylwEwqkANYUAA6p0rxaTpVl6QZEDGSZeAvvAHGBOgdGZrGmYUXAVE0YoOGfgg9YHlqCnaHRALcf5JGfsOxZjku8AOpisoRWOlyWVZAkAGQ0AF6jjplWUdrl7nqMlZTYCoOxwCAJCiiIzCyJAkjwBABg9nAAgQD4mwgKgdWirGcYAPJOOEg2Qto5DCO4cAACSclsngkG0AA8G15TF6gLPOOJznxBDLatsrICQBCGb1EANFMVgAEoAOJrHAACsABsr0fe9fLvgAwsdbQAJzVrWwPVvJu0Em%2BW6Zh9VhgMANpvY4zANIZfGo8IhmDnKcAACwvaDINOBgCM2rqjjGsTL2oEAA&title=Fractal%2520Tree&author=examples",
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
