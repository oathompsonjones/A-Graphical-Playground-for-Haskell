import { Stack, Typography } from "@mui/material";
import { Examples } from "components/pages/home/examples";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import americanFlag from "assets/images/examples/americanFlag.png";
import flower from "assets/images/examples/flower.png";
import fractalTree from "assets/images/examples/fractalTree.png";
import pacman from "assets/images/examples/pacman.png";
import sineCosine from "assets/images/examples/sineCosine.png";
import styles from "styles/pages/home.module.css";
import unionFlag from "assets/images/examples/unionFlag.png";

// TODO: Warn user before opening a sketch if they have unsaved changes to their current sketch.

/**
 * This is the home page.
 * @returns The home page.
 */
export default function Home(): ReactNode {
    const issuesURL = "https://github.com/oathompsonjones/A-Graphical-Playground-for-Haskell/issues";

    /* eslint-disable max-len */
    const staticExamples: Array<{ name: string; code: string; image: StaticImageData; }> = [
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwAEngAecAEQAGOQCYAjABYA7DNRRecUROkzkyZQGZTeLewrAYI8VNkFnLrZmz4ipclTgsKAIZgeGycAOQ6cADmECRR5BBwjCx4qCwwUMDBbKIE9DxQcAAUAGQAlHB44DAAnnAA2umZwcXAcABUcIodxcoKcnAA9HBmZRUAPnBtADzY9XIAdAvKigC6q6hwHFQ6m1t%2BGVlkdaI61PDKAJwKvf1DI6YVAHwvcER0dHA6PHAvT3DECAAZQyEAA1mQ-uQoAFiCw6AE7ABNOA1dDpAKsPRvfKfUoVZDxRowTFwaR1SbSWYNRbLOSrAA0qLg1PmSwAHIzKgA3PDEYpQJjEH6SColEo8vkCoU-GoTSYQHg-IqCxjCsliiWK5Wq9VyjZbax4Xb7OCE7FneDqABsAyKRT6N2Gox66mer3enwEQl%2Br0BIMFEL2WwxhXJ2NDYV9-wysPhiLIRQAanhztBitJOtaAKxwADUcGzcgqRTqnWzuYLRbGwYOmKjokjUah-tBEOjb2AHzgAHUbHZa03sZA6DUYvz6im04UiqYBp0irQ2LConQ8GNinOekUWCQ4Cu1xva6aT6etpMD2RqUUQEEvgEeMA48Vb2BilXVCWb3eip01J%2BaSWS5VjGMYNncOAAFkAj3PkMjqSASHgCACB8MgwEFKIYRAVBbz3U4%2BR4Y04AAEjgahYW5AI2GmWiDmaEI4AlUMWD2I0TX2CjiConIzQCagwSwmVtlsRNqB0BMAGFKOokZrgGR1i1QIA",
            image: americanFlag,
            name: "Star Spangled Banner",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlOhYuIQkZJRkLBQJYHhsnADkunA1JOnkEHCMLHioLLQwdEk8%2BnBZuQdwAGRwxLUIeE-E6ahwHCoun%2BAMCVxuxB4WzgCXuumo8DaVRqdWifVUxVUkzUFgAfPi4EQ6NE7A4yPjce8IABlGBQCAAa3JBPpiUc7VhfQyWWmoIe3zyv3ulwg11u0NUcAp5BycRujgAmu0MCYpiCAR9iF8fodGuDxcqsVKCXSCvKyG0AGp4BHQQ2qTTYibTVDAXQQu6iR6C9KSt7eg7qV5UrUCg5%2BkPan3qEF2PDAvluvAe6Gw0TwxHI2r1OAAenM2Ms8mlROiuju0o%2BtPpTONlNZCXZbU5MN%2BPPV-J1ktESZTag7Ad%2BQZ77oNkulprljYt1ttUDgmgAHPIimq%2BZqo%2BH7r2DW0MEaJ7KWOalRMOxuw0Pt6PIdC9weTUfze1ZzA7ZpulpLC68G3bvcADd4xgYBqHqCMKGgYAAC9akSOgIyA2BQPqIM3kgqAYLg1DYyBc4%2BSQkCwIQuEbXgYx5AmOtCWAYlATJaiqzpRlmUpSdj2nAANKwug7DCsOScDSIRfpP26EtaJJex2UrGlmNrQ8zWnJUKI7QiULoYc4AzHFKPEglS20vRZOrFjqPY81uMsIs%2BKg2DBM04T4E6boNGLAzJKMisCSYmtWJlJTFXMGyfDgABZBISDgX86QATzgSASHgCACHIKgEvpdIchAVAQEi4hSMheM4AAEhiJIAISNgAB5arBUUPWDHdIWDX9MluXD43w0EwOISq2FEZAEmoBksqYFqBCEdpqF0acAGEKqq0S9LVIA",
            image: unionFlag,
            name: "Union Jack",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYDGANgIZR5wlwAmJMVAzjFAK4EwvlwwRzljkGeAHbwqyYMLIBPbuTypa9OABV5lOAF5EeEgDMNAHzgA5CNQpUAFGryWAlHBvqSji1GAA3SQHMnAZQALCAB3e0U6KiRhCm1oigAxIgg6OCSU%2BHTUrMzk1PcvXwDgsPQsXEJSLhgQ3j0WYXZgCGEGOBDA4AJAyiJkkLgWNp44ZBZgImoNCSkoWWY7VAJyOjwAISgSRsC8NoAuPbS8%2BAwAPkRJCjOneIAaC5jw5d0Ydc3t3cphHyIKK3i4AAPOCyMh%2BX7CdrAagwQKObRWX56GD3Dw%2BQIwcJwdo7ciobHYgBqhB4UCBAHIQZTtMT2NAnMCANRwCFwABUcAIEDaYMcVlkzNZHIYkkoUB8WIJSPgWgeFEBlOklKsYLgzK2PzwfKFcAATHAAPRwADMfJC0Nh7L1hpNkuxaIxsoBCqpTlV2A1v21Iit%2BqNpqc5phPQ5ftt6Geq1sFAOcAAkqI4NccknzgDrtG5UsVq9MxYwJbPRQiJc4MZ85bNNoAAxO3QGEsxfEEluttvt7HGCCwvBQc1CbHaMwWFml6V8YDomDNjq9hQtxF4ZF3CdT%2BLwzk5t5bbqfIujpst8eyyO59RWCs9bAARkc%2B%2Bl8Wb9snju0p7wmYveALV7gt6%2Bmqrhij6YNg%2BDEGQFA1HUDRNC0bQdF0PQkH0oRtEM3C8NQmwDOIkgyHIizYSQIQArG6bnEEJBgAoxGkaW-ylsCoLiiyPpBrCG6NsWIhPh2-ECW2pzCXIWwMKQrxOLSpJAiCdqCQpAnCecUDdqsYo%2BHxinaa2ylwEwqkANYUAA6p0rxaTpVl6QZEDGSZeAvvAHGBOgdGZrGmYUXAVE0YoOGfgg9YHlqCnaHRALcf5JGfsOxZjku8AOpisoRWOlyWVZAkAGQ0AF6jjplWUdrl7nqMlZTYCoOxwCAJCiiIzCyJAkjwBABg9nAAgQD4mwgKgdWirGcYAPJOOEg2Qto5DCO4cAACSclsngkG0AA8G15TF6gLPOOJznxBDLatsrICQBCGb1EANFMVgAEoAOJrHAACsABsr0fe9fLvgAwsdbQAJzVrWwPVvJu0Em%2BW6Zh9VhgMANpvY4zANIZfGo8IhmDnKcAACwvaDINOBgCM2rqjjGsTL2oEAA",
            image: fractalTree,
            name: "Fractal Tree",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgZQKbwwAWecAxgIYB2AbhQM5z3ABeeqjAvHAIwAMA9FjgARKBQDucPBTJE4YCrDgQAZnGKlVAGwgS8UDjDwg43VRG0ATbQHI4ACgB8LgJRMiFMKRjiq9CygQChhgCH9UODgJEih2KKj6T28E1NTuZDwWYAMAJkcANTwyGGhHDEYAejhcvncHKprXeqKSsoa4aoBmesbc5siE32oA6GDQ8K44AG1h-20Q0gdW0qhHPt7OptcAGiZfCABrPAB1PGAAcyJ4fj36A%2BO4AHE4vCoAXVRvGAptKYdvr84AAyBQEX62dwuJxwKgQHAPeLRWJIqKA7QJDJZHJQLqFYqrJhwOr4tprPhMFoEsqMehQlxwVTAbQYgAK2gAriBBmk0qDMtkDHjltTyZTSYTGCSRWSiXS4NDGcy2cAqIceWCfnYotx0QqGXN6AtjAANImKqAQH7GRxgYBbfpfYD3ZlmRmWGz2ZxuDxeHx%2BUZBEJhCJRGIGVG%2BlK89LkYBQMjaJZ8AB0PAArHAAFSUjWGwLjENTWYB41LFbtDbrB3NPZwhGW457JksuAATTwLL0n0w2AAKiRpFRfABPBQQVXwNQaQdgS0XcTc4KquAALlXcAAkgB5RyuVDLqhuuJUKwGOAAEnI1DojAAPA-9iYQZrfoxQXaXdpBuG4hrKLQDBumQcSLAAwjeQEOJaHKnuK0EQLBViUkAA",
            image: flower,
            name: "4 Petal Flower",
        },
    ];

    const animatedExamples: Array<{ name: string; code: string; image: StaticImageData; }> = [
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYDGANgIZR5wlwAmJMVAzjFAK4EwvlwwRzljkGeAHbwqyYMLIBPbuTypa9OABV5lOAF5EeEgDMNAHzgA5CNQpUAFGryWAlHBvqSji1GAA3SQHMnAZQALCAB3e0U6KiRhCm1oigAxIgg6OCSU%2BHTUrMzk1PcvXwDgsPQsXEJSLhgQ3j0WYXZgCGEGOBDA4AJAyiJkkLgWNp44ZBZgImoNCSkoWWY7VAJyOjwAISgSRsC8NoAuPbS8%2BAwAPkRJCjOneIAaC5jw5d0Ydc3t3cphHyIKK3i4AAPOCyMh%2BX7CdrAagwQKObRWX56GD3Dw%2BQIwcJwdo7ciobHYgBqhB4UCBAHIQZTtMT2NAnMCANRwCFwABUcAIEDaYMcVlkzNZHIYkkoUB8WIJSPgWgeFEBlOklKsYLgzK2PzwfKFcAATHAAPRwADMfJC0Nh7L1hpNkuxaIxsoBCqpTlV2A1v21Iit%2BqNpqc5phPQ5ftt6Geq1sFAOcAAkqI4NccknzgDrtG5UsVq9MxYwJbPRQiJc4MZ85bNNoAAxO3QGEsxfEEluttvt7HGCCwvBQc1CbHaMwWFml6V8YDomDNjq9hQtxF4ZF3CdT%2BLwzk5t5bbqfIujpst8eyyO59RWCs9bAARkc%2B%2Bl8Wb9snju0p7wmYveALV7gt6%2Bmqrhij6YNg%2BDEGQFA1HUDRNC0bQdF0PQkH0oRtEM3C8NQmwDOIkgyHIizYSQIQArG6bnEEJBgAoxGkaW-ylsCoLiiyPpBrCG6NsWIhPh2-ECW2pzCXIWwMKQrxOLSpJAiCdqCQpAnCecUDdqsYo%2BHxinaa2ylwEwqkANYUAA6p0rxaTpVl6QZEDGSZeAvvAHGBOgdGZrGmYUXAVE0YoOGfgg9YHlqCnaHRALcf5JGfsOxZjku8AOpisoRWOlyWVZAkAGQ0AF6jjplWUdrl7nqMlZTYCoOxwCAJCiiIzCyJAkjwBABg9nAAgQD4mwgKgdWirGcYAPJOOEg2Qto5DCO4cAACRwHoECTEQTgADybXyy2rRtW2clsngkG0y19mQ1AMI4yAkAQhkhOdDAzri84EgQh3HS22h6GAbTXrWAB0ow3YZvUQA0UxWAASgA4mscAAKwAGwI8jSOOIt74AMLvW0ACc1a1vj1Z8QslifZuLwfuoyNUMwDSGSTrDCIZrZxKWAAs8OEwTTgYGAwA2rqjjGjz8N8ad91QBdg5wAA2mVdhOKTTj8zawD2I4xgC%2Bt2Cy%2Bztzs-98P-f9f0ALpm3x123ZL0vaPL%2BWK1YytWKrRrq5rcDa9g5CeL2A56wbRsm7jFuoEAA",
            image: fractalTree,
            name: "Animated Fractal Tree",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCrAQwMYkwOzgC4i4AxAGwk3gFoA%2BOAZQAtMwBTDHPQ%2BAXjhZcBAORx49KRKgEAzgDNoIVHDgB3Fhyhc1amLPyLlcQQfkUaHOAAoAah2wxocAKwAGd288BKOADo4BWAKCjgATQ5QiHUAuHwIJgMIAGtdPWFecS9BbGAobAprABZPVQyeMQlTIWASzzhSrxt4ACpauAB6OAAOPxtaNo7uvvK1VDxgQhI4AEkAeVsfCcwpmp18ABNtOAASIIgKTYpxGwAeC79sAgA3TDk4AG1MgmqAH2qz2if3ABp3fwARn8QIAunAANQQn7%2BACc-38PRB7lBoPKmm01mu%2BDuD0ECjADwAbM1kDgUgBzKAQACuWzgACFLNgUrZsDorABhW73OCAzxefm%2BHxAA",
            image: pacman,
            name: "Pacman",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoK4DtgwMLBQDGANgKZwBclcAYiRAIbwC0AfHAMoAWjYZmHPkKkKjOHAC8cVBLnyFLFnAAiURgHc44oiPJwNubtqxwAHi0ZngAZwA02uAE9L1%2ByYAmjqBBjNgWADmcOoewBg2sgrREgAUusT6AIwADClwbJlwMOpYNiTMFLEAamREMNBwAEwArOlJdQCUUTExAGRwJAEUVWkSmRw5jHkFMEWl5ZW16U0trfOtAyG%2BhXCxYMBwAPTVzQvRHV1YPX0ZWUMjqyVlFVBwDfWz%2BwqH3fenSxf5VxO31XX3J7PYH9LI%2BPxjbRzBaNM4cGw5CAAazIAHUyMBAtx4ABmaHzJSqdRacSQALwABmlRg3AoRAgNjeADdGCQMAIQR14noKEk4XAET4UXAAPK5QIckH7T65b6Q66TO6xaZwADU73SACo4PSbNpYQ89sDCWpNI4yVhKdTaQLmaz2fjWh0EqJ7qD4YjhQAlMgeR1S%2BQy4Zy8Y3KYA2IPNUauDaxmmRiNI3PE3E80QclwKl3Gl0iDIAL%2BCCmCAU23HTw6hl2tlkSKcnU8t38wXIiio7i4SUBxbnWWjUOKtYq9WpLVVvWJtZR0d9OMBfXJ-awpatlHozHY97%2BntyVCBdRgYzUOgMZhwdhcXj8feH4ziST%2B1Nm4bmVy2ByGGmOJlkWDAIhWU6N5zzMKQ4BSBxhi8YZfFpO5QPAqpYzgDYZFaWIjgoOp0iDS4xgATTgHE6h3OAuSwmN3WyfsfjDO4ARxNIlydYCKzHaivgHNZfkqAAWRjmLIiQVzBFZ5TQnYqhYhRRI9IU0QxLFcX9LlhIvZRTRJBMwDAHxrBAIsSzLHRqwrIgMCgX84AiAJgmQMgAC9gD-HVLN-et9liBznL-Pl%2BS4uih1iFhDTgPihOeDofJcqBcL7YNuIVP5BJSGSDjgGK-IC2j5V4u4VQitL1OipzYviwZcsHP4BPSJjiueOSBU9CgxWGCUyOfbTtF0-TQCMuBS0ceM6XcihbKCTKytciyrLrMiuSyqB-LwkMePotYWCacLIv2UrfLinLEqCv4o3q9KXimg6%2BVWpL8uIgEioukSWxauAfT9VomrXRTN3gKon00tNSQzS0swgEgGEMSbc3LUa5tUxtEl5aifo3ZTt13VG3o%2B9Te0q468o22IAWUXpx3EHYNlhWISPSdUAXnBMk0BokX1Q0GrUhiBoeCWHdTeWbf0Rl1kmxhT0a3VI8ZiVc3raoJux7W6TsqEn0jJudHCp4Aabpi84EZidFyiDR4KVpb0mkJacXWocAHYqgAOgADggmn7qSKp7ad5Cxw9jbyY1Jclr5a3pqgW3ksqR3Xfdu3Tu933Nv9gOhyDkLU-QQyFxPABJEU1maHPTGkKAyCwDxXIAEnBkgPBIAByNYAB429hQCsBZPUAG1sFwAhkccDoDz4e84AAH0cFvlB7yCLxSJ2%2BSdp2L2Q7UNgAXU303zbmTvu-A5BGCIJEDwgbAvCQZSAHFy6cNYiHLwo8GGQ%2BcMNoSgA",
            image: sineCosine,
            name: "Sine and Cosine",
        },
    ];
    /* eslint-enable max-len */

    return (
        <>
            <Stack direction={{ md: "row" }} gap={2} className={styles.title!}>
                <Image src="/favicon.svg" alt="Haskell Logo" width={150} height={100} />
                <div>
                    <Typography variant="h2" component="h2">A Graphical Playground</Typography>
                    <Typography variant="h3" component="h3">for Haskell</Typography>
                    <hr />
                    All the fun, with none of the hassle.
                </div>
            </Stack>

            <div className={styles.section}>
                <Typography variant="h4">
                    Welcome! Whether you have never written a line of code before, or you are a seasoned veteran, you
                        will find something here to enjoy.
                    <br />
                    <br />
                    With this easy-to-use playground, you can get right down to business — no need to install anything,
                        just write some code and see the results. <Link href="/editor">Start Coding!</Link>
                </Typography>
            </div>

            <div className={`edge wrapper ${styles.section} ${styles.big}`}>
                <div className="breakout">
                    <Typography variant="h3">See what you can do...</Typography>
                    <br />
                    <Examples examples={staticExamples} />
                    <Typography variant="h4" textAlign="center">Images</Typography>

                    <Examples examples={animatedExamples} />
                    <Typography variant="h4" textAlign="center">Animations</Typography>
                </div>
            </div>

            <div className={styles.section}>
                <Typography variant="h3">Looking for help?</Typography>
                <Typography variant="h6">
                    Check out the <Link href="/reference">reference page</Link> for documentation.
                    If you have any other questions, or would like to report a bug, please visit
                        the <Link href={issuesURL}>GitHub issue tracker</Link>.
                </Typography>
            </div>
        </>
    );
}
