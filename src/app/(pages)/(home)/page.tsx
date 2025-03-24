import { Stack, Typography } from "@mui/material";
import { Examples } from "components/pages/home/examples";
import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import americanFlag from "assets/images/examples/americanFlag.png";
import australia from "assets/images/examples/australia.png";
import balls from "assets/images/examples/balls.png";
import flower from "assets/images/examples/flower.png";
import fractalTree from "assets/images/examples/fractalTree.png";
import pacman from "assets/images/examples/pacman.png";
import pyramids from "assets/images/examples/pyramids.png";
import sineCosine from "assets/images/examples/sineCosine.png";
import stopwatch from "assets/images/examples/stopwatch.png";
import styles from "styles/pages/home.module.css";
import ukrainianFlag from "assets/images/examples/ukrainianFlag.png";
import unionFlag from "assets/images/examples/unionFlag.png";

/**
 * This is the home page.
 * @returns The home page.
 */
export default function Home(): ReactNode {
    /* eslint-disable max-len */
    const staticExamples: Array<{ name: string; code: string; image: StaticImageData; }> = [
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwAEngAecAEQAGOQCYAjABYA7DNRRecUROkzkyZQGZTeLewrAYI8VNkFnLrZmz4ipclTgsKAIZgeGycAOQ6cADmECRR5BBwjCx4qCwwUMDBbKIE9DxQcAAUAGQAlHB44DAAnnAA2umZwcXAcABUcIodxcoKcnAA9HBmZRUAPnBtADzY9XIAdAvKigC6q6hwHFQ6m1t%2BGVlkdaI61PDKAJwKvf1DI6YVAHwvcER0dHA6PHAvT3DECAAZQyEAA1mQ-uQoAFiCw6AE7ABNOA1dDpAKsPRvfKfUoVZDxRowTFwaR1SbSWYNRbLOSrAA0qLg1PmSwAHIzKgA3PDEYpQJjEH6SColEo8vkCoU-GoTSYQHg-IqCxjCsliiWK5Wq9VyjZbax4Xb7OCE7FneDqABsAyKRT6N2Gox66mer3enwEQl%2Br0BIMFEL2WwxhXJ2NDYV9-wysPhiLIRQAanhztBitJOtaAKxwADUcGzcgqRTqnWzuYLRbGwYOmKjokjUah-tBEOjb2AHzgAHUbHZa03sZA6DUYvz6im04UiqYBp0irQ2LConQ8GNinOekUWCQ4Cu1xva6aT6etpMD2RqUUQEEvgEeMA48Vb2BilXVCWb3eip01J%2BaSWS5VjGMYNncOAAFkAj3PkMjqSASHgCACB8MgwEFKIYRAVBbz3U4%2BR4Y04AAEjgahYW5AI2GmWiDmaEI4AlUMWD2I0TX2CjiConIzQCagwSwmVtlsRNqB0BMAGFKOokZrgGR1i1QIA",
            image: americanFlag,
            name: "Star Spangled Banner",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCuQGwK4FM4C8cAErgB5wBEAxAAy0CsAbAEICclqAnrpphAHdCJclWoAxcQBEAHPU6oQAQ2AA7OAC4NcAJIB5OAAoAlIpXqiUXKoAmuKHAAkcZEoDGAawDmUCNltwPHyCRm5WSjC4AMJKqgBuSgDOcGz0cEz0xnAAPLlwVm7wqbRwAMxpAHxVcABmwHwuOPhVFXCqEADKML4euKhAA",
            image: ukrainianFlag,
            name: "Slava Ukraini",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlOhYuIQkZJRkLBQJYHhsnADkunA1JOnkEHCMLHioLLQwdEk8%2BnBZuQdwAGRwxLUIeE-E6ahwHCoun%2BAMCVxuxB4WzgCXuumo8DaVRqdWifVUxVUkzUFgAfPi4EQ6NE7A4yPjce8IABlGBQCAAa3JBPpiUc7VhfQyWWmoIe3zyv3ulwg11u0NUcAp5BycRujgAmu0MCYpiCAR9iF8fodGuDxcqsVKCXSCvKyG0AGp4BHQQ2qTTYibTVDAXQQu6iR6C9KSt7eg7qV5UrUCg5%2BkPan3qEF2PDAvluvAe6Gw0TwxHI2r1OAAenM2Ms8mlROiuju0o%2BtPpTONlNZCXZbU5MN%2BPPV-J1ktESZTag7Ad%2BQZ77oNkulprljYt1ttUDgmgAHPIimq%2BZqo%2BH7r2DW0MEaJ7KWOalRMOxuw0Pt6PIdC9weTUfze1ZzA7ZpulpLC68G3bvcADd4xgYBqHqCMKGgYAAC9akSOgIyA2BQPqIM3kgqAYLg1DYyBc4%2BSQkCwIQuEbXgYx5AmOtCWAYlATJaiqzpRlmUpSdj2nAANKwug7DCsOScDSIRfpP26EtaJJex2UrGlmNrQ8zWnJUKI7QiULoYc4AzHFKPEglS20vRZOrFjqPY81uMsIs%2BKg2DBM04T4E6boNGLAzJKMisCSYmtWJlJTFXMGyfDgABZBISDgX86QATzgSASHgCACHIKgEvpdIchAVAQEi4hSMheM4AAEhiJIAISNgAB5arBUUPWDHdIWDX9MluXD43w0EwOISq2FEZAEmoBksqYFqBCEdpqF0acAGEKqq0S9LVIA",
            image: unionFlag,
            name: "Keep Calm and Carry On",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlLbAPJT65gOoVMDpFPCiGl3oWHAAwhBgwHp5utQwdACecARQweRUcACqANI3dNVweAAeCeBZqBYtAuSR4CyyuRI6TgADI4MRagg8JDiOlUHAOFRdOiMYFgR9iDwAORwBILM7wNpVGp1aJ9VTFVSTNQWAB87JuwDo0TsDjI7NZ8IgAGUYHcANb8jl3RKOdpkvoZLLTXFwCF5VELIEQEGEkmqOAC8g5OIfRwATXaGBMUxxGIRxCRKOhjXxoJJbQwTMNHLFBTNZDaADU8OdoFbVKpNMyJtNUMBdASwaJ1VCDXDU6j1LChY7kRr0unc06C%2BocXY8NjVQm8EmSWTRBT2tTavU4AB6czMyzyI1EblwXRgo0I0USqWCmUJOVtBWk1HKu1q-NphY1utqJeZ9LZ0Tr91qH2Cv2m6eBkNhqBwTQADnkRVtqodJdXe8TB893qNJ5YActEyXZ8VyzNd3z1CNmW-E1fzPdoLxgcNNG6LRLDjPAF1BBYADdKxgYBqHqIsKGgYAAC9akSOgixw2B8PqbM4WIqAyIo%2BjyyxPAlxovCCKo8lQ3gYx5AmI9OQHXk5RHEUxQgSVRJ-AMAA0rG2VUmJY5JCP485%2BmQ7o%2By5Hl7EkjlRxkuSoP9M9LSErjcLouhd0HASWWE-SOX7aIh1Eszx3k6ClKsHsl3U8jNMc7T4E6botlEzznOHUzpL8yzTwtcxgp8fZgmQEgyEoMgwASWA2AgAgHjIV53k%2BBD0jwAqr0w3IwR%2BP1nPONgEJiCAaIquAesrAgGHYVBGHFLU3UJHN9ymuF0MyUElzhZpGGKwN2FmeZO3USDfQCs9lLaDa5goDsuziwy4AEIQfOS2TONVOEmyOzbTu25k2hWNZ4He-yrPSz68FWdYzp2i6B2uichTHe6dmwCR0MrWCyTMSASEcMF4mKwFEivZM4EgK4aTgABteDwzab44GAD7rhpuAAD0egZ9pgHK4A4AAAxACAeE58x9FEeQCuIOAWroFgyEsXoxbO0pmQAH2puAAB5sBJg1imKNRLAAXV1pdcSNKcZwwI5Qd2wV4oAdWMqHfNhjEKyrXElScbM%2BnNztVEsJcqY50RaDYWcFzIPpbmCABJZI8HSHJohppc6a1Eh5VDmWI5AaPHDjttE6ygAFCAWAcYBaj6rHWBx4q2EaXH2gUGWvpB7apj%2BtLzwEinjq2qwPsbvpm5%2BvuVVVHNK-aIezp9y3jX%2BzvLwbmWe7evvJ6B76zqUSZR8ewJ68B4Hh5n9uYJncmrzaAeOFes7UPadxB43luRh3w3x4Pqfvfv1Kz4XhDL6PxvidM6N4PpmCfkfae29d64mWp-Z%2Bx8f57XnnBLul8FAGj6CvM6ygPo3lKE3RBoMexvzHh-Yq68oHbWQcefa590HtGltg2%2BnZoztFUNfL%2BGU35ZTEOhHglY%2BpDWqKgEACRU4AC5JFwEjgAeXaNMcRqdGwCKEQAEiugkag4o45MCmpDdo1BdBnj2EkLCCQ2A4KHsyFWdi4BjQoVXIAA",
            image: australia,
            name: "Down Under",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCrATygQxMAEwGc4AuUuAZQAscwBTDbPQkgXjkgBtMBzCAHZwA2gDV6AYxjQ4AZgAM8uADZFAGjjipMgByKV6zZOlQ4AVjNKATJYC6cAHxO4AM2BcuVHAIKYAQlAQAO4CqHDhEREAZJwQPPxCYsYyCkqq8hpaJnL6lhlG2qYW1naOzm4eVMD0AgI4YZGRMdx8giJZMuFphh2m6Qb5vTlKqfZODq7ungDC1BAScTgwjI1NsfFtSYUR3YPJpl3mPfvDw2PlU14EBFz0AcGhq9HrrYlDqQOZJwCc%2Bulf236ABZFOcJhVPAB1aj0JYNJ7NOKvdonD7-ArZKx-Y6A-Qg%2BRgyaVAAq3nQRAArkJyFRaAxUJShBwABQSYBQCS3OSWMrgy7MgAS9AAHnAAEQAYhcLiBeQsYoAlHB4c82RyuR9xkTPAB5XACXj0ABK9AIKvCSq1MH1RC4S3ocGZQ3xcHxCvQAFoPVQYDhYHAglBgDBgAa4JgIBTTAsCA6YVB6AA6VAgHChsgUACSOsd7tT6Y4CZ89FMABI4BJvAA3HAkAA8DcdjLgzWY%2BGIlucAgglGtEAA1isA-GHZWBDX2HBkDgJP3eIEqQQ4AARP39vXeQ2OiQJ%2B3Tau1uB6XbuoA",
            image: pyramids,
            name: "Pyramids",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYDGANgIZR5wlwAmJMVAzjFAK4EwvlwwRzljkGeAHbwqyYMLIBPbuTypa9OABV5lOAF5EeEgDMNAHzgA5CNQpUAFGryWAlHBvqSji1GAA3SQHMnAZQALCAB3e0U6KiRhCm1oigAxIgg6OCSU%2BHTUrMzk1PcvXwDgsPQsXEJSLhgQ3j0WYXZgCGEGOBDA4AJAyiJkkLgWNp44ZBZgImoNCSkoWWY7VAJyOjwAISgSRsC8NoAuPbS8%2BAwAPkRJCjOneIAaC5jw5d0Ydc3t3cphHyIKK3i4AAPOCyMh%2BX7CdrAagwQKObRWX56GD3Dw%2BQIwcJwdo7ciobHYgBqhB4UCBAHIQZTtMT2NAnMCANRwCFwABUcAIEDaYMcVlkzNZHIYkkoUB8WIJSPgWgeFEBlOklKsYLgzK2PzwfKFcAATHAAPRwADMfJC0Nh7L1hpNkuxaIxsoBCqpTlV2A1v21Iit%2BqNpqc5phPQ5ftt6Geq1sFAOcAAkqI4NccknzgDrtG5UsVq9MxYwJbPRQiJc4MZ85bNNoAAxO3QGEsxfEEluttvt7HGCCwvBQc1CbHaMwWFml6V8YDomDNjq9hQtxF4ZF3CdT%2BLwzk5t5bbqfIujpst8eyyO59RWCs9bAARkc%2B%2Bl8Wb9snju0p7wmYveALV7gt6%2Bmqrhij6YNg%2BDEGQFA1HUDRNC0bQdF0PQkH0oRtEM3C8NQmwDOIkgyHIizYSQIQArG6bnEEJBgAoxGkaW-ylsCoLiiyPpBrCG6NsWIhPh2-ECW2pzCXIWwMKQrxOLSpJAiCdqCQpAnCecUDdqsYo%2BHxinaa2ylwEwqkANYUAA6p0rxaTpVl6QZEDGSZeAvvAHGBOgdGZrGmYUXAVE0YoOGfgg9YHlqCnaHRALcf5JGfsOxZjku8AOpisoRWOlyWVZAkAGQ0AF6jjplWUdrl7nqMlZTYCoOxwCAJCiiIzCyJAkjwBABg9nAAgQD4mwgKgdWirGcYAPJOOEg2Qto5DCO4cAACSclsngkG0AA8G15TF6gLPOOJznxBDLatsrICQBCGb1EANFMVgAEoAOJrHAACsABsr0fe9fLvgAwsdbQAJzVrWwPVvJu0Em%2BW6Zh9VhgMANpvY4zANIZfGo8IhmDnKcAACwvaDINOBgCM2rqjjGsTL2oEAA",
            image: fractalTree,
            name: "Fractal Tree",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgZQKbwwAWecAxgIYB2AbhQM5z3ABeeqjAvHAIwAMA9FjgARKBQDucPBTJE4YCrDgQAZnGKlVAGwgS8UDjDwg43VRG0ATbQHI4ACgB8LgJRMiFMKRjiq9CygQChhgCH9UODgJEih2KKj6T28E1NTuZDwWYAMAJkcANTwyGGhHDEYAejhcvncHKprXeqKSsoa4aoBmesbc5siE32oA6GDQ8K44AG1h-20Q0gdW0qhHPt7OptcAGiZfCABrPAB1PGAAcyJ4fj36A%2BO4AHE4vCoAXVRvGAptKYdvr84AAyBQEX62dwuJxwKgQHAPeLRWJIqKA7QJDJZHJQLqFYqrJhwOr4tprPhMFoEsqMehQlxwVTAbQYgAK2gAriBBmk0qDMtkDHjltTyZTSYTGCSRWSiXS4NDGcy2cAqIceWCfnYotx0QqGXN6AtjAANImKqAQH7GRxgYBbfpfYD3ZlmRmWGz2ZxuDxeHx%2BUZBEJhCJRGIGVG%2BlK89LkYBQMjaJZ8AB0PAArHAAFSUjWGwLjENTWYB41LFbtDbrB3NPZwhGW457JksuAATTwLL0n0w2AAKiRpFRfABPBQQVXwNQaQdgS0XcTc4KquAALlXcAAkgB5RyuVDLqhuuJUKwGOAAEnI1DojAAPA-9iYQZrfoxQXaXdpBuG4hrKLQDBumQcSLAAwjeQG0kAA",
            image: flower,
            name: "4 Petal Flower",
        },
    ];

    const animatedExamples: Array<{ name: string; code: string; image: StaticImageData; }> = [
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYDGANgIZR5wlwAmJMVAzjFAK4EwvlwwRzljkGeAHbwqyYMLIBPbuTypa9OABV5lOAF5EeEgDMNAHzgA5CNQpUAFGryWAlHBvqSji1GAA3SQHMnAZQALCAB3e0U6KiRhCm1oigAxIgg6OCSU%2BHTUrMzk1PcvXwDgsPQsXEJSLhgQ3j0WYXZgCGEGOBDA4AJAyiJkkLgWNp44ZBZgImoNCSkoWWY7VAJyOjwAISgSRsC8NoAuPbS8%2BAwAPkRJCjOneIAaC5jw5d0Ydc3t3cphHyIKK3i4AAPOCyMh%2BX7CdrAagwQKObRWX56GD3Dw%2BQIwcJwdo7ciobHYgBqhB4UCBAHIQZTtMT2NAnMCANRwCFwABUcAIEDaYMcVlkzNZHIYkkoUB8WIJSPgWgeFEBlOklKsYLgzK2PzwfKFcAATHAAPRwADMfJC0Nh7L1hpNkuxaIxsoBCqpTlV2A1v21Iit%2BqNpqc5phPQ5ftt6Geq1sFAOcAAkqI4NccknzgDrtG5UsVq9MxYwJbPRQiJc4MZ85bNNoAAxO3QGEsxfEEluttvt7HGCCwvBQc1CbHaMwWFml6V8YDomDNjq9hQtxF4ZF3CdT%2BLwzk5t5bbqfIujpst8eyyO59RWCs9bAARkc%2B%2Bl8Wb9snju0p7wmYveALV7gt6%2Bmqrhij6YNg%2BDEGQFA1HUDRNC0bQdF0PQkH0oRtEM3C8NQmwDOIkgyHIizYSQIQArG6bnEEJBgAoxGkaW-ylsCoLiiyPpBrCG6NsWIhPh2-ECW2pzCXIWwMKQrxOLSpJAiCdqCQpAnCecUDdqsYo%2BHxinaa2ylwEwqkANYUAA6p0rxaTpVl6QZEDGSZeAvvAHGBOgdGZrGmYUXAVE0YoOGfgg9YHlqCnaHRALcf5JGfsOxZjku8AOpisoRWOlyWVZAkAGQ0AF6jjplWUdrl7nqMlZTYCoOxwCAJCiiIzCyJAkjwBABg9nAAgQD4mwgKgdWirGcYAPJOOEg2Qto5DCO4cAACSclsngkG0AA8G2HFYejQCEZDUG0jLMsgJAEIZe1QAdkqzniLYEMtq0ttoehgG0161gAdKMp2Gb1EANFMVgAEoAOJrHAACsABskMw9DjiLe%2BADCD1tAAnNWtYY9WfELJYT2bi8H7qDDVDMA0hm46wwiGa2cSlgALBDWOY04GBgMANq6o4xqsxDfE7X2%2B1tLKADaZV2E4eNOBzNrAPYjjGJza3YKLDO3AzH0Qx9H3vQAunrfEnWdF0HWLEt-NLViy0a8uK3AyvYOQni9gOasa1rOtowbqBAA",
            image: fractalTree,
            name: "Animated Fractal Tree",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCrAQwMYkwOzgC4i4AxAGwk3gFoA%2BOAZQAtMwBTDHPQ%2BAXjhZcBAORx49KRKgEAzgDNoIVHDgB3Fhyhc1amLPyLlcQQfkUaHOAAoAah2wxocAKwAGd288BKOADo4BWAKCjgATQ5QiHUAuHwIJgMIAGtdPWFecS9BbGAobAprABZPVQyeMQlTIWASzzhSrxt4ACpauAB6OAAOPxtaNo7uvvK1VDxgQhI4AEkAeVsfCcwpmp18ABNtOAASOGwCADdMOTgAHkvSAG1MgmqAH2rz2jhr9wAad38ARn9fgC6cAA1MC3t8AJxffw9f7uAEA8qabTWQ74E5nQQKMBnABszWQOBSAHMoBAAK5bOAAIUs2BStmwOisAGFjqc4D9PF4ub4fEA",
            image: pacman,
            name: "Pacman",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoK4DtgwMLBQDGANgKZwBclcAYiRAIbwC0AfHAMoAWjYZmHPkKkKjOHAC8qCbLnyWLOABEojAO5xxREeTjrc3LVjgAPFo1PAAzgBotcAJ4Wrd4wBMHUCDGbAsAOZwau7AGNYy8lESABQ6xHoAjAAMyXBsGXAwaljWJMwUMQBqZEQw0HAATACsaYm1AJSR0dEAZHAk-hSVqRIZHNmMufkwhSVlFTVpjc0tcy39wT4FcDFgwHAA9FVN81HtnVjdvemZg8MrxaXlUHD1dTN78gdddyeL53mX4zdVtXePJ5AvqZby%2BUZaWbzBqnDjWbIQADWZAA6mRgAFuPAAMxQuaKFRqTTiSD%2BeAAMwqMG4FCIEGsrwAbowSBgBMD2nFdBRErC4PDvMi4AB5HIBdnAvYfHJfCFXCa3GJTOAAajeaQAVHA6dYtDD7rsgQTVBoHKSsBSqTT%2BUyWWy8S12vFRHcQXCEUKAEpkdwOyVyaVDWVja6Tf4xe6q9VwLUMkyMBqGp7GolmiBkuCU27U2kQZD%2BPwQEwQck2o4ebX022ssgRDna7muvkCpEUFHcXAS-0LM4ykYhhWrZVqlKayu6hOrSMj3qx-x6pN7GGLFvItEYrFvdABNRgIzUOgMZhwdhcXj8VA7vhGcTSFop01DMwuGz2AzUhyMsiwYBEFkdV5j1MKQ4GSewhk8IYfBpW4gJAyoYzgdY-RiQ4KFqNJAwuUYAE04GxWo-QkTk0OjN0sj7b5Q1uf5sVSRdHQA8tR3Iz5%2B1WH4KgAFlo%2BiiNkZdQWWOV1i2HZ%2BME91BVRdFMRxP1OX4iQH2JeMwDAbwrBAQti1LbQq3LIgMCgL84HCfwgmQMgAC9gG-bVjK-Os9hiKzbO-Xk%2BTYqjBxiFgDTgLi%2BKedo3LsqBMN7IN2PlX5eOSBi2jgMKPK8yi5U425lSChKlLgUKbPCyKBnSgdfh4tI6Nyp5JP5D0KFFIZxX4lTjC0dTNNAHS4BLBw41pRyKHMwJksK%2ByjJM2t%2BM5FKoE8rDgw46jVhYRpAuCvYCvciK0uinzfkjKrEv2Ubtt5BaYsy-D-hy46BObeq4G9X0Wlq1cZI3eBKj9VqSXTC1MwgEgGAMEaczLAbJoUhsEh5cj3vXOSt27WQV0e568p7Eq9oy5aYn%2BJQejHcRtnWGEYgItI1X%2BOd40TH6lBNYkkP%2By1gYgUGgnBnVXgmr9oedJJ4fqxHNxSTHojR6SRTFLtuwu-aKnxtJCdnBxSeAcnKZPOAafHBdInUGC5dmtJJFO8LsSWwcAHZKgAOgADlA8mrsSSobfthDR1d5aifVRdZt5c3Zqt2KKjtp2Xetg6Pa9lafd9wd-b8xP0G0%2BcDwASWFVYmgzkxzagMgsHceyABJtSGZldQAHnrmgAG1sFwAhYYcdorz3BwAB8HFrpRG7Ak9knt3l7ftk8EK1dYAF1Z8N43Zj-LAa5A5BGCIREdwgbBPCQOSAHFi8cVYiGLgo8GrxhdQw3W%2BKAA",
            image: sineCosine,
            name: "Sine and Cosine",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoGZQIYgKYBK2MecAXGXAJIB2MmO%2BRJcAvHAEwAs6AzgMbYANngBi2fjGjlKooRGKoBwsRKlQ2cAKwAGOAFp9cAIxx8JDe11wwwAB54hvdAHMcAN2AwAnjLgA1PEloVDdsTx9NQOCNPQAKAE4AOgAOUwAqOGURcRiASnR%2BCDwMDAI-OQV6IpKyzWN0ABNibDgABWxYYH4RTQ6untIAb1Q4GwheL2AIGj9o9QAaUbh3Rwh%2BL18KAKDF5Yl%2BRzwcGGnZ7fnoJbGcRuAAV14-BDwaFxgAC2u4Ivl7jW2AGEIPIoKgAL5wRrHYCeN5wOIAZQ%2BEAA7gVUGBOqdBnNdtJ9AA%2BHYxAzEl5vT5kuDA0HU-o4kSY7HdXqQJ6rIRwDT8Pos3HslaOOBhCK%2BOIaTLZVT5H7oe5gZosbaVYjUgDaDNZeAAuhqtYMdagFUrSPAwE92CBsGAESbiHhEcA3r0YHkbM5jYqHU6XaQVfI1UT2vzesGDUz7SRfS5XTZNHEo3g2hM4Ek4EnAkJ3WBlqiPsc8MsxpnhXEI6RBZy4AduT8CmNG3AAD5wDDyaB4RrjJ6WEMDXpxZAQe40Q5tHvc91DkdjvD%2BIXZmv8Xk84uN1uHYBCZ0uLuTvsVhHD0eHQF4be7ieCqDTk9z8%2BXt4Lznu2trptjVuo4Qibs3sZ2CPGdTzwAB1X9r1TW9j1nQ4IKEIQX0cN8Vzrfh1y-OAIE%2BY5UWAXhSCbIDQ0rVM4mrAA9ABqSiEVrSj0jot1UNXOUm3XJMUyectSMnat33rPkB1IOJBRouiKOFRjmLyd0BLQj9G3XdsIE7bs4kuDQ7Dgbx3T7XxqLrQkrB0HR13vccES0uAdL0ut2BsnS4l0PQjFvCy4Pnaz8W03T3Uc3zbIRXxMjifQalKAg5PXdctx3N5900oK7P0zRfHcuAAB52HMptLLwR8EpcCdktJVKHJJdRgrBfKvKK3cFzK6qKsC8qQrgMKIuKKKYo4psf0QpKnP8yqdMynK4D0ZtWx0oyNBM7QzM80CEKEUqRvs2rP1mgw60mvQoiCjRvEwltgvmuBFtco7SRcsy9pg07P3OnCCygfDCOIqrpDsla5zWprNoCn6NDiHSusiso5N09APmwGhGhEWkd0mGYnm2TVSL1YMsZEo14cR5GQVRs4nnVPV2Ap1BCaRwqSYIsmETAMgLRBqA8F4EFVhRxn0fjC04GpsZ80LdcOa5oQeYZtGaCeW0KeXXl2FtSgDnFznufpxC%2Bbl%2BMxI4VneFYs7NwZu54VtMAOE0ER4DE4wAHIFhsDhHfZzWpe10mZgFjgzpewOXudblPelnXZflp2PWZt3yCVhsduw3CPoIojvolrXecjgWeOt%2BODkTsY4vN3cEWApzTG8Yx3QAfTgeuoFMWvpwrlKbe8Dg64b7kbZbtgzrtqECJgREAEcEzsG2jDsGvOsyG2jLiTu9ur910gXgPG1uB5eERe4QE0Ju4Hm-3g9mO5eFHiesu5bA7keffD43zhYqbTOvezs5y749lTE5UwBxTDH34HPXiIkew205DbA4NsoA234F3AeL0h40GgNaLkKsJg20ovoOif8t5jEvvAK02AXBwDQVADBhCMw0C8AAOXQcIAAAs1aQNAdI0HsuwShGC4CUQAPR0WITQ0cXgAAqCM9x0FutVcKXD3QcJoe4Ywsx2CNBwouUwYiYCMKocIZRxgSFQk0QA2hEipGvHoEHMY7gOBqJMfAaB5jdFMKEMojgxiNFOMcDbHRkjEp0EMTQR2mg7E0A8SEsJqjDH0LwKiBMOi9F8JkisVR7t%2BG0TtHQmAATpHwFSSot0Hi4kJPYImHJyThD8KYisexGSJLZIsYEgptS7HFPPj-CBf80mlKVsA0woCXZHnZFAjgfTYG9x%2BF3PgMAHQY0oOqPGjJdRGivvMzQXhjgOgRLTYmEcmbpiTAiUwAi2xQAgCAWgJAwhciwLgQgDoYZYhEp6RoOAEnbCPMGZENoizvOwAk207ANhQFxBKe%2Bu8bDukJLCts24uRxF%2BCODQYAYVwrQYiGAFyADWpBYXEmxQjXgQgdliQmFMX2aL0AvJWQsoWFYjS0u1JaIWNg%2BJsI0MYawxgzLThsryqa7pTAEC7DQsYLtmXgpsgAZmsBwXQ-KgrhV5e6HQwq4AACEhD3CLDYxsRpUAAtRAAQUQn4ZZ2ocbEl%2BWAf5HyzWYLbCCd5CIABk7o8DgEiOma0tpjXoGtCHbYVAADyCIChBocRo5YHNEbHDgAAEh%2BAjdw2AnhZUzZQOZeK4AAHZrBxD9VCB15r1kkGNuuUWHMzqCBoGm1lGBBb3KYGS5AEgcVuFnN2JALgPgwAAOIc3FPwDmDpASpvTUtPQrk5JAA",
            image: balls,
            name: "Bouncing Balls",
        },
        {
            code: "JYWwDg9gTgLgBAGWAIwFCoMYBsIYNZwBchcAkgHbwC0AfHAMoAWAhmAKaY75wDOcAvHGQQAJgE84AMjgAzZhjZS4zKCB6o4cAO6M2UDps3DxAuAAoMwKNkUAmAAz24NF3BhRm5HlmYxFZgDU2DBhoOFsAVidI%2BwBKZ1cZYCwsOABBSmAARwBXNgB1RmA-OA1Dcs1pfRC4ADZopxc6d09vX38gkLDbBzgAZjiEuiSUuAAhH3x4pt53CDwCtmAAc0Z4AEZ7Ms05BVMzGQgsEShzSXi2cBgJAG0QFQJgcIi4AB84J4AeKjgbpwA6f5wCIATgAumDthVKrIjiczhcrrd7lBHsCnO8vj8-gAaYFwQHA8FgoazKDzRYrNbA6auFpeHwlQLBUKnGLPOJQzQ6PQGaFwFEEZhwVKCLDAciKVIzeltJmdVnmDAQPhmTzLLCKABUyli8TMPAl5nVmrgOuYergAD0tVbzA4nD8sHqufy3dCZuSYO1jeQNdrda7NCbFIJbGa4GAngB6OpbcoqNT7cWSuDrAAcjTpHgZPuZXTZUQ5sSD7oqnog3qZVCjcFjthLZabFar-nDOpk5JAFD8yw8qT4Otrsfqjab0ks1lNm1JssZHRZ3SLMUtMx4cwW%2BSWq3gLzXG8UACU2CJSSNUgBxOHoKg-eje2DaKDFCXLOBiCA5U7KkSKXT6f5UHuI1iDIAB5cwS2A8hTH0chf1OAASWQwD4dZzGQeQ8D7T94MQKkYAvfQJAsfR2gAYU8AA3Zg%2BCiJx6MtT5mJIG5sFwNFMTgb5fnsQF6ghVAgA",
            image: stopwatch,
            name: "Stopwatch",
        },
    ];
    /* eslint-enable max-len */

    return (
        <>
            <Stack direction={{ md: "row" }} gap={2} className={styles.title!}>
                <Image src="/favicon.svg" alt="Haskell Logo" width={150} height={100} />
                <div>
                    <Typography variant="h2" component="h2"><b>A Graphical Playground</b></Typography>
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
                        the <Link href="/issues">GitHub issue tracker</Link>.
                </Typography>
            </div>
        </>
    );
}
