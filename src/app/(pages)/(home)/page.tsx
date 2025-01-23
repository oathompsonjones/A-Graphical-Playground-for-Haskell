import Image from "next/image";
import Link from "next/link";
import type { ReactNode } from "react";
import type { StaticImageData } from "next/image";
import { Typography } from "@mui/material";
import americanFlag from "assets/images/examples/americanFlag.png";
import styles from "styles/pages/home.module.css";
import unionFlag from "assets/images/examples/unionFlag.png";

const examples: Array<{ name: string; code: string; image: StaticImageData; }> = [
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwASgHEAQnAAMcAMwA2OAEYATAFZUUXnFGSZ62QHY4xuABYFqdhWAwR46ec2bX7rdszZ8RUnAsECBkVHRgBIx0cLTELDAAhsQwLKhJAOZ0TlAJPMBJbAqyAHQAnEqoeQnpEMQJ0aIsAI6wcAAUqrJdcABUal3yANRwSt19o7IAlOhYuIQkZJRkLBQJYHhsnADkunA1JOnkEHCMLHioLLQwdEk8%2BnBZuQdwAGRwxLUIeE-E6ahwHCoun%2BAMCVxuxB4WzgCXuumo8DaVRqdWifVUxVUkzUFgAfPi4EQ6NE7A4yPjcYEYFAIABrMgAFRycTACV0yTgFLgNMSjnasL6GSy01BD2%2BeV%2B90uEGut2hqk5BOpBRufLaADU8AjoHJ2hgTFMRaCPsQvj9Do1wXK9VjFZTlXFVWQNVqYDq2hhVKpNNiJtNUMBdBC7qJHhL0gq3mGDupXu9PuKDpH46bE791CC7HhgaLA3hg9DYaJ4YjkbV6nAAPTmbGWeRconRXR3LnxGn0uBMgqs9nwLk8hJqgUw37CkEA6O-BWiPMFtTjsXm2MzoPWhVch0sJ3tTXaqBwTQADnkRSmC5NZvD07gs%2BtHttG%2BZW8Hzt3bv38j954TS-ut8h0L3tij4qi%2BO6ujqmjdFolj%2Bngo63PcABu2YwMA1D1MmFDQMAABetSJHQyYobA6H1LGbzYVAeEEeRmZAucookWhGFEXCrrmN0Ex2oSwDEoCZI8W2dKMk%2BPbwX2SpPtuLp7lY3RnqKVE0ckmHsQi-TQd0DZ8SS9h8q21IiZ2YlshJPGbjJb46vIxiKaCzFkXQy7chxGiftpBKNq5LYEsJHZdiyZkciBjpgbJ75WHWchGgCyn4apznqfAnTdO5PHec2QlGQFpm9hZ0nhdZH7mHW-qzAAsgkJBwBJUAAJ5wJAJDwBABDkFQTU0ukOQgKgIDVcQ7GQtmcAACQxEkSEJGwAA881gjKwZwPNs03qukIrQt8GZLc9HZoxoIYcQ01sKIyAJNQtI9Uwm0CEI7TULoL4AMJTTNmkeWeQA",
        image: unionFlag,
        name: "Union Jack",
    },
    {
        // eslint-disable-next-line max-len
        code: "JYWwDg9gTgLgBAGWAIwFCoLQbgEQKYBmwAdnnDABZkDGEANhAK5QDOcA7maXgCarJ1GZALxwAEngAecAEQAGOQCYAjABYA7DNRRecUROkzkyZQGZTeLewrAYI8VNkFnLrZmz4ipclTgsKAIZgeGycAOQ6cADmECRR5BBwjCx4qCwwUMDBbKIE9DxQcAAUAGQAlHB44DAAngDKgcFwANrpmU1FwHAAVHCKPcXKCnJwAPRwZmUVAD5wXQA82M1yAHQryooAupuocBxUOrt7fhlZZDV6cDrU8MoAnAqDw2MTphUAfJ9wRHR0V7qfd4nKAQADWZAAKlAAsQWGAAjpiPBAeRobC6AE7MUAGp4G7QOAjGpldDpBE5b75P6lCrIeKtGAIuDSC6zaSLFqrdZyTYAGjgFw5yzWAA4%2BZUAG54YjFEGMYg8ZkVEolSXS2VMBUCmazCA8RVFOVayTK1V6g1GxXEnZ7ax4Q7HOB0y7XeDqABsIyKRSGj3GkwG6g%2BXx%2BfwEQjgKLaYMhaLhCOlMCOezJhRZl1TYUjXwyMJYGKxRVx%2BMKRWkvXdAFY4ABqOCVuQVIoXXqV6t1htTZMnBFZ0SZrNRjIxuBQvPwxHIkPAX5wADqNjs3YHl0gdBqMRlzWLMAJRVMI16RVobBhUToeCmxQPAyKLBIcDPF6v3cdb-fe1mT7IHKKICCVwBDwwB5sU-5gMUHaqE2f4AUUvRqNBnJrHcmxTFMOzuHAACyAQPomUAXJAJDwBABA%2BGQYAglE0IgKg-4PqIiI8PacAACRwNQMISgEbDzPxwJnHxAmpiwRx2g6xxccQPEUsgATUKCNGaoq1i2GQx46JieAAMLcbxEwPCMvqNqgQA",
        image: americanFlag,
        name: "American Flag",
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
                        <Link href={`/editor?code=${code}`}>{name}</Link>
                    </div>
                ))}
            </div>
        </div>
    );
}
