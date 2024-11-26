import type { NextRequest } from "next/server";
import { NextResponse } from "next/server";
import type { User } from "database/schemas/user";

/**
 * Checks if the request is authenticated.
 * @param req - The incoming request.
 * @returns If the request is authenticated.
 */
async function isValidAuth(req: NextRequest): Promise<boolean> {
    await Promise.resolve(req);

    return true;
}

/**
 * Fetches a user by their ID.
 * @param req - The incoming request.
 * @returns The user from the database.
 */
export async function GET(req: NextRequest): Promise<NextResponse> {
    if (!await isValidAuth(req))
        return new NextResponse("Unauthorized", { status: 401 });

    return NextResponse.json({
        avatar: "https://www.gchq.gov.uk/images/alan_turing.jpg",
        email: "alan.turing@bletchleypark.co.uk",
        passwordHash: "password",
        sketches: [
            {
                content: "main :: IO ()\nmain = print \"Hello, world!\"",
                createdAt: new Date().toISOString(),
                modifiedAt: new Date().toISOString(),
                name: "Hello, world!",
            },
            {
                content: "factorial :: Integer -> Integer\nfactorial 0 = 1\nfactorial n = n * factorial (n - 1)",
                createdAt: new Date().toISOString(),
                modifiedAt: new Date().toISOString(),
                name: "Factorial",
            },
            {
                content: "fibonacci :: Integer -> Integer\nfibonacci 0 = 0\nfibonacci 1 = 1" +
                    "\nfibonacci n = fibonacci (n - 1) + fibonacci (n - 2)",
                createdAt: new Date().toISOString(),
                modifiedAt: new Date().toISOString(),
                name: "Fibonacci",
            },
            {
                content: "data Maybe a = Just a | Nothing\n\ninstance Functor Maybe where" +
                    "\n    fmap f (Just x) = Just (f x)\n    fmap _ Nothing = Nothing\n" +
                    "\ninstance Applicative Maybe where\n    pure = Just\n    Just f <*> m = fmap f m" +
                    "\n    Nothing <*> _ = Nothing\n\ninstance Monad Maybe where\n    Just x >>= f = f x" +
                    "\n    Nothing >>= _ = Nothing",
                createdAt: new Date().toISOString(),
                modifiedAt: new Date().toISOString(),
                name: "Maybe",
            },
        ],
        username: "Alan Turing",
    } satisfies User);
}

/**
 * Adds a new user to the database.
 * @param req - The incoming request.
 * @returns The new user.
 */
export async function POST(req: NextRequest): Promise<NextResponse> {
    if (!await isValidAuth(req))
        return new NextResponse("Unauthorized", { status: 401 });

    return NextResponse.json("Success");
}

/**
 * Deletes a user from the database.
 * @param req - The incoming request.
 * @returns The deleted user.
 */
export async function DELETE(req: NextRequest): Promise<NextResponse> {
    if (!await isValidAuth(req))
        return new NextResponse("Unauthorized", { status: 401 });

    return NextResponse.json("Success");
}

/**
 * Updates a user in the database.
 * @param req - The incoming request.
 * @returns The updated user.
 */
export async function PUT(req: NextRequest): Promise<NextResponse> {
    if (!await isValidAuth(req))
        return new NextResponse("Unauthorized", { status: 401 });

    return NextResponse.json("Success");
}

/**
 * Updates a user in the database.
 * @param req - The incoming request.
 * @returns The updated user.
 */
export async function PATCH(req: NextRequest): Promise<NextResponse> {
    if (!await isValidAuth(req))
        return new NextResponse("Unauthorized", { status: 401 });

    return NextResponse.json("Success");
}
