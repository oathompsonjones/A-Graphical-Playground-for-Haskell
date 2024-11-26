"use server";

import type { Collection } from "mongodb";
import { MongoClient } from "mongodb";
import type { User } from "./schemas/user";

/**
 * Connects to the database.
 * @returns The MongoDB collection.
 */
async function connect(): Promise<Collection<User>> {
    return (await new MongoClient(process.env.MONGO_DB_URL!).connect())
        .db("database")
        .collection<User>("users");
}

/**
 * Gets the user with the specified email.
 * @param email - The email of the user to get.
 * @returns The user with the specified email.
 */
export async function getUser(email: string): Promise<User> {
    const collection = await connect();
    const user = await collection.findOne({ email });

    if (user === null)
        throw new Error("User not found.");

    return user;
}

/**
 * Creates a user.
 * @param email - The email of the user to create.
 * @param passwordHash - The password of the user to create.
 * @returns The created user.
 */
export async function createUser(email: string, passwordHash: string): Promise<User> {
    const collection = await connect();

    let user = await collection.findOne({ email });

    if (user !== null)
        throw new Error("User already exists.");

    const _id = await collection.insertOne({
        avatar: null,
        email,
        passwordHash,
        sketches: [],
        username: null,
    });

    user = await collection.findOne({ _id });

    if (user === null)
        throw new Error("Failed to register user.");

    return user;
}
