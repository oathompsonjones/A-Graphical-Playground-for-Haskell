"use server";

import type { Sketch, SketchWithoutId, User, UserWithoutId } from "schemas/database";
import type { Collection } from "mongodb";
import { ObjectId } from "mongodb";
import client from "./connection";

/**
 * Gets the user with the specified email.
 * @param email - The email of the user to get.
 * @returns A JSON string representing the user with the given email.
 */
export async function getUserFromEmail(email: string): Promise<string> {
    const db = (await client).db("database");
    const users: Collection<User> = db.collection("users");

    const user = await users.findOne({ email });

    if (user === null)
        throw new Error("User not found.");

    return JSON.stringify(user);
}

/**
 * Gets the user with the specified email.
 * @param id - The id of the user to get.
 * @returns A JSON string representing the user with the given id.
 */
export async function getUserFromId(id: string): Promise<string> {
    const db = (await client).db("database");
    const users: Collection<User> = db.collection("users");

    const user = await users.findOne({ _id: new ObjectId(id) });

    if (user === null)
        throw new Error("User not found.");

    return JSON.stringify(user);
}

/**
 * Creates a user.
 * @param email - The email of the user to create.
 * @param passwordHash - The password of the user to create.
 * @returns A JSON string representing the created user.
 */
export async function createUser(email: string, passwordHash: string): Promise<string> {
    const db = (await client).db("database");
    const users: Collection<UserWithoutId> = db.collection("users");
    let user = await users.findOne({ email });

    if (user !== null)
        throw new Error("A user already exists with that email.");

    const { acknowledged, insertedId } = await users.insertOne({ avatar: null, email, passwordHash, username: null });

    if (!acknowledged)
        throw new Error("Failed to register user.");

    user = await users.findOne({ _id: insertedId });

    if (user === null)
        throw new Error("Failed to register user.");

    return JSON.stringify(user);
}

/**
 * Updates the user with the specified id.
 * @param id - The id of the user to update.
 * @param user - The new user data.
 * @returns A JSON string representing the updated user.
 */
export async function updateUser(id: string, user: Partial<User>): Promise<string> {
    const db = (await client).db("database");
    const users: Collection<User> = db.collection("users");
    const result = await users.findOneAndUpdate({ _id: new ObjectId(id) }, { $set: user });

    if (result === null)
        throw new Error("User not found.");

    return JSON.stringify(result);
}

/**
 * Deletes the user with the specified id.
 * @param id - The id of the user to delete.
 */
export async function deleteUser(id: string): Promise<void> {
    const db = (await client).db("database");
    const users: Collection<User> = db.collection("users");
    const result = await users.deleteOne({ _id: new ObjectId(id) });

    if (result.deletedCount === 0)
        throw new Error("User not found.");
}

/**
 * Gets the sketch with the specified name for the specified user.
 * @param authorId - The email of the user who owns the sketch.
 * @param name - The name of the sketch to get.
 * @returns A JSON string representing the sketch with the given name and author.
 */
export async function getSketch(authorId: string, name: string): Promise<string> {
    const db = (await client).db("database");
    const sketches: Collection<Sketch> = db.collection("sketches");
    const sketch = await sketches.findOne({ author: authorId, name });

    if (sketch === null)
        throw new Error("Sketch not found.");

    return JSON.stringify(sketch);
}

/**
 * Gets all sketches for the specified user.
 * @param authorId - The id of the user who owns the sketches.
 * @returns A JSON string representing all sketches for the given user.
 */
export async function getSketches(authorId: string): Promise<string> {
    const db = (await client).db("database");
    const sketches: Collection<Sketch> = db.collection("sketches");
    const sketchArray = await sketches.find({ authorId }).toArray();

    return JSON.stringify(sketchArray);
}

/**
 * Creates a sketch.
 * @param authorId - The id of the user who owns the sketch.
 * @param name - The id of the sketch to create.
 * @param content - The content of the sketch to create.
 * @returns A JSON string representing the created sketch.
 */
export async function createSketch(authorId: string, name: string, content: string): Promise<string> {
    console.log("createSketch", authorId, name);

    const db = (await client).db("database");
    const sketches: Collection<SketchWithoutId> = db.collection("sketches");
    let sketch = await sketches.findOne({ authorId, name });

    if (sketch !== null)
        throw new Error("A sketch already exists with that name.");

    const timestamp = Date.now().toString();

    const { acknowledged, insertedId } = await sketches.insertOne({
        authorId,
        content,
        createdAt: timestamp,
        modifiedAt: timestamp,
        name,
        public: false,
    });

    if (!acknowledged)
        throw new Error("Failed to create sketch.");

    sketch = await sketches.findOne({ _id: insertedId });

    if (sketch === null)
        throw new Error("Failed to create sketch.");

    return JSON.stringify(sketch);
}

/**
 * Updates the sketch with the specified name for the specified user.
 * @param authorId - The id of the user who owns the sketch.
 * @param name - The name of the sketch to update.
 * @param sketch - The new sketch data.
 * @returns A JSON string representing the updated sketch.
 */
export async function updateSketch(authorId: string, name: string, sketch: Partial<Sketch>): Promise<string> {
    const db = (await client).db("database");
    const sketches: Collection<Sketch> = db.collection("sketches");
    const result = await sketches.findOneAndUpdate({ author: authorId, name }, { $set: sketch });

    if (result === null)
        throw new Error("Sketch not found.");

    return JSON.stringify(result);
}

/**
 * Deletes the sketch with the specified name for the specified user.
 * @param id - The id of the sketch to delete.
 */
export async function deleteSketch(id: string): Promise<void> {
    console.log("deleteSketch", id);

    const db = (await client).db("database");
    const sketches: Collection<Sketch> = db.collection("sketches");

    await sketches.deleteOne({ _id: new ObjectId(id) });
}
