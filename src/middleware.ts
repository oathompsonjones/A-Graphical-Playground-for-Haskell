import type { NextRequest } from "next/server";
import { NextResponse } from "next/server";

/**
 * The middleware function.
 * @param request - The incoming request.
 * @returns The response to send.
 */
export function middleware(request: NextRequest): NextResponse {
    const user = request.cookies.get("user")?.value;

    switch (true) {
        case user === undefined && request.nextUrl.pathname.startsWith("/account"):
            return NextResponse.redirect(new URL("/login", request.nextUrl));
        case user !== undefined && request.nextUrl.pathname.startsWith("/login"):
        case user !== undefined && request.nextUrl.pathname.startsWith("/register"):
            return NextResponse.redirect(new URL("/account", request.nextUrl));
        default:
            return NextResponse.next();
    }
}
