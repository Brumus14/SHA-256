const std = @import("std");

pub fn main() !void {
    const file_bytes = try readFile("src/main.zig");
    const hash = try sha256(file_bytes);

    const writer = std.io.getStdOut().writer();

    for (hash) |word| {
        try writer.print("{x}", .{word});
    }

    try writer.print("\n", .{});
}

fn sha256(message: []const u8) ![8]u32 {
    const allocator = std.heap.page_allocator;

    const l: u64 = message.len * 8;

    var n: u64 = l + 1 + 64;

    if (n % 512 != 0) {
        n += 512 - (n % 512);
    }

    const N = n / 512;

    var MB = try allocator.alloc(u8, n / 8);
    defer allocator.free(MB);

    @memcpy(MB[0..message.len], message[0..]);

    MB[message.len] = 0b10000000;

    for ((message.len + 1)..(n / 8)) |i| {
        MB[i] = 0;
    }

    MB[n / 8 - 8] = @truncate(l >> 56);
    MB[n / 8 - 7] = @truncate((l << 8) >> 56);
    MB[n / 8 - 6] = @truncate((l << 16) >> 56);
    MB[n / 8 - 5] = @truncate((l << 24) >> 56);
    MB[n / 8 - 4] = @truncate((l << 32) >> 56);
    MB[n / 8 - 3] = @truncate((l << 40) >> 56);
    MB[n / 8 - 2] = @truncate((l << 48) >> 56);
    MB[n / 8 - 1] = @truncate((l << 56) >> 56);

    var M = try allocator.alloc([16]u32, N);

    for (0..N) |i| {
        for (0..16) |j| {
            M[i][j] = @as(u32, MB[i * 64 + j * 4]) << 24;
            M[i][j] |= @as(u32, MB[i * 64 + j * 4 + 1]) << 16;
            M[i][j] |= @as(u32, MB[i * 64 + j * 4 + 2]) << 8;
            M[i][j] |= @as(u32, MB[i * 64 + j * 4 + 3]);
        }
    }

    const K = [64]u32{ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 };

    var H = [8]u32{ 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 };

    var W: [64]u32 = undefined;
    var T: [2]u32 = undefined;
    var a: u32 = undefined;
    var b: u32 = undefined;
    var c: u32 = undefined;
    var d: u32 = undefined;
    var e: u32 = undefined;
    var f: u32 = undefined;
    var g: u32 = undefined;
    var h: u32 = undefined;

    for (0..N) |i| {
        @memcpy(W[0..16], M[i][0..]);

        for (16..64) |t| {
            W[t] = LSigma1(W[t - 2]) +% W[t - 7] +% LSigma0(W[t - 15]) +% W[t - 16];
        }

        a = H[0];
        b = H[1];
        c = H[2];
        d = H[3];
        e = H[4];
        f = H[5];
        g = H[6];
        h = H[7];

        for (0..64) |t| {
            T[0] = h +% USigma1(e) +% Ch(e, f, g) +% K[t] +% W[t];
            T[1] = USigma0(a) +% Maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = d +% T[0];
            d = c;
            c = b;
            b = a;
            a = T[0] +% T[1];
        }

        H[0] +%= a;
        H[1] +%= b;
        H[2] +%= c;
        H[3] +%= d;
        H[4] +%= e;
        H[5] +%= f;
        H[6] +%= g;
        H[7] +%= h;
    }

    return H;
}

fn Ch(x: u32, y: u32, z: u32) u32 {
    return (x & y) ^ (~x & z);
}

fn Maj(x: u32, y: u32, z: u32) u32 {
    return (x & y) ^ (x & z) ^ (y & z);
}

fn USigma0(x: u32) u32 {
    return ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22);
}

fn USigma1(x: u32) u32 {
    return ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25);
}

fn LSigma0(x: u32) u32 {
    return ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3);
}

fn LSigma1(x: u32) u32 {
    return ROTR(x, 17) ^ ROTR(x, 19) ^ SHR(x, 10);
}

fn SHR(x: u32, n: u5) u32 {
    return x >> n;
}

fn ROTR(x: u32, n: u5) u32 {
    return (x >> n) | (x << @truncate(32 - @as(u8, n)));
}

pub fn readFile(path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    // read whole file
    const contents: []u8 = try file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));

    return contents;
}
