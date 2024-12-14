const std = @import("std");

pub fn determine_first_large_enough_empty_region(arr: []isize, size: usize) ?[]isize {
    var len: usize = 0;
    for (arr, 0..) |val, i| {
        if (val == -1) {
            len += 1;
        } else {
            len = 0;
        }
        if (len == size) {
            return arr[(i + 1) - len .. i + 1];
        }
    }
    return null;
}

pub fn memset(arr: []isize, val: isize) void {
    for (arr) |*ptr| {
        ptr.* = val;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var blocks = std.ArrayList(i64).init(allocator);
    defer blocks.deinit();

    for (content, 0..) |char, i| {
        if (char == '\n') {
            continue;
        }
        const val = char - '0';
        if (i % 2 == 1) {
            for (0..val) |_| {
                try blocks.append(-1);
            }
            continue;
        }
        const half = try std.math.divExact(i64, @intCast(i), 2);
        for (0..val) |_| {
            try blocks.append(half);
        }
    }

    var regionval: i64 = -1;
    var len: usize = 0;
    for (blocks.items, 0..) |_, ri| {
        const i = blocks.items.len - 1 - ri;
        const val = blocks.items[i];
        if (val == regionval) {
            len += 1;
            continue;
        }

        if (regionval != -1) {
            if (determine_first_large_enough_empty_region(blocks.items[0 .. i + 1], len)) |region| {
                @memset(region, regionval);
                @memset(blocks.items[i + 1 .. i + 1 + len], -1);
            }
        }

        len = 1;
        regionval = val;
    }

    var result: u64 = 0;
    for (blocks.items, 0..) |block, index| {
        if (block == -1) {
            continue;
        }
        const b: usize = @intCast(block);
        result += b * index;
    }

    std.debug.print("{d}\n", .{result});
}
