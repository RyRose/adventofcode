const std = @import("std");

fn determine_first_large_enough_empty_region(arr: []isize, size: usize) ?[]isize {
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

const Block = struct {
    value: i64,
    len: usize,
};

fn determine_first_empty(blocks: []Block, len: usize) ?usize {
    for (blocks, 0..) |block, i| {
        if (block.value != -1) {
            continue;
        }
        if (block.len >= len) {
            return i;
        }
    }
    return null;
}

fn insert(blocks: *std.ArrayList(Block), from: usize, to: usize) !void {
    const block = blocks.items[from];
    blocks.items[from].value = -1;
    try blocks.insert(to, block);
    try std.testing.expect(blocks.items[to + 1].len >= block.len);
    try std.testing.expect(blocks.items[to + 1].value == -1);
    blocks.items[to + 1].len -= block.len;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var blocks = std.ArrayList(Block).init(allocator);
    defer blocks.deinit();

    for (content, 0..) |char, i| {
        if (char == '\n') {
            continue;
        }
        const val = char - '0';
        if (i % 2 == 1) {
            try blocks.append(Block{ .value = -1, .len = val });
            continue;
        }
        const half = try std.math.divExact(i64, @intCast(i), 2);
        try blocks.append(Block{ .value = half, .len = val });
    }

    for (blocks.items, 0..) |_, ri| {
        const i: usize = blocks.items.len - ri - 1;
        const val = blocks.items[i];
        if (val.value == -1) {
            continue;
        }

        if (determine_first_empty(blocks.items[0..i], val.len)) |first| {
            try insert(&blocks, i, first);
        }
    }

    var index: usize = 0;
    var result: u64 = 0;
    for (blocks.items) |block| {
        for (0..block.len) |_| {
            defer index += 1;
            if (block.value == -1) {
                std.debug.print(".", .{});
                continue;
            }
            std.debug.print("{d}", .{block.value});
            const b: usize = @intCast(block.value);
            result += b * index;
        }
    }

    std.debug.print("\n{d}\n", .{result});
}
