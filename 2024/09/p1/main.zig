const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var blocks = std.ArrayList(isize).init(allocator);
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
        const half = try std.math.divExact(isize, @intCast(i), 2);
        for (0..val) |_| {
            try blocks.append(half);
        }
    }

    var lhs: usize = 0;
    var rhs: usize = blocks.items.len - 1;

    while (lhs < rhs) {
        if (blocks.items[rhs] == -1) {
            rhs -= 1;
            continue;
        }
        if (blocks.items[lhs] != -1) {
            lhs += 1;
            continue;
        }
        const tmp = blocks.items[lhs];
        blocks.items[lhs] = blocks.items[rhs];
        blocks.items[rhs] = tmp;
        lhs += 1;
        rhs -= 1;
    }

    var result: u64 = 0;
    for (blocks.items, 0..) |block, index| {
        if (block == -1) {
            break;
        }
        const b: usize = @intCast(block);
        result += b * index;
    }

    std.debug.print("{d}\n", .{result});
}
