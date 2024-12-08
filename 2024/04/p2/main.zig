const std = @import("std");

fn isXmas(arr: [3]u8) bool {
    return std.mem.eql(u8, &arr, "MAS") or std.mem.eql(u8, &arr, "SAM");
}

fn validTopLeft(arr: [][]const u8, x: usize, y: usize) bool {
    return x + 2 < arr[y].len;
}

fn validDiagonalRight(arr: [][]const u8, x: usize, y: usize) bool {
    return x <= arr[y].len - 3 and y <= arr.len - 3;
}

fn validDiagonalLeft(arr: [][]const u8, x: usize, y: usize) bool {
    return x >= 2 and y <= arr.len - 3;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var list = std.ArrayList([]const u8).init(allocator);
    defer list.deinit();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 100000);
    defer allocator.free(content);

    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        try list.append(line);
    }

    var sum: usize = 0;
    for (list.items, 0..) |line, y| {
        for (line, 0..) |v, x| {
            if (!validTopLeft(list.items, x, y)) {
                break;
            }
            if (!validDiagonalLeft(list.items, x + 2, y)) {
                continue;
            }
            if (!isXmas([3]u8{ list.items[y][x + 2], list.items[y + 1][x + 1], list.items[y + 2][x] })) {
                continue;
            }
            if (!validDiagonalRight(list.items, x, y)) {
                continue;
            }
            if (!isXmas([3]u8{ v, list.items[y + 1][x + 1], list.items[y + 2][x + 2] })) {
                continue;
            }
            sum += 1;
        }
    }
    std.debug.print("{d}\n", .{sum});
}
