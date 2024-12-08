const std = @import("std");

fn isXmas(arr: [4]u8) bool {
    return std.mem.eql(u8, &arr, "XMAS") or std.mem.eql(u8, &arr, "SAMX");
}

fn validRight(arr: [][]const u8, x: usize, y: usize) bool {
    return x <= arr[y].len - 4;
}

fn validDiagonalRight(arr: [][]const u8, x: usize, y: usize) bool {
    return x <= arr[y].len - 4 and y <= arr.len - 4;
}

fn validDiagonalLeft(arr: [][]const u8, x: usize, y: usize) bool {
    return x >= 3 and y <= arr.len - 4;
}

fn validDown(arr: [][]const u8, y: usize) bool {
    return y <= arr.len - 4;
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
            if (validRight(list.items, x, y)) {
                const arr = [4]u8{ v, line[x + 1], line[x + 2], line[x + 3] };
                if (isXmas(arr)) {
                    sum += 1;
                }
            }
            if (validDown(list.items, y)) {
                const arr = [4]u8{ v, list.items[y + 1][x], list.items[y + 2][x], list.items[y + 3][x] };
                if (isXmas(arr)) {
                    sum += 1;
                }
            }
            if (validDiagonalLeft(list.items, x, y)) {
                const arr = [4]u8{ v, list.items[y + 1][x - 1], list.items[y + 2][x - 2], list.items[y + 3][x - 3] };
                if (isXmas(arr)) {
                    sum += 1;
                }
            }
            if (validDiagonalRight(list.items, x, y)) {
                const arr = [4]u8{ v, list.items[y + 1][x + 1], list.items[y + 2][x + 2], list.items[y + 3][x + 3] };
                if (isXmas(arr)) {
                    sum += 1;
                }
            }
        }
    }
    std.debug.print("{d}\n", .{sum});
}
