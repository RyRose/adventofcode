const std = @import("std");

const EMPTY = '.';
const WALL = '#';
const PATH = 'X';
const START = '^';

const Direction = enum {
    Up,
    Right,
    Down,
    Left,
};

fn inBounds(x: isize, y: isize, width: usize, height: usize) bool {
    return x >= 0 and y >= 0 and x < width and y < height;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };

    var startX: ?isize = null;
    var startY: ?isize = null;
    {
        var y: isize = 0;
        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            y += 1;
            if (line.len == 0) {
                continue;
            }
            var lst = std.ArrayList(u8).init(allocator);
            for (line, 0..) |char, x| {
                if (char == START) {
                    startX = @intCast(x);
                    startY = y - 1;
                }
                try lst.append(char);
            }
            try grid.append(lst);
        }
    }
    try std.testing.expect(startX != null and startY != null);

    var x: isize = startX.?;
    var y: isize = startY.?;
    var direction = Direction.Up;
    while (inBounds(x, y, grid.items[0].items.len, grid.items.len)) {
        if (grid.items[@intCast(y)].items[@intCast(x)] != WALL) {
            grid.items[@intCast(y)].items[@intCast(x)] = PATH;
            switch (direction) {
                Direction.Up => y -= 1,
                Direction.Right => x += 1,
                Direction.Down => y += 1,
                Direction.Left => x -= 1,
            }
            continue;
        }
        switch (direction) {
            Direction.Up => {
                direction = Direction.Right;
                y += 1;
            },
            Direction.Right => {
                direction = Direction.Down;
                x -= 1;
            },
            Direction.Down => {
                direction = Direction.Left;
                y -= 1;
            },
            Direction.Left => {
                direction = Direction.Up;
                x += 1;
            },
        }
    }

    var sum: usize = 0;
    for (grid.items) |row| {
        for (row.items) |char| {
            sum += @intFromBool(char == PATH);
        }
    }
    std.debug.print("{}\n", .{sum});
}
