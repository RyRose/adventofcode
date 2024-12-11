const std = @import("std");

const EMPTY = '.';
const WALL = '#';
const START = '^';

const Direction = enum {
    Up,
    Right,
    Down,
    Left,

    fn rotateRight(self: Direction) Direction {
        switch (self) {
            Direction.Up => return Direction.Right,
            Direction.Right => return Direction.Down,
            Direction.Down => return Direction.Left,
            Direction.Left => return Direction.Up,
        }
    }

    fn index(self: Direction) usize {
        switch (self) {
            Direction.Up => return 0,
            Direction.Right => return 1,
            Direction.Down => return 2,
            Direction.Left => return 3,
        }
    }
};

test "Direction.rotateRight" {
    const tests = [_]struct {
        input: Direction,
        expected: Direction,
    }{
        .{ .input = Direction.Up, .expected = Direction.Right },
        .{ .input = Direction.Right, .expected = Direction.Down },
        .{ .input = Direction.Down, .expected = Direction.Left },
        .{ .input = Direction.Left, .expected = Direction.Up },
    };
    for (tests) |t| {
        const actual = t.input.rotateRight();
        try std.testing.expect(actual == t.expected);
    }
}

test "Direction.index" {
    const tests = [_]struct {
        input: Direction,
        expected: usize,
    }{
        .{ .input = Direction.Up, .expected = 0 },
        .{ .input = Direction.Right, .expected = 1 },
        .{ .input = Direction.Down, .expected = 2 },
        .{ .input = Direction.Left, .expected = 3 },
    };
    for (tests) |t| {
        const actual = t.input.index();
        try std.testing.expect(actual == t.expected);
    }
}

const Point = struct {
    x: isize,
    y: isize,

    fn equals(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }

    fn move(self: *Point, direction: Direction) void {
        switch (direction) {
            Direction.Down => self.*.y += 1,
            Direction.Up => self.*.y -= 1,
            Direction.Left => self.*.x -= 1,
            Direction.Right => self.*.x += 1,
        }
    }

    fn lookup(self: Point, grid: std.ArrayList(std.ArrayList(u8))) u8 {
        return grid.items[@intCast(self.y)].items[@intCast(self.x)];
    }

    fn set(self: Point, grid: std.ArrayList(std.ArrayList(u8)), value: u8) void {
        grid.items[@intCast(self.y)].items[@intCast(self.x)] = value;
    }

    fn inBounds(self: Point, grid: std.ArrayList(std.ArrayList(u8))) bool {
        return self.x >= 0 and self.y >= 0 and self.y < grid.items.len and self.x < grid.items[@intCast(self.y)].items.len;
    }
};

test "Point.equals" {
    const tests = [_]struct {
        input: Point,
        other: Point,
        expected: bool,
    }{
        .{ .input = Point{ .x = 1, .y = 2 }, .other = Point{ .x = 1, .y = 2 }, .expected = true },
        .{ .input = Point{ .x = 1, .y = 2 }, .other = Point{ .x = 2, .y = 2 }, .expected = false },
        .{ .input = Point{ .x = 1, .y = 2 }, .other = Point{ .x = 1, .y = 3 }, .expected = false },
    };
    for (tests) |t| {
        const actual = t.input.equals(t.other);
        try std.testing.expect(actual == t.expected);
    }
}

test "Point.move" {
    const tests = [_]struct {
        input: Point,
        direction: Direction,
        expected: Point,
    }{
        .{ .input = Point{ .x = 1, .y = 2 }, .direction = Direction.Up, .expected = Point{ .x = 1, .y = 1 } },
        .{ .input = Point{ .x = 1, .y = 2 }, .direction = Direction.Right, .expected = Point{ .x = 2, .y = 2 } },
        .{ .input = Point{ .x = 1, .y = 2 }, .direction = Direction.Down, .expected = Point{ .x = 1, .y = 3 } },
        .{ .input = Point{ .x = 0, .y = 2 }, .direction = Direction.Left, .expected = Point{ .x = -1, .y = 2 } },
    };
    for (tests) |t| {
        var actual = t.input;
        actual.move(t.direction);
        try std.testing.expect(actual.equals(t.expected));
    }
}

test "Point.lookup" {
    var grid = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[0].append(1);
    try grid.items[0].append(2);
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[1].append(3);
    try grid.items[1].append(4);
    const tests = [_]struct {
        input: Point,
        expected: u8,
    }{
        .{ .input = Point{ .x = 0, .y = 0 }, .expected = 1 },
        .{ .input = Point{ .x = 1, .y = 0 }, .expected = 2 },
        .{ .input = Point{ .x = 0, .y = 1 }, .expected = 3 },
        .{ .input = Point{ .x = 1, .y = 1 }, .expected = 4 },
    };
    for (tests) |t| {
        const actual = t.input.lookup(grid);
        try std.testing.expect(actual == t.expected);
    }
}

test "Point.set" {
    var grid = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[0].append(1);
    try grid.items[0].append(2);
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[1].append(3);
    try grid.items[1].append(4);
    const tests = [_]struct {
        input: Point,
        value: u8,
        expected: std.ArrayList(std.ArrayList(u8)),
    }{
        .{ .input = Point{ .x = 0, .y = 0 }, .value = 5, .expected = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator) },
        .{ .input = Point{ .x = 1, .y = 0 }, .value = 6, .expected = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator) },
        .{ .input = Point{ .x = 0, .y = 1 }, .value = 7, .expected = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator) },
        .{ .input = Point{ .x = 1, .y = 1 }, .value = 8, .expected = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator) },
    };
    for (tests) |t| {
        t.input.set(grid, t.value);
        try std.testing.expect(grid.items[@intCast(t.input.y)].items[@intCast(t.input.x)] == t.value);
    }
}

test "Point.inBounds" {
    var grid = std.ArrayList(std.ArrayList(u8)).init(std.testing.allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[0].append(1);
    try grid.items[0].append(2);
    try grid.append(std.ArrayList(u8).init(std.testing.allocator));
    try grid.items[1].append(3);
    try grid.items[1].append(4);
    const tests = [_]struct {
        input: Point,
        expected: bool,
    }{
        .{ .input = Point{ .x = 0, .y = 0 }, .expected = true },
        .{ .input = Point{ .x = 1, .y = 0 }, .expected = true },
        .{ .input = Point{ .x = 0, .y = 1 }, .expected = true },
        .{ .input = Point{ .x = 1, .y = 1 }, .expected = true },
        .{ .input = Point{ .x = -1, .y = 0 }, .expected = false },
        .{ .input = Point{ .x = 2, .y = 0 }, .expected = false },
        .{ .input = Point{ .x = 0, .y = -1 }, .expected = false },
        .{ .input = Point{ .x = 0, .y = 2 }, .expected = false },
    };
    for (tests) |t| {
        const actual = t.input.inBounds(grid);
        try std.testing.expect(actual == t.expected);
    }
}

fn searchCycle(grid: std.ArrayList(std.ArrayList(u8)), startPoint: Point, startDirection: Direction) !bool {
    var cur = startPoint;
    var direction = startDirection;

    var seen = std.AutoHashMap(Point, u8).init(grid.allocator);
    defer seen.deinit();

    while (cur.inBounds(grid)) {

        // Project in the current direction and check if we're out of bounds.
        var next = cur;
        next.move(direction);
        if (!next.inBounds(grid)) {
            break;
        }

        // If we've seen this point 10 times, then we've found a cycle.
        // This is overly conservative, but it works with little performance impact.
        const val = try seen.getOrPutValue(cur, 0);
        if (val.value_ptr.* >= 10) {
            return true;
        }
        val.value_ptr.* += 1;

        // If we hit a wall, turn right.
        if (next.lookup(grid) == WALL) {
            direction = direction.rotateRight();
            continue;
        }

        cur = next;
    }
    return false;
}

fn printGrid(grid: std.ArrayList(std.ArrayList(u8))) void {
    for (grid.items) |row| {
        for (row.items) |char| {
            std.debug.print("{c}", .{char});
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };

    const f = "./data/test2.in";
    var start: ?Point = null;
    {
        const content = try std.fs.cwd().readFileAlloc(allocator, f, 1_000_000);
        defer allocator.free(content);
        var y: isize = -1;
        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            y += 1;
            if (line.len == 0) {
                continue;
            }
            var lst = std.ArrayList(u8).init(allocator);
            for (line, 0..) |char, x| {
                if (char != START) {
                    try lst.append(char);
                    continue;
                }
                try std.testing.expect(start == null);
                start = Point{ .x = @intCast(x), .y = y };
                try lst.append(EMPTY);
            }
            try grid.append(lst);
        }
    }
    try std.testing.expect(start != null);

    var cur = start.?;
    var direction = Direction.Up;
    var seen = std.AutoHashMap(Point, bool).init(allocator);
    defer seen.deinit();
    while (cur.inBounds(grid)) {

        // Project in the current direction and check if we're out of bounds.
        var next = cur;
        next.move(direction);
        if (!next.inBounds(grid)) {
            break;
        }

        // If we hit a wall, turn right.
        // Don't try to find a cycle if there's a wall in the way.
        if (next.lookup(grid) == WALL) {
            direction = direction.rotateRight();
            continue;
        }

        // Try to find a cycle if we haven't seen this point before and
        // it's not the starting point. If we find a cycle, mark all the
        // points we've seen along the way as walls.
        if (!seen.contains(next) and !next.equals(start.?)) {
            next.set(grid, WALL);
            if (try searchCycle(grid, cur, direction)) {
                try seen.put(next, true);
            }
            next.set(grid, EMPTY);
        }

        cur = next;
    }
    std.debug.print("{d}\n", .{seen.count()});
}
