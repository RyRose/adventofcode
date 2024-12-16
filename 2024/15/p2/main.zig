const std = @import("std");

const Direction = enum(u8) {
    Up = '^',
    Right = '>',
    Down = 'v',
    Left = '<',

    fn rotateRight(self: Direction) Direction {
        switch (self) {
            Direction.Up => return Direction.Right,
            Direction.Right => return Direction.Down,
            Direction.Down => return Direction.Left,
            Direction.Left => return Direction.Up,
        }
    }
};

const Point = struct {
    x: isize,
    y: isize,

    fn in_bounds(self: Point, grid: [][]u8) bool {
        return self.x >= 0 and self.y >= 0 and self.y < grid.len and self.x < grid[@intCast(self.y)].len;
    }

    fn lookup(self: Point, grid: [][]u8) ?u8 {
        if (!self.in_bounds(grid)) {
            return null;
        }
        return grid[@intCast(self.y)][@intCast(self.x)];
    }

    fn move(self: *Point, direction: Direction) void {
        switch (direction) {
            Direction.Down => self.*.y += 1,
            Direction.Up => self.*.y -= 1,
            Direction.Left => self.*.x -= 1,
            Direction.Right => self.*.x += 1,
        }
    }

    fn equals(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }

    fn set(self: Point, grid: [][]u8, value: u8) void {
        grid[@intCast(self.y)][@intCast(self.x)] = value;
    }
};

fn printGrid(grid: [][]u8) void {
    for (grid) |row| {
        for (row) |char| {
            std.debug.print("{c}", .{char});
        }
        std.debug.print("\n", .{});
    }
}

const Grid = struct {
    grid: [][]u8,

    fn move(self: *Grid, direction: Direction) !void {
        const r = self.robot().?;
        if (direction == Direction.Left or direction == Direction.Right) {
            _ = self.try_move_h(direction, r);
            return;
        }
        _ = try self.try_move_v(direction, r);
    }

    fn try_move_h(self: *Grid, direction: Direction, point: Point) bool {
        if (point.lookup(self.grid)) |v| {
            if (v == '#') {
                return false;
            }
            if (v == '.') {
                return true;
            }
            var next = point;
            next.move(direction);
            if (!self.try_move_h(direction, next)) {
                return false;
            }
            next.set(self.grid, v);
            point.set(self.grid, '.');
            return true;
        }
        return false;
    }

    fn try_move_v(self: *Grid, direction: Direction, point: Point) !void {
        if (point.lookup(self.grid)) |v| {
            try std.testing.expect(v == '@');

            var next = point;
            next.move(direction);
            if (self.probe_move_v(direction, next)) {
                try self.move_v(direction, point);
            }
            return;
        }
        unreachable;
    }

    fn probe_move_v(self: *Grid, direction: Direction, point: Point) bool {
        if (point.lookup(self.grid)) |v| {
            if (v == '#') {
                return false;
            }
            if (v == '.') {
                return true;
            }
            var next = point;
            next.move(direction);
            var next2 = next;
            if (v == '[') {
                next2.move(Direction.Right);
            } else if (v == ']') {
                next2.move(Direction.Left);
            } else {
                std.debug.print("v: {c}\n", .{v});
                unreachable;
            }
            if (!self.probe_move_v(direction, next)) {
                return false;
            }
            if (!self.probe_move_v(direction, next2)) {
                return false;
            }
            return true;
        }
        return false;
    }

    fn move_v(self: *Grid, direction: Direction, point: Point) !void {
        if (point.lookup(self.grid)) |v| {
            try std.testing.expect(v != '#');
            if (v == '.') {
                return;
            }

            var next = point;
            next.move(direction);

            var twin = point;
            if (v == '[') {
                twin.move(Direction.Right);
            } else if (v == ']') {
                twin.move(Direction.Left);
            } else if (v == '@') {
                try self.move_v(direction, next);
                next.set(self.grid, v);
                point.set(self.grid, '.');
                return;
            } else {
                unreachable;
            }

            try self.move_v(direction, next);

            var next2 = twin;
            next2.move(direction);
            try self.move_v(direction, next2);

            next.set(self.grid, v);
            point.set(self.grid, '.');

            next2.set(self.grid, twin.lookup(self.grid).?);
            twin.set(self.grid, '.');
            return;
        }
        unreachable;
    }

    fn robot(self: *Grid) ?Point {
        for (self.grid, 0..) |row, y| {
            for (row, 0..) |char, x| {
                if (char == '@') {
                    return Point{ .x = @intCast(x), .y = @intCast(y) };
                }
            }
        }
        return null;
    }

    fn gps(self: *Grid) usize {
        var sum: usize = 0;
        for (self.grid, 0..) |row, y| {
            for (row, 0..) |char, x| {
                if (char == '[') {
                    sum += y * 100 + x;
                }
            }
        }
        return sum;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test.in", 1_000_000);
    defer allocator.free(content);

    var grid = std.ArrayList([]u8).init(allocator);
    defer grid.deinit();
    var moves = std.ArrayList(Direction).init(allocator);
    defer moves.deinit();

    {
        var lit = std.mem.splitScalar(u8, content, '\n');
        while (lit.next()) |line| {
            if (line.len == 0) {
                break;
            }
            var l = std.ArrayList(u8).init(allocator);

            // If the tile is #, the new map contains ## instead.
            // If the tile is O, the new map contains [] instead.
            // If the tile is ., the new map contains .. instead.
            // If the tile is @, the new map contains @. instead.
            for (line) |char| {
                if (char == '#') {
                    try l.append('#');
                    try l.append('#');
                } else if (char == 'O') {
                    try l.append('[');
                    try l.append(']');
                } else if (char == '.') {
                    try l.append('.');
                    try l.append('.');
                } else if (char == '@') {
                    try l.append('@');
                    try l.append('.');
                } else {
                    unreachable;
                }
            }

            try grid.append(l.items);
        }
        for (content) |char| {
            try moves.append(std.meta.intToEnum(Direction, char) catch {
                continue;
            });
        }
    }

    var g = Grid{ .grid = grid.items };
    for (moves.items) |direction| {
        try g.move(direction);
    }
    printGrid(grid.items);
    std.debug.print("GPS: {d}\n", .{g.gps()});
}
