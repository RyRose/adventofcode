const std = @import("std");

const Direction = enum(u8) {
    Up = '^',
    Right = '>',
    Down = 'v',
    Left = '<',
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

    fn move(self: *Grid, direction: Direction) void {
        _ = self.try_move(direction, self.robot().?);
    }

    fn try_move(self: *Grid, direction: Direction, point: Point) bool {
        if (point.lookup(self.grid)) |v| {
            if (v == '#') {
                return false;
            }
            if (v == '.') {
                return true;
            }
            var next = point;
            next.move(direction);
            if (!self.try_move(direction, next)) {
                return false;
            }
            next.set(self.grid, v);
            point.set(self.grid, '.');
            return true;
        }
        return false;
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
                if (char == 'O') {
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
            try grid.append(@constCast(line));
        }
        for (content) |char| {
            try moves.append(std.meta.intToEnum(Direction, char) catch {
                continue;
            });
        }
    }

    var g = Grid{ .grid = grid.items };
    for (moves.items) |direction| {
        g.move(direction);
    }
    printGrid(grid.items);
    std.debug.print("GPS: {d}\n", .{g.gps()});
}
