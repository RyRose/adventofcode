const std = @import("std");

const Point = struct {
    x: isize,
    y: isize,

    fn in_bounds(self: Point, grid: [][]const u8) bool {
        return self.x >= 0 and self.y >= 0 and self.y < grid.len and self.x < grid[@intCast(self.y)].len;
    }

    fn lookup(self: Point, grid: [][]const u8) ?u8 {
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
};

const Searcher = struct {
    grid: [][]const u8,
    value: u8,
    global: *std.AutoHashMap(Point, bool),
    edge: *std.AutoHashMap(Point, bool),
    area: u64 = 0,
    sides: u64 = 0,

    fn search(self: *Searcher, point: Point) !void {
        // std.debug.print("x: {}, y: {}\n", .{ point.x, point.y });

        // Skip if out of bounds.
        if (!point.in_bounds(self.grid)) {
            return;
        }

        // Skip if not equal to self.value.
        const val = self.grid[@intCast(point.y)][@intCast(point.x)];
        if (val != self.value) {
            return;
        }

        // Skip if already seen else mark as seen.
        {
            const res = try self.global.getOrPut(point);
            if (res.found_existing) {
                return;
            }
        }

        self.area += 1;
        self.sides += try search_perimeter(self.grid, self.edge, point);

        const x = point.x;
        const y = point.y;
        try self.search(Point{ .x = x + 1, .y = y });
        try self.search(Point{ .x = x - 1, .y = y });
        try self.search(Point{ .x = x, .y = y + 1 });
        try self.search(Point{ .x = x, .y = y - 1 });
    }
};

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

    fn rotateLeft(self: Direction) Direction {
        switch (self) {
            Direction.Up => return Direction.Left,
            Direction.Right => return Direction.Up,
            Direction.Down => return Direction.Right,
            Direction.Left => return Direction.Down,
        }
    }
};

fn search_perimeter(grid: [][]const u8, seen: *std.AutoHashMap(Point, bool), start: Point) !u64 {
    const start_val = grid[@intCast(start.y)][@intCast(start.x)];

    var cur = start;

    // Move all the way left.
    cur.move(Direction.Left);
    while (cur.lookup(grid)) |c| {
        if (c != start_val) {
            break;
        }
        cur.move(Direction.Left);
    }

    // Move all the way down.
    {
        var rhs = cur;
        rhs.move(Direction.Right);
        while (rhs.lookup(grid)) |c| {
            if (cur.lookup(grid)) |c1| {
                if (c1 == start_val) {
                    break;
                }
            }
            if (c != start_val) {
                break;
            }
            rhs.move(Direction.Down);
            cur.move(Direction.Down);
        }
    }

    // Move up one to start the perimeter search with the value on the rhs.
    cur.move(Direction.Up);

    // We've already searched this edge so skip search.
    if (seen.contains(cur)) {
        return 0;
    }

    var first_iter = true;
    var direction = Direction.Up;
    const initial = cur;
    // std.debug.print("initial: x: {}, y: {}\n", .{ initial.x, initial.y });
    var sides: u64 = 0;
    while (true) {
        // std.debug.print("x: {}, y: {}\n", .{ cur.x, cur.y });
        if (!first_iter and cur.equals(initial) and direction == Direction.Up) {
            break;
        }
        if (first_iter) {
            first_iter = false;
        }

        // Insert edge into seen.
        try seen.put(cur, true);

        sides += 1;

        var rhs = cur;
        rhs.move(direction.rotateRight());
        while (rhs.lookup(grid)) |c| {
            // We are convex and have found a corner.
            if (c != start_val) {
                break;
            }
            // Break if we are concave and have broken the perimeter.
            if (cur.lookup(grid)) |c1| {
                if (c1 == start_val) {
                    break;
                }
            }
            rhs.move(direction);
            cur.move(direction);
        }

        var convex = true;
        if (rhs.lookup(grid)) |c1| {
            convex = c1 != start_val;
        }

        if (convex) {
            direction = direction.rotateRight();
            cur.move(direction);
        } else {
            cur.move(direction.rotateRight().rotateRight());
            direction = direction.rotateLeft();
        }
    }
    return sides;
}

fn print_grid(grid: [][]const u8) void {
    for (grid) |row| {
        for (row) |char| {
            std.debug.print("{c}", .{char});
        }
        std.debug.print("\n", .{});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var grid = std.ArrayList([]const u8).init(allocator);
    defer grid.deinit();
    {
        var it = std.mem.splitScalar(u8, content, '\n');
        while (it.next()) |line| {
            if (line.len == 0) {
                continue;
            }
            try grid.append(line);
        }
    }

    var global = std.AutoHashMap(Point, bool).init(allocator);
    defer global.deinit();

    print_grid(grid.items);

    var price: u64 = 0;
    for (grid.items, 0..) |row, y| {
        for (row, 0..) |cell, x| {
            const p = Point{ .x = @intCast(x), .y = @intCast(y) };
            if (global.contains(p)) {
                continue;
            }

            var local = std.AutoHashMap(Point, bool).init(allocator);
            defer local.deinit();

            var s = Searcher{
                .grid = grid.items,
                .value = cell,
                .global = &global,
                .edge = &local,
            };
            try s.search(p);
            std.debug.print("val: {c}, area: {}, sides: {}\n", .{ cell, s.area, s.sides });
            price += s.area * s.sides;
        }
    }
    std.debug.print("{}\n", .{price});
}
