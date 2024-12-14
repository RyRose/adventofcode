const std = @import("std");

const Point = struct {
    x: isize,
    y: isize,
};

fn in_bounds(grid: [][]const u8, x: isize, y: isize) bool {
    return x >= 0 and y >= 0 and y < grid.len and x < grid[@intCast(y)].len;
}

const Searcher = struct {
    grid: [][]const u8,
    value: u8,
    global: *std.AutoHashMap(Point, bool),
    seen: std.AutoHashMap(Point, bool),
    area: u64 = 0,
    perimeter: u64 = 0,

    fn search(self: *Searcher, point: Point) !void {
        const oob = !in_bounds(self.grid, point.x, point.y);
        var equals = false;
        if (!oob) {
            const val = self.grid[@intCast(point.y)][@intCast(point.x)];
            equals = val == self.value;
        }

        // Skip if already seen and is equal to value.
        // Also, mark as seen if unseen.
        var seen = false;
        {
            const res = try self.seen.getOrPut(point);
            if (res.found_existing and equals) {
                return;
            }
            seen = res.found_existing;
        }

        if (equals) {
            try self.global.put(point, true);
            self.area += 1;
        } else {
            self.perimeter += 1;
        }

        // Don't continue searching if out of bounds.
        // Or, if it's not equal to self.value.
        if (oob or !equals) {
            return;
        }

        const x = point.x;
        const y = point.y;
        try self.search(Point{ .x = x + 1, .y = y });
        try self.search(Point{ .x = x - 1, .y = y });
        try self.search(Point{ .x = x, .y = y + 1 });
        try self.search(Point{ .x = x, .y = y - 1 });
    }
};

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
                .seen = local,
                .area = 0,
                .perimeter = 0,
            };
            try s.search(p);
            price += s.area * s.perimeter;
        }
    }
    std.debug.print("{}\n", .{price});
}
