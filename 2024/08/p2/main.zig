const std = @import("std");

const AntiIterator = struct {
    p1: Point,
    p2: Point,
    grid: std.ArrayList(std.ArrayList(u8)),
    index: isize = 1,
    reverse: bool = false,

    fn next(self: *AntiIterator) ?Point {
        const xdst = self.p1.x - self.p2.x;
        const ydst = self.p1.y - self.p2.y;
        if (!self.reverse) {
            var p = self.p1;
            p.x += self.index * xdst;
            p.y += self.index * ydst;
            if (p.inBounds(self.grid)) {
                self.index += 1;
                return p;
            }
            self.reverse = true;
            self.index = 1;
        }
        var p = self.p2;
        p.x -= self.index * xdst;
        p.y -= self.index * ydst;
        if (p.inBounds(self.grid)) {
            self.index += 1;
            return p;
        }
        return null;
    }
};

const Point = struct {
    x: isize,
    y: isize,

    fn lookup(self: Point, grid: std.ArrayList(std.ArrayList(u8))) ?u8 {
        if (!self.inBounds(grid)) {
            return null;
        }

        return grid.items[@intCast(self.y)].items[@intCast(self.x)];
    }

    fn set(self: Point, grid: std.ArrayList(std.ArrayList(u8)), value: u8) void {
        grid.items[@intCast(self.y)].items[@intCast(self.x)] = value;
    }

    fn inBounds(self: Point, grid: std.ArrayList(std.ArrayList(u8))) bool {
        return self.x >= 0 and self.y >= 0 and self.y < grid.items.len and self.x < grid.items[@intCast(self.y)].items.len;
    }

    fn antiPoints(self: Point, other: Point, grid: std.ArrayList(std.ArrayList(u8))) AntiIterator {
        return AntiIterator{ .p1 = self, .p2 = other, .grid = grid };
    }
};

fn printGrid(grid: std.ArrayList(std.ArrayList(u8))) void {
    for (grid.items) |row| {
        for (row.items) |char| {
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

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);
    defer grid.deinit();
    defer for (grid.items) |row| {
        row.deinit();
    };

    var seen = std.AutoHashMap(u8, std.ArrayList(Point)).init(allocator);
    defer seen.deinit();
    defer {
        var it = seen.valueIterator();
        while (it.next()) |value| {
            value.deinit();
        }
    }

    var y: isize = -1;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        y += 1;
        if (line.len == 0) {
            continue;
        }
        var lst = std.ArrayList(u8).init(allocator);
        for (line, 0..) |char, x| {
            try lst.append(char);
            if (char == '.') {
                continue;
            }
            var result = try seen.getOrPut(char);
            if (result.found_existing) {
                try result.value_ptr.append(Point{ .x = @intCast(x), .y = y });
                continue;
            }
            var pts = std.ArrayList(Point).init(allocator);
            try pts.append(Point{ .x = @intCast(x), .y = y });
            result.value_ptr.* = pts;
        }
        try grid.append(lst);
    }

    var nodeMap = std.AutoHashMap(Point, bool).init(allocator);
    defer nodeMap.deinit();

    var mapIt = seen.iterator();
    while (mapIt.next()) |entry| {
        const pts = entry.value_ptr.*;
        for (pts.items, 0..) |p1, i| {
            try nodeMap.put(p1, true);
            for (i + 1..pts.items.len) |j| {
                const p2 = pts.items[j];
                try nodeMap.put(p2, true);
                var antiIt = p1.antiPoints(p2, grid);
                while (antiIt.next()) |anti| {
                    if (anti.lookup(grid)) |a| {
                        if (a == '.') {
                            anti.set(grid, '#');
                        }
                        try nodeMap.put(anti, true);
                    }
                }
            }
        }
    }

    // printGrid(grid);
    std.debug.print("{}\n", .{nodeMap.count()});
}
