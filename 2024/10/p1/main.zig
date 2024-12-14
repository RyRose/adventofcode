const std = @import("std");

fn in_bounds(x: isize, y: isize, width: usize, height: usize) bool {
    return x >= 0 and y >= 0 and x < width and y < height;
}

const Searcher = struct {
    grid: [][]const u8,
    seen: std.AutoHashMap([2]isize, bool),

    fn search(self: *Searcher, prev: u8, x: isize, y: isize) !void {
        if (!in_bounds(x, y, self.grid[0].len, self.grid.len)) {
            return;
        }

        const val = self.grid[@intCast(y)][@intCast(x)];
        if (val == '.') {
            return;
        }
        if ((val - '0') != (prev - '0') + 1) {
            return;
        }

        if (val == '9') {
            try self.seen.put([_]isize{ x, y }, true);
            return;
        }

        try self.search(val, x + 1, y);
        try self.search(val, x - 1, y);
        try self.search(val, x, y + 1);
        try self.search(val, x, y - 1);
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

    var sum: usize = 0;
    for (grid.items, 0..) |row, y| {
        for (row, 0..) |cell, x| {
            const i: isize = @intCast(x);
            const j: isize = @intCast(y);
            if (cell != '0') {
                continue;
            }
            var s = Searcher{
                .grid = grid.items,
                .seen = std.AutoHashMap([2]isize, bool).init(allocator),
            };
            defer s.seen.deinit();
            try s.search('0', i + 1, j);
            try s.search('0', i - 1, j);
            try s.search('0', i, j + 1);
            try s.search('0', i, j - 1);
            sum += s.seen.count();
        }
    }
    std.debug.print("sum: {}\n", .{sum});
}
