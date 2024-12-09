const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var rules = std.StringHashMap(std.ArrayList([]const u8)).init(allocator);
    defer rules.deinit();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var rulesAllocator = std.heap.ArenaAllocator.init(allocator);
    defer rulesAllocator.deinit();
    const rulesAlloc = rulesAllocator.allocator();

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }
        var split = std.mem.splitScalar(u8, line, '|');
        const lhs = split.next().?;
        const rhs = split.next().?;
        if (rules.getPtr(lhs)) |list| {
            try list.append(rhs);
        } else {
            var list = std.ArrayList([]const u8).init(rulesAlloc);
            try list.append(rhs);
            try rules.put(lhs, list);
        }
    }

    var middleSum: usize = 0;
    lineLoop: while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        var split = std.mem.splitScalar(u8, line, ',');
        var len: u32 = 0;
        var seen = std.BufSet.init(allocator);
        defer seen.deinit();
        while (split.next()) |page| {
            const values = rules.get(page) orelse std.ArrayList([]const u8).init(rulesAlloc);
            for (values.items) |value| {
                if (!seen.contains(value)) {
                    continue;
                }
                continue :lineLoop;
            }

            try seen.insert(page);
            len += 1;
        }

        split.reset();
        const end = try std.math.divFloor(usize, len, 2);
        for (0..end) |_| {
            _ = split.next();
        }
        middleSum += try std.fmt.parseInt(u32, split.next().?, 10);
    }
    std.debug.print("{d}\n", .{middleSum});
}
