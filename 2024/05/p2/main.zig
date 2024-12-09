const std = @import("std");

fn printMap(map: std.StringHashMap(std.BufSet)) void {
    var it = map.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}: ", .{entry.key_ptr.*});
        var set = entry.value_ptr.*;
        var setIt = set.iterator();
        while (setIt.next()) |value| {
            std.debug.print("'{s}' ", .{value.*});
        }
        std.debug.print("\n", .{});
    }
}

fn printSet(set: std.BufSet) void {
    var it = set.iterator();
    while (it.next()) |entry| {
        std.debug.print("'{s}' ", .{entry.*});
    }
    std.debug.print("\n", .{});
}

fn makeMapping(content: []const u8, rules: *std.StringHashMap(std.BufSet)) !void {
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }
        var split = std.mem.splitScalar(u8, line, '|');
        const lhs = split.next().?;
        const rhs = split.next().?;
        if (rules.getPtr(lhs)) |set| {
            try set.insert(rhs);
        } else {
            var set = std.BufSet.init(rules.allocator);
            try set.insert(rhs);
            try rules.put(lhs, set);
        }
    }
}

fn makeMappingWith(content: []const u8, rules: *std.StringHashMap(std.BufSet), with: std.BufSet) !void {
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) {
            break;
        }
        var split = std.mem.splitScalar(u8, line, '|');
        const rhs = split.next().?;
        const lhs = split.next().?;
        if (!with.contains(lhs) or !with.contains(rhs)) {
            continue;
        }

        if (rules.getPtr(lhs)) |set| {
            try set.insert(rhs);
        } else {
            var set = std.BufSet.init(rules.allocator);
            try set.insert(rhs);
            try rules.put(lhs, set);
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var mainAllocator = std.heap.ArenaAllocator.init(allocator);
    defer mainAllocator.deinit();
    const mainAlloc = mainAllocator.allocator();

    var rules = std.StringHashMap(std.BufSet).init(allocator);
    defer rules.deinit();
    try makeMapping(content, &rules);

    var middleSum: usize = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        var valid = true;
        validBlock: {
            var seen = std.BufSet.init(allocator);
            var split = std.mem.splitScalar(u8, line, ',');
            defer seen.deinit();
            while (split.next()) |page| {
                const values = rules.get(page) orelse std.BufSet.init(mainAlloc);
                var it = values.iterator();
                while (it.next()) |value| {
                    if (!seen.contains(value.*)) {
                        continue;
                    }
                    valid = false;
                    break :validBlock;
                }
                try seen.insert(page);
            }
        }
        if (valid) {
            continue;
        }

        var linePages = std.BufSet.init(allocator);
        defer linePages.deinit();
        {
            var split = std.mem.splitScalar(u8, line, ',');
            while (split.next()) |page| {
                try linePages.insert(page);
            }
        }

        var lineAllocator = std.heap.ArenaAllocator.init(allocator);
        defer lineAllocator.deinit();
        const lineAlloc = lineAllocator.allocator();

        var graph = std.StringHashMap(std.BufSet).init(lineAlloc);
        defer graph.deinit();
        try makeMappingWith(content, &graph, linePages);

        var noIncomingEdges = std.BufSet.init(allocator);
        defer noIncomingEdges.deinit();

        var split = std.mem.splitScalar(u8, line, ',');
        while (split.next()) |page| {
            if (graph.contains(page)) {
                continue;
            }
            try noIncomingEdges.insert(page);
        }

        var sortedElements = std.ArrayList([]const u8).init(lineAlloc);
        defer sortedElements.deinit();
        while (noIncomingEdges.count() > 0) {
            var nieIt = noIncomingEdges.iterator();
            const node = nieIt.next().?.*;
            defer noIncomingEdges.remove(node);
            {
                const temp = try lineAlloc.alloc(u8, node.len);
                @memcpy(temp, node);
                try sortedElements.append(temp);
            }

            var entriesToRemove = std.BufSet.init(lineAlloc);
            defer entriesToRemove.deinit();

            var graphIt = graph.iterator();
            while (graphIt.next()) |entry| {
                var arrValue = entry.value_ptr.*;
                var foundVal: ?[]const u8 = null;
                var it = arrValue.iterator();
                while (it.next()) |value| {
                    if (!std.mem.eql(u8, node, value.*)) {
                        continue;
                    }
                    foundVal = value.*;
                    break;
                }
                if (foundVal == null) {
                    continue;
                }

                arrValue.remove(foundVal.?);
                var setIt = arrValue.iterator();
                if (setIt.next() != null) {
                    continue;
                }
                try entriesToRemove.insert(entry.key_ptr.*);
                try noIncomingEdges.insert(entry.key_ptr.*);
            }

            var ertIt = entriesToRemove.iterator();
            while (ertIt.next()) |entry| {
                _ = graph.remove(entry.*);
            }
        }

        const middle = try std.math.divFloor(usize, sortedElements.items.len, 2);
        middleSum += try std.fmt.parseInt(u32, sortedElements.items[middle], 10);
    }
    std.debug.print("{d}\n", .{middleSum});
}
