const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var ints = std.ArrayList(u64).init(allocator);
    defer ints.deinit();
    var lines = std.mem.splitScalar(u8, content, '\n');
    var retSum: u64 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        // Clear the values from the previous line.
        ints.clearRetainingCapacity();

        var halves = std.mem.splitScalar(u8, line, ':');
        const test_result = try std.fmt.parseInt(u64, halves.next().?, 10);
        const tail = halves.next().?;
        var parts = std.mem.splitScalar(u8, tail, ' ');
        _ = parts.next();
        while (parts.next()) |part| {
            const value = try std.fmt.parseInt(u64, part, 10);
            try ints.append(value);
        }

        for (0..@intCast(@as(usize, 1) << @intCast(ints.items.len))) |i| {
            var result: u64 = 0;
            for (ints.items, 0..) |value, index| {
                if (index == 0) {
                    result += value;
                    continue;
                }
                if (i & (@as(usize, 1) << @intCast(index)) == 0) {
                    result += value;
                } else {
                    result *= value;
                }
            }
            if (result == test_result) {
                retSum += result;
                break;
            }
        }
    }
    std.debug.print("Sum: {}\n", .{retSum});
}
