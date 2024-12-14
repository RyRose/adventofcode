const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var map = std.AutoHashMap(u64, u64).init(allocator);
    defer map.deinit();

    {
        const stripped = std.mem.trim(u8, content, " \n");
        var it = std.mem.splitScalar(u8, stripped, ' ');
        while (it.next()) |part| {
            const val = try std.fmt.parseInt(u64, part, 10);
            const res = try map.getOrPutValue(val, 0);
            res.value_ptr.* += 1;
        }
    }

    var temp = std.AutoHashMap(u64, u64).init(allocator);
    defer temp.deinit();

    const blinks = 75;
    for (0..blinks) |_| {
        temp.clearRetainingCapacity();
        var it = map.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key == 0) {
                const res = try temp.getOrPutValue(1, 0);
                res.value_ptr.* += entry.value_ptr.*;
                continue;
            }

            const digits = std.math.log10(key) + 1;
            if (digits % 2 == 1) {
                const res = try temp.getOrPutValue(key * 2024, 0);
                res.value_ptr.* += entry.value_ptr.*;
                continue;
            }

            const lhs = key / (std.math.pow(u64, 10, digits / 2));
            const rhs = key % std.math.pow(u64, 10, digits / 2);
            {
                const res = try temp.getOrPutValue(lhs, 0);
                res.value_ptr.* += entry.value_ptr.*;
            }
            {
                const res = try temp.getOrPutValue(rhs, 0);
                res.value_ptr.* += entry.value_ptr.*;
            }
        }
        const t = map;
        map = temp;
        temp = t;
    }

    var sum: u64 = 0;
    var it = map.valueIterator();
    while (it.next()) |val| {
        sum += val.*;
    }
    std.debug.print("sum = {}\n", .{sum});
}
