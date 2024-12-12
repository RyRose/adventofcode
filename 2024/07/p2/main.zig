const std = @import("std");

pub fn incrementTernaryNumber(ternaryNumber: *std.ArrayList(u2)) !void {
    var carry: bool = true;
    for (ternaryNumber.items, 0..) |digit, i| {
        const newDigit = (digit + 1) % 3;
        ternaryNumber.items[i] = newDigit;

        if (newDigit > 0) {
            carry = false;
            break;
        }
    }
    if (carry) {
        try ternaryNumber.append(1);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    var ints = std.ArrayList(u64).init(allocator);
    defer ints.deinit();

    var ternaryNumber = std.ArrayList(u2).init(allocator);
    defer ternaryNumber.deinit();

    var lines = std.mem.splitScalar(u8, content, '\n');
    var retSum: u64 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        // Clear the values from the previous line.
        ints.clearRetainingCapacity();
        ternaryNumber.clearRetainingCapacity();

        var halves = std.mem.splitScalar(u8, line, ':');
        const test_result = try std.fmt.parseInt(u64, halves.next().?, 10);
        const tail = halves.next().?;
        var parts = std.mem.splitScalar(u8, tail, ' ');
        _ = parts.next();
        while (parts.next()) |part| {
            const value = try std.fmt.parseInt(u64, part, 10);
            try ints.append(value);
        }

        while (ternaryNumber.items.len < ints.items.len) {
            var result: u64 = 0;
            for (ints.items, 0..) |value, index| {
                if (index == 0) {
                    result += value;
                    continue;
                }
                if (result > test_result) {
                    break;
                }

                const operatorIndex = index - 1;
                if (operatorIndex >= ternaryNumber.items.len) {
                    result += value;
                    continue;
                }

                switch (ternaryNumber.items[operatorIndex]) {
                    0 => result += value,
                    1 => result *= value,
                    2 => {
                        const len = std.math.log10(value);
                        result *= std.math.pow(u64, 10, len + 1);
                        result += value;
                    },
                    else => unreachable,
                }
            }
            if (result == test_result) {
                retSum += result;
                break;
            }

            try incrementTernaryNumber(&ternaryNumber);
        }
    }
    std.debug.print("Sum: {}\n", .{retSum});
}
