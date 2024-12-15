const std = @import("std");

const Game = struct {
    a: [2]i128 = undefined,
    b: [2]i128 = undefined,
    prize: [2]i128 = undefined,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test.in", 1_000_000);
    defer allocator.free(content);

    const addend = 10000000000000;

    var games = std.ArrayList(Game).init(allocator);
    defer games.deinit();
    {
        var c_it = std.mem.splitScalar(u8, content, '\n');
        while (c_it.next()) |a_line| {
            if (a_line.len == 0) {
                continue;
            }
            var game = Game{};

            {
                var it = std.mem.splitScalar(u8, a_line, ' ');
                _ = it.next();
                _ = it.next();
                if (it.next()) |a| {
                    const x = std.mem.trim(u8, a, "X+,");
                    game.a[0] = try std.fmt.parseInt(i128, x, 10);
                }
                if (it.next()) |a| {
                    const y = std.mem.trim(u8, a, "Y+");
                    game.a[1] = try std.fmt.parseInt(i128, y, 10);
                }
            }

            if (c_it.next()) |b_line| {
                var it = std.mem.splitScalar(u8, b_line, ' ');
                _ = it.next();
                _ = it.next();
                if (it.next()) |b| {
                    const x = std.mem.trim(u8, b, "X+,");
                    game.b[0] = try std.fmt.parseInt(i128, x, 10);
                }
                if (it.next()) |b| {
                    const y = std.mem.trim(u8, b, "Y+");
                    game.b[1] = try std.fmt.parseInt(i128, y, 10);
                }
            }
            if (c_it.next()) |prize_line| {
                var it = std.mem.splitScalar(u8, prize_line, ' ');
                _ = it.next();
                if (it.next()) |p| {
                    const x = std.mem.trim(u8, p, "X=,");
                    game.prize[0] = try std.fmt.parseInt(i128, x, 10) + addend;
                }
                if (it.next()) |p| {
                    const y = std.mem.trim(u8, p, "Y=");
                    game.prize[1] = try std.fmt.parseInt(i128, y, 10) + addend;
                }
            }
            // Skip empty line.
            _ = c_it.next();
            try games.append(game);
        }
    }

    var sum: i128 = 0;
    for (games.items) |game| {
        const x1 = game.a[0];
        const x2 = game.b[0];
        const x3 = game.prize[0];
        const y1 = game.a[1];
        const y2 = game.b[1];
        const y3 = game.prize[1];

        const a = std.math.divExact(i128, x3 * y2 - x2 * y3, x1 * y2 - x2 * y1) catch {
            continue;
        };
        const b = std.math.divExact(i128, x3 * y1 - x1 * y3, x2 * y1 - x1 * y2) catch {
            continue;
        };
        sum += 3 * a + b;
    }
    std.debug.print("sum: {}\n", .{sum});
}
