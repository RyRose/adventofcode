const std = @import("std");

const Game = struct {
    a: [2]i64 = undefined,
    b: [2]i64 = undefined,
    prize: [2]i64 = undefined,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test.in", 1_000_000);
    defer allocator.free(content);

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
                    game.a[0] = try std.fmt.parseInt(i64, x, 10);
                }
                if (it.next()) |a| {
                    const y = std.mem.trim(u8, a, "Y+");
                    game.a[1] = try std.fmt.parseInt(i64, y, 10);
                }
            }

            if (c_it.next()) |b_line| {
                var it = std.mem.splitScalar(u8, b_line, ' ');
                _ = it.next();
                _ = it.next();
                if (it.next()) |b| {
                    const x = std.mem.trim(u8, b, "X+,");
                    game.b[0] = try std.fmt.parseInt(i64, x, 10);
                }
                if (it.next()) |b| {
                    const y = std.mem.trim(u8, b, "Y+");
                    game.b[1] = try std.fmt.parseInt(i64, y, 10);
                }
            }
            if (c_it.next()) |prize_line| {
                var it = std.mem.splitScalar(u8, prize_line, ' ');
                _ = it.next();
                if (it.next()) |p| {
                    const x = std.mem.trim(u8, p, "X=,");
                    game.prize[0] = try std.fmt.parseInt(i64, x, 10);
                }
                if (it.next()) |p| {
                    const y = std.mem.trim(u8, p, "Y=");
                    game.prize[1] = try std.fmt.parseInt(i64, y, 10);
                }
            }
            // Skip empty line.
            _ = c_it.next();
            try games.append(game);
        }
    }

    var sum: i64 = 0;
    for (games.items) |game| {
        var min_tokens: ?i64 = null;
        var a: i64 = 0;
        while (a * game.a[0] <= game.prize[0] and a * game.a[1] <= game.prize[1]) : (a += 1) {
            const bx = @divTrunc(game.prize[0] - a * game.a[0], game.b[0]);
            const by = @divTrunc(game.prize[1] - a * game.a[1], game.b[1]);
            if (bx != by) {
                continue;
            }

            if (a * game.a[0] + bx * game.b[0] != game.prize[0]) {
                continue;
            }
            if (a * game.a[1] + by * game.b[1] != game.prize[1]) {
                continue;
            }
            const tokens = (a * 3) + bx;
            if (min_tokens == null or tokens < min_tokens.?) {
                min_tokens = tokens;
            }
        }
        if (min_tokens) |m| {
            sum += m;
        }
    }
    std.debug.print("sum: {}\n", .{sum});
}
