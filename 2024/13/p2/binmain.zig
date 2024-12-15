// Binary search attempt. Doesn't work!
const std = @import("std");

const Game = struct {
    a: [2]i128 = undefined,
    b: [2]i128 = undefined,
    prize: [2]i128 = undefined,
};

fn bsearch(game: *const Game, l: i128, r: i128) ?i128 {
    if (l + 1 >= r) {
        return null;
    }
    const a = @divTrunc(l + r, 2);
    const bx = @divTrunc(game.prize[0] - a * game.a[0], game.b[0]);
    const by = @divTrunc(game.prize[1] - a * game.a[1], game.b[1]);
    const diff = by - bx;

    // Found the answer.
    var min: ?i128 = null;
    if (diff == 0) {
        if (a * game.a[0] + bx * game.b[0] != game.prize[0]) {}
        if (a * game.a[1] + by * game.b[1] != game.prize[1]) {}
        min = (a * 3) + bx;
    }
    if (diff < 0) {
        if (bsearch(game, a, r)) |m| {
            if (min == null or m < min.?) {
                min = m;
            }
        }
    } else {
        if (bsearch(game, l, a)) |m| {
            if (min == null or m < min.?) {
                min = m;
            }
        }
    }

    return min;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/sample0.in", 1_000_000);
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
                    game.prize[0] = try std.fmt.parseInt(i128, x, 10);
                    // game.prize[0] = try std.fmt.parseInt(i128, x, 10) + 10000000000000;
                }
                if (it.next()) |p| {
                    const y = std.mem.trim(u8, p, "Y=");
                    game.prize[1] = try std.fmt.parseInt(i128, y, 10);
                    // game.prize[1] = try std.fmt.parseInt(i128, y, 10) + 10000000000000;
                }
            }
            // Skip empty line.
            _ = c_it.next();
            try games.append(game);
        }
    }

    var sum: i128 = 0;
    for (games.items) |game| {
        std.debug.print("{}\n", .{game});
        const min_tokens: ?i128 = bsearch(&game, 0, 1000000000000000);
        // {
        //     var l: i128 = 0;
        //     var r: i128 = 1000000000000000;
        //     while (l + 1 < r) {
        //         // std.debug.print("l: {}, r: {}\n", .{ l, r });
        //         const a = @divTrunc(l + r, 2);
        //         const bx = @divTrunc(game.prize[0] - a * game.a[0], game.b[0]);
        //         const by = @divTrunc(game.prize[1] - a * game.a[1], game.b[1]);
        //         const diff = by - bx;

        //         // Found the answer.
        //         if (diff == 0) {
        //             if (a * game.a[0] + bx * game.b[0] != game.prize[0]) {
        //                 break;
        //             }
        //             if (a * game.a[1] + by * game.b[1] != game.prize[1]) {
        //                 break;
        //             }
        //             min_tokens = (a * 3) + bx;
        //         }
        //         // Go right.
        //         if (diff > 0) {
        //             l = a;
        //             continue;
        //         }
        //         r = a;
        //     }
        // }
        if (min_tokens) |m| {
            std.debug.print("m: {}\n", .{m});
            sum += m;
        }
    }
    std.debug.print("sum: {}\n", .{sum});
}
