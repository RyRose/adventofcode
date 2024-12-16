const std = @import("std");

const Robot = struct {
    p: [2]i128 = undefined,
    v: [2]i128 = undefined,

    fn move(self: *Robot, width: i128, height: i128) void {
        self.p[0] += self.v[0];
        self.p[0] = @mod(self.p[0], width);
        self.p[1] += self.v[1];
        self.p[1] = @mod(self.p[1], height);
    }

    fn quadrant(self: *Robot, width: i128, height: i128) ?u4 {
        const midwidth = @divTrunc(width, 2);
        const midheight = @divTrunc(height, 2);

        // Return null if on quadrant lines.
        if (self.p[0] == midwidth) {
            return null;
        }
        if (self.p[1] == midheight) {
            return null;
        }

        if (self.p[0] < midwidth) {
            if (self.p[1] < midheight) {
                return 0;
            } else {
                return 1;
            }
        } else {
            if (self.p[1] < midheight) {
                return 2;
            } else {
                return 3;
            }
        }
    }

    fn printgrid(self: *Robot, width: i128, height: i128) void {
        for (0..@intCast(height)) |y| {
            for (0..@intCast(width)) |x| {
                if (x == self.p[0] and y == self.p[1]) {
                    std.debug.print("{s}", .{"1"});
                    continue;
                }
                std.debug.print(".", .{});
            }
            std.debug.print("\n", .{});
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    // const width = 11;
    // const height = 7;
    // const content = try std.fs.cwd().readFileAlloc(allocator, "./data/sample0.in", 1_000_000);
    const width = 101;
    const height = 103;
    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test.in", 1_000_000);
    defer allocator.free(content);

    var robots = std.ArrayList(Robot).init(allocator);
    defer robots.deinit();
    {
        var lit = std.mem.splitScalar(u8, content, '\n');
        while (lit.next()) |line| {
            if (line.len == 0) {
                continue;
            }
            var robot = Robot{};

            const line0 = std.mem.trim(u8, line, "p=");
            var sit = std.mem.splitScalar(u8, line0, ' ');

            {
                var it = std.mem.splitScalar(u8, sit.next().?, ',');
                robot.p[0] = try std.fmt.parseInt(i128, it.next().?, 10);
                robot.p[1] = try std.fmt.parseInt(i128, it.next().?, 10);
            }

            {
                const value = std.mem.trim(u8, sit.next().?, "v=");
                var it = std.mem.splitScalar(u8, value, ',');
                robot.v[0] = try std.fmt.parseInt(i128, it.next().?, 10);
                robot.v[1] = try std.fmt.parseInt(i128, it.next().?, 10);
            }

            try robots.append(robot);
        }
    }

    var quads = [_]u64{ 0, 0, 0, 0 };
    for (robots.items) |*robot| {
        for (0..100) |_| {
            robot.move(width, height);
        }
        if (robot.quadrant(width, height)) |q| {
            quads[q] += 1;
        }
    }
    std.debug.print("{d} {d} {d} {d}\n", .{ quads[0], quads[1], quads[2], quads[3] });
    std.debug.print("multiplier: {d}\n", .{quads[0] * quads[1] * quads[2] * quads[3]});
}
