const std = @import("std");

fn print_list(node: ?*std.SinglyLinkedList(u64).Node) void {
    var cur = node;
    while (cur != null) {
        std.debug.print("{} ", .{cur.?.data});
        cur = cur.?.next;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "./data/test1.in", 1_000_000);
    defer allocator.free(content);

    const L = std.SinglyLinkedList(u48);
    var stones = L{};

    var memory_index: usize = 0;
    const memory = try allocator.alloc(L.Node, 1_000_000);
    defer allocator.free(memory);

    {
        const stripped = std.mem.trim(u8, content, " \n");
        var it = std.mem.splitScalar(u8, stripped, ' ');
        while (it.next()) |part| {
            var node: *L.Node = &memory[memory_index];
            memory_index += 1;
            node.data = try std.fmt.parseInt(u48, part, 10);
            stones.prepend(node);
        }
    }

    const blinks = 25;
    for (0..blinks) |_| {
        var cur = stones.first;
        while (cur) |c| {
            if (c.data == 0) {
                c.data = 1;
                cur = c.next;
                continue;
            }
            const digits = std.math.log10(c.data) + 1;
            if (digits % 2 == 1) {
                c.data *= 2024;
                cur = c.next;
                continue;
            }

            const lhs = c.data / (std.math.pow(u48, 10, digits / 2));
            const rhs = c.data % std.math.pow(u48, 10, digits / 2);

            c.data = rhs;

            var node: *L.Node = &memory[memory_index];
            memory_index += 1;
            node.data = lhs;
            c.insertAfter(node);
            cur = node.next;
        }
    }

    var sum: u64 = 0;
    var cur = stones.first;
    while (cur) |c| {
        sum += 1;
        cur = c.next;
    }
    std.debug.print("sum = {}\n", .{sum});
}
