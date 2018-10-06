import com.warrior.dynamic.connectivity.FastDynamicConnectivity;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

public class BenchmarkProgram {
    public static class Instruction {
        public static enum Tag {
            INSERT,
            LINK,
            DELETE,
            CUT,
            CONNECTED
        }

        public Tag tag = Tag.INSERT;
        public int x = 0;
        public int y = 0;
        public boolean expect = false;
    }

    public static List<Instruction> parseInstructions(InputStream stream) {
        List<Instruction> instructions = new ArrayList<Instruction>();
        Scanner scanner = new Scanner(stream);

        while (scanner.hasNext()) {
            String tag = scanner.next();
            Instruction instr = new Instruction();

            if (tag.equals("insert")) {
                instr.tag = Instruction.Tag.INSERT;
                instr.x = scanner.nextInt();
            } else if (tag.equals("link")) {
                instr.tag = Instruction.Tag.LINK;
                instr.x = scanner.nextInt();
                instr.y = scanner.nextInt();
            } else if (tag.equals("delete")) {
                instr.tag = Instruction.Tag.DELETE;
                instr.x = scanner.nextInt();
            } else if (tag.equals("cut")) {
                instr.tag = Instruction.Tag.CUT;
                instr.x = scanner.nextInt();
                instr.y = scanner.nextInt();
            } else if (tag.equals("connected")) {
                instr.tag = Instruction.Tag.CONNECTED;
                instr.x = scanner.nextInt();
                instr.y = scanner.nextInt();
                instr.expect = scanner.nextBoolean();
            } else {
                throw new RuntimeException("Unknown instruction: " + tag);
            }

            instructions.add(instr);
        }

        return instructions;
    }

    public static int getMaxVertex(List<Instruction> instructions) {
        int max = 0;
        for (Instruction i: instructions) {
            if (i.x > max) max = i.x;
            if (i.y > max) max = i.y;
        }
        return max;
    }

    public static void run(List<Instruction> instructions, int maxVertex) {
        FastDynamicConnectivity fdc =
                new FastDynamicConnectivity(maxVertex + 1);

        Map<Integer, Set<Integer>> edges = new HashMap<Integer, Set<Integer>>();

        for (Instruction i: instructions) {
            if (i.tag == Instruction.Tag.INSERT) {
                edges.put(i.x, new HashSet<Integer>());

            } else if (i.tag == Instruction.Tag.LINK) {
                edges.get(i.x).add(i.y);
                edges.get(i.y).add(i.x);
                fdc.link(i.x, i.y);

            } else if (i.tag == Instruction.Tag.DELETE) {
                for (int neighbour: edges.get(i.x)) {
                    edges.get(neighbour).remove(i.x);
                    fdc.cut(i.x, neighbour);
                }
                edges.remove(i.x);

            } else if (i.tag == Instruction.Tag.CUT) {
                edges.get(i.x).remove(i.y);
                edges.get(i.y).remove(i.x);
                fdc.cut(i.x, i.y);

            } else if (i.tag == Instruction.Tag.CONNECTED) {
                boolean c = fdc.reach(i.x, i.y);
                if (c != i.expect) {
                    throw new RuntimeException("Unexpected result!");
                }
            }
        }
    }

    public static void main(String[] args) {
        List<Instruction> instructions = parseInstructions(System.in);
        int maxVertex = getMaxVertex(instructions);
        System.err.println("Running " + instructions.size() +
                " instructions, max vertex=" + maxVertex + "...");

        /* Warmup */
        run(instructions, maxVertex);
        int samples = 10;
        long total = 0;

        for (int i = 0; i < samples; i++) {
           long start = System.currentTimeMillis();
           run(instructions, maxVertex);
           long end = System.currentTimeMillis();
           total += (end - start);
        }

        System.out.println("" + ((double) total / 1000.0) + " s");
    }
}
