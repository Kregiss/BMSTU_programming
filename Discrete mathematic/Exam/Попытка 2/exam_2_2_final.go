package main

/*
решение на java

import java.util.*;

class File {
    String name;
    int timestampPas;
    int timestampDcu;
    List<String> dependencies;

    public File(String name, int timestampPas, int timestampDcu, List<String> dependencies) {
        this.name = name;
        this.timestampPas = timestampPas;
        this.timestampDcu = timestampDcu;
        this.dependencies = dependencies;
    }
}

public class PascalCompiler {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        Map<String, File> files = new HashMap<>();

        // Reading files and dependencies
        for (int i = 0; i < n; i++) {
            String name = scanner.next();
            int k = scanner.nextInt();
            List<String> dependencies = new ArrayList<>();
            for (int j = 0; j < k; j++) {
                dependencies.add(scanner.next());
            }
            files.put(name, new File(name, 0, 0, dependencies));
        }

        int m = scanner.nextInt();
        // Reading timestamps
        for (int i = 0; i < m; i++) {
            String filename = scanner.next();
            int timestamp = scanner.nextInt();

            String name;
            if (filename.endsWith(".pas")) {
                name = filename.substring(0, filename.length() - 4);
                File file = files.getOrDefault(name, new File(name, timestamp, 0, new ArrayList<>()));
                file.timestampPas = timestamp;
                files.put(name, file);
            } else if (filename.endsWith(".dcu")) {
                name = filename.substring(0, filename.length() - 4);
                File file = files.getOrDefault(name, new File(name, 0, timestamp, new ArrayList<>()));
                file.timestampDcu = timestamp;
                files.put(name, file);
            }
        }

        List<String> recompile = findFilesToRecompile(files);
        if (recompile != null) {
            Collections.sort(recompile);
            for (String filename : recompile) {
                System.out.println(filename);
            }
        }
    }

    private static List<String> findFilesToRecompile(Map<String, File> files) {
        List<String> recompile = new ArrayList<>();
        Map<String, Boolean> visited = new HashMap<>();
        Map<String, Boolean> inStack = new HashMap<>();
        Map<String, Boolean> recMap = new HashMap<>();

        for (File file : files.values()) {
            if (!visited.getOrDefault(file.name, false)) {
                if (dfs(file, files, visited, inStack, recompile, recMap)) {
                    System.out.println("!CYCLE");
                    return null;
                }
            }
        }

        return recompile;
    }

    private static boolean dfs(File file, Map<String, File> files, Map<String, Boolean> visited,
                               Map<String, Boolean> inStack, List<String> recompile, Map<String, Boolean> recMap) {
        if (inStack.getOrDefault(file.name, false)) {
            return true;
        }
        if (visited.getOrDefault(file.name, false)) {
            return false;
        }
        visited.put(file.name, true);
        inStack.put(file.name, true);

        for (String dependency : file.dependencies) {
            File depFile = files.get(dependency);
            if (depFile != null) {
                if (dfs(depFile, files, visited, inStack, recompile, recMap)) {
                    return true;
                }
            }
        }

        inStack.put(file.name, false);

        boolean needsRecompile = file.name.equals("main") || file.timestampDcu == 0 || file.timestampPas > file.timestampDcu;
        if (!needsRecompile) {
            for (String dependency : file.dependencies) {
                File depFile = files.get(dependency);
                if (depFile != null && (depFile.timestampPas > file.timestampDcu ||
                        depFile.timestampDcu > file.timestampDcu || recMap.getOrDefault(dependency, false))) {
                    needsRecompile = true;
                    break;
                }
            }
        }

        if (needsRecompile) {
            recompile.add(file.name + ".pas");
            recMap.put(file.name, true);
        }

        return false;
    }
}
*/

/*
6
parser    2 lexer syntree
lexer     0
main      3 parser semantic generator
generator 1 syntree
syntree   0
semantic  1 syntree
10
lexer.dcu     26
parser.pas    30
lexer.pas     21
syntree.pas   23
syntree.dcu   25
generator.dcu 20
semantic.pas  33
generator.pas 15
semantic.dcu  31
main.pas      28

generator.pas
main.pas
parser.pas
semantic.pas


10
uytecur  0
main     2 yvrexen uytecur
yvrexen  2 amaoyueh ajjoacvo
anafs    0
cumeg    1 bylfaody
anelov   0
ajjoacvo 0
amaoyueh 2 oeoky anelov
bylfaody 0
oeoky    2 anafs cumeg
19
ajjoacvo.pas 1718450743
amaoyueh.dcu 1718450743
main.pas     1718450754
anafs.pas    1718450732
bylfaody.dcu 1718450738
cumeg.dcu    1718450740
oeoky.dcu    1718450742
oeoky.pas    1718450732
anelov.dcu   1718450741
bylfaody.pas 1718450735
amaoyueh.pas 1718450733
yvrexen.pas  1718450745
yvrexen.dcu  1718450746
cumeg.pas    1718450737
ajjoacvo.dcu 1718450753
anelov.pas   1718450740
anafs.dcu    1718450733
uytecur.pas  1718450749
uytecur.dcu  1718450742

main.pas
uytecur.pas
yvrexen.pas
*/