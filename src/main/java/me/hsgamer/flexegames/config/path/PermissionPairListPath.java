package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jglrxavpok.hephaistos.nbt.NBT;
import org.jglrxavpok.hephaistos.nbt.NBTCompound;
import org.jglrxavpok.hephaistos.parser.SNBTParser;

import java.io.Reader;
import java.io.StringReader;
import java.util.*;

public class PermissionPairListPath extends AdvancedConfigPath<List<Map<String, String>>, List<PermissionPairListPath.PermissionPair>> {
    private static final String PERMISSION_KEY = "permission";
    private static final String DATA_KEY = "data";

    public PermissionPairListPath(@NotNull String path, @Nullable List<PermissionPair> def) {
        super(path, def);
    }

    @Override
    public @Nullable List<Map<String, String>> getFromConfig(@NotNull Config config) {
        if (!config.contains(getPath())) return null;
        var rawValue = config.get(getPath());
        if (rawValue instanceof List<?> rawList) {
            List<Map<String, String>> list = new ArrayList<>();
            for (var o : rawList) {
                if (o instanceof Map<?, ?> rawMap) {
                    Map<String, String> map = new LinkedHashMap<>();
                    for (Map.Entry<?, ?> entry : rawMap.entrySet()) {
                        map.put(Objects.toString(entry.getKey()), Objects.toString(entry.getValue()));
                    }
                    list.add(map);
                } else if (o instanceof String rawString) {
                    list.add(Map.of(PERMISSION_KEY, rawString));
                }
            }
            return list;
        } else if (rawValue instanceof Map<?, ?> rawMap) {
            Map<String, String> map = new LinkedHashMap<>();
            for (var entry : rawMap.entrySet()) {
                map.put(Objects.toString(entry.getKey()), Objects.toString(entry.getValue()));
            }
            return List.of(map);
        } else if (rawValue instanceof String rawString) {
            return List.of(Map.of(PERMISSION_KEY, rawString));
        }
        return null;
    }

    @Override
    public @Nullable List<PermissionPair> convert(@NotNull List<Map<String, String>> rawValue) {
        List<PermissionPair> list = new ArrayList<>();
        for (Map<String, String> map : rawValue) {
            String permission = map.get(PERMISSION_KEY);
            if (permission == null) continue;

            NBTCompound data = null;
            if (map.containsKey(DATA_KEY)) {
                String value = map.get(DATA_KEY);
                Reader reader = new StringReader(value);
                try {
                    NBT nbt = new SNBTParser(reader).parse();
                    if (nbt instanceof NBTCompound compound) {
                        data = compound;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            list.add(new PermissionPair(permission, data));
        }
        return list;
    }

    @Override
    public @Nullable List<Map<String, String>> convertToRaw(@NotNull List<PermissionPair> value) {
        List<Map<String, String>> list = new ArrayList<>();
        for (PermissionPair permissionPair : value) {
            list.add(Map.of(
                    PERMISSION_KEY, permissionPair.permission,
                    DATA_KEY, permissionPair.data.toSNBT()
            ));
        }
        return list;
    }

    public record PermissionPair(String permission, NBTCompound data) {
        public static PermissionPair fromPermission(Permission permission) {
            return new PermissionPair(permission.getPermissionName(), permission.getNBTData());
        }

        public Permission toPermission() {
            return new Permission(permission, data);
        }
    }
}
