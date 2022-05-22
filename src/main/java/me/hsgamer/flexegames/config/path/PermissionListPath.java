package me.hsgamer.flexegames.config.path;

import com.google.gson.Gson;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jglrxavpok.hephaistos.json.NBTGsonReader;
import org.jglrxavpok.hephaistos.nbt.NBT;
import org.jglrxavpok.hephaistos.nbt.NBTCompound;
import org.jglrxavpok.hephaistos.nbt.NBTType;
import org.jglrxavpok.hephaistos.parser.SNBTParser;

import java.io.Reader;
import java.io.StringReader;
import java.util.*;

public class PermissionListPath extends AdvancedConfigPath<List<Map<String, ?>>, List<Permission>> {
    private static final Gson GSON = new Gson();
    private static final String PERMISSION_KEY = "permission";
    private static final String DATA_KEY = "data";

    public PermissionListPath(@NotNull String path, @Nullable List<Permission> def) {
        super(path, def);
    }

    @Override
    public @Nullable List<Map<String, ?>> getFromConfig(@NotNull Config config) {
        if (!config.contains(getPath())) return null;
        var rawValue = config.get(getPath());
        if (rawValue instanceof List<?> rawList) {
            List<Map<String, ?>> list = new ArrayList<>();
            for (var o : rawList) {
                if (o instanceof Map<?, ?> rawMap) {
                    Map<String, Object> map = new LinkedHashMap<>();
                    for (Map.Entry<?, ?> entry : rawMap.entrySet()) {
                        map.put(Objects.toString(entry.getKey()), entry.getValue());
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
    public @Nullable List<Permission> convert(@NotNull List<Map<String, ?>> rawValue) {
        List<Permission> list = new ArrayList<>();
        for (Map<String, ?> map : rawValue) {
            String permission = Objects.toString(map.get(PERMISSION_KEY));
            if (permission == null) continue;

            NBTCompound data = null;
            if (map.containsKey(DATA_KEY)) {
                Object value = map.get(DATA_KEY);
                if (value instanceof String dataString) {
                    try (Reader reader = new StringReader(dataString)) {
                        NBT nbt = new SNBTParser(reader).parse();
                        if (nbt instanceof NBTCompound compound) {
                            data = compound;
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                } else if (value instanceof Map<?, ?> dataMap) {
                    String json = GSON.toJson(dataMap);
                    try (Reader reader = new StringReader(json)) {
                        NBT nbt = new NBTGsonReader(reader).read(NBTType.TAG_Compound);
                        if (nbt instanceof NBTCompound compound) {
                            data = compound;
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }

            list.add(new Permission(permission, data));
        }
        return list;
    }

    @Override
    public @Nullable List<Map<String, ?>> convertToRaw(@NotNull List<Permission> value) {
        List<Map<String, ?>> list = new ArrayList<>();
        for (var permissionPair : value) {
            Map<String, String> map = new LinkedHashMap<>();
            map.put(PERMISSION_KEY, permissionPair.getPermissionName());
            if (permissionPair.getNBTData() != null) {
                map.put(DATA_KEY, permissionPair.getNBTData().toSNBT());
            }
            list.add(map);
        }
        return list;
    }
}
