package me.hsgamer.flexegames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.ConfigPath;
import net.minestom.server.permission.Permission;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class PermissionListPath implements ConfigPath<List<Permission>> {
    private final PermissionPairListPath permissionPairListPath;

    public PermissionListPath(@NotNull String path, @Nullable List<Permission> def) {
        permissionPairListPath = new PermissionPairListPath(path, def == null ? null : def.stream().map(PermissionPairListPath.PermissionPair::fromPermission).toList());
    }

    @Override
    public List<Permission> getValue() {
        return permissionPairListPath.getValue().stream().map(PermissionPairListPath.PermissionPair::toPermission).toList();
    }

    @Override
    public void setValue(@Nullable List<Permission> value) {
        permissionPairListPath.setValue(value == null ? null : value.stream().map(PermissionPairListPath.PermissionPair::fromPermission).toList());
    }

    @Override
    public List<Permission> getValue(@NotNull Config config) {
        return permissionPairListPath.getValue(config).stream().map(PermissionPairListPath.PermissionPair::toPermission).toList();
    }

    @Override
    public void setValue(@Nullable List<Permission> value, @NotNull Config config) {
        permissionPairListPath.setValue(value == null ? null : value.stream().map(PermissionPairListPath.PermissionPair::fromPermission).toList(), config);
    }

    @Override
    public @NotNull String getPath() {
        return permissionPairListPath.getPath();
    }

    @Override
    public @Nullable Config getConfig() {
        return permissionPairListPath.getConfig();
    }

    @Override
    public void setConfig(@NotNull Config config) {
        permissionPairListPath.setConfig(config);
    }
}
