package me.hsgamer.epicmegagames.config.path;

import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.coordinate.Pos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class PosPath extends AdvancedConfigPath<String, Pos> {
    public PosPath(@NotNull String path, @Nullable Pos def) {
        super(path, def);
    }

    @Override
    public @Nullable String getFromConfig(@NotNull Config config) {
        return Objects.toString(config.get(getPath()), null);
    }

    @Override
    public @Nullable Pos convert(@NotNull String rawValue) {
        String[] split = rawValue.split(",", 5);
        if (split.length != 5) {
            return null;
        }
        try {
            return new Pos(
                    Double.parseDouble(split[0].trim()),
                    Double.parseDouble(split[1].trim()),
                    Double.parseDouble(split[2].trim()),
                    Float.parseFloat(split[3].trim()),
                    Float.parseFloat(split[4].trim())
            );
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    @Override
    public @Nullable String convertToRaw(@NotNull Pos value) {
        return String.format("%s,%s,%s,%s,%s", value.x(), value.y(), value.z(), value.yaw(), value.pitch());
    }
}
