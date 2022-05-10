package me.hsgamer.flexgames.config.path;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexgames.util.PosUtil;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import net.minestom.server.coordinate.Pos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

@ExtensionMethod({PosUtil.class})
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
        return rawValue.convert().orElse(null);
    }

    @Override
    public @Nullable String convertToRaw(@NotNull Pos value) {
        return value.convert();
    }
}
