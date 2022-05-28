package me.hsgamer.flexegames.config.path;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.util.PosUtil;
import net.minestom.server.coordinate.Pos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@ExtensionMethod({PosUtil.class})
public class PosPath extends StringToObjectPath<Pos> {
    public PosPath(@NotNull String path, @Nullable Pos def) {
        super(path, def);
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
