package me.hsgamer.flexegames.config.path;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.flexegames.util.PosUtil;
import me.hsgamer.hscore.common.CollectionUtils;
import net.minestom.server.coordinate.Pos;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

@ExtensionMethod({PosUtil.class, CollectionUtils.class})
public class PosListPath extends StringListToObjectPath<List<Pos>> {
    public PosListPath(@NotNull String path, @Nullable List<Pos> def) {
        super(path, def);
    }

    @Override
    public @Nullable List<Pos> convert(@NotNull List<String> rawValue) {
        List<Pos> list = new ArrayList<>();
        for (String s : rawValue) {
            s.convert().ifPresent(list::add);
        }
        return list;
    }

    @Override
    public @Nullable List<String> convertToRaw(@NotNull List<Pos> value) {
        return value.stream().map(PosUtil::convert).toList();
    }
}
