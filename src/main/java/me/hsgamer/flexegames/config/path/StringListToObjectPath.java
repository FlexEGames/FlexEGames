package me.hsgamer.flexegames.config.path;

import lombok.experimental.ExtensionMethod;
import me.hsgamer.hscore.common.CollectionUtils;
import me.hsgamer.hscore.config.Config;
import me.hsgamer.hscore.config.path.AdvancedConfigPath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

@ExtensionMethod(CollectionUtils.class)
public abstract class StringListToObjectPath<T> extends AdvancedConfigPath<List<String>, T> {
    protected StringListToObjectPath(@NotNull String path, @Nullable T def) {
        super(path, def);
    }

    @Override
    public @Nullable List<String> getFromConfig(@NotNull Config config) {
        Object o = config.get(getPath());
        if (o == null) {
            return null;
        } else {
            return o.createStringListFromObject(true);
        }
    }
}
